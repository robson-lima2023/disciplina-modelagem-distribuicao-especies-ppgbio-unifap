source("scripts/_bootstrap.R")

# ============================================================
# Unidade 10 - Maxent em SDM
# Script 02: Preparar dados presença-background
# ============================================================

# ------------------------------------------------------------
# 1. Pacotes
# ------------------------------------------------------------

pacotes <- c(
  "dplyr", "readr", "tidyr", "rsample",
  "tibble", "terra", "sf", "lwgeom"
)

require_packages(pacotes)
invisible(lapply(pacotes, library, character.only = TRUE))

sf::sf_use_s2(FALSE)

# ------------------------------------------------------------
# 2. Diretório raiz do projeto
# ------------------------------------------------------------
# ------------------------------------------------------------
# 3. Diretórios de saída
# ------------------------------------------------------------

dir.create("dados/unidade10/processados", recursive = TRUE, showWarnings = FALSE)
dir.create("tabelas/unidade10", recursive = TRUE, showWarnings = FALSE)
dir.create("resultados/unidade10", recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# 4. Arquivos de entrada
# ------------------------------------------------------------

arquivo_oc <- "dados/unidade05/processados/presencas_area_m_dinizia.csv"

arquivo_vars <- "dados/unidade04/processados/variaveis_selecionadas_final.csv"

arquivo_bioma <- "dados/unidade01/brutos/amazon_biome_border.shp"

arquivo_amb_bioma <- "dados/unidade04/processados/variaveis_ambientais_selecionadas_bioma.tif"

# ------------------------------------------------------------
# 5. Checagem dos arquivos
# ------------------------------------------------------------

if (!file.exists(arquivo_oc)) {
  stop("Arquivo de presenças não encontrado: ", arquivo_oc)
}

if (!file.exists(arquivo_vars)) {
  stop("Arquivo de variáveis selecionadas pelo VIF não encontrado: ", arquivo_vars)
}

if (!file.exists(arquivo_bioma)) {
  stop("Limite do bioma Amazônia não encontrado: ", arquivo_bioma)
}

if (!file.exists(arquivo_amb_bioma)) {
  stop(
    "Raster ambiental selecionado do bioma não encontrado: ",
    arquivo_amb_bioma,
    "\nExecute antes o script da Unidade 4: 07_gerar_raster_ambiental.R"
  )
}

# ------------------------------------------------------------
# 6. Leitura dos dados
# ------------------------------------------------------------

oc <- read_csv(
  arquivo_oc,
  show_col_types = FALSE
)

vars_tab <- read_csv(
  arquivo_vars,
  show_col_types = FALSE
)

bioma_raw <- sf::st_read(
  arquivo_bioma,
  quiet = TRUE
)

amb_bioma <- terra::rast(
  arquivo_amb_bioma
)

# ------------------------------------------------------------
# 7. Verificar variáveis selecionadas pelo VIF
# ------------------------------------------------------------

if (!"variavel" %in% names(vars_tab)) {
  stop("O arquivo variaveis_selecionadas_final.csv precisa conter a coluna 'variavel'.")
}

vars <- vars_tab$variavel |> unique()

vars_ausentes_raster <- setdiff(vars, names(amb_bioma))

if (length(vars_ausentes_raster) > 0) {
  stop(
    "As seguintes variáveis selecionadas pelo VIF não estão no raster ambiental: ",
    paste(vars_ausentes_raster, collapse = ", ")
  )
}

amb_bioma <- amb_bioma[[vars]]

if (terra::nlyr(amb_bioma) != length(vars)) {
  stop("O número de camadas do raster não coincide com o número de variáveis selecionadas.")
}

# ------------------------------------------------------------
# 8. Corrigir e padronizar limite do bioma
# ------------------------------------------------------------

bioma_proj <- bioma_raw |>
  sf::st_transform(5880) |>
  sf::st_make_valid() |>
  sf::st_union() |>
  sf::st_as_sf() |>
  sf::st_make_valid()

bioma <- bioma_proj |>
  sf::st_transform(terra::crs(amb_bioma)) |>
  sf::st_make_valid()

# ------------------------------------------------------------
# 9. Converter presenças para sf
# ------------------------------------------------------------

if (!all(c("lon", "lat") %in% names(oc))) {
  stop("O arquivo de presenças precisa conter as colunas 'lon' e 'lat'.")
}

oc <- oc |>
  filter(!is.na(lon), !is.na(lat))

oc_sf <- sf::st_as_sf(
  oc,
  coords = c("lon", "lat"),
  crs = 4326,
  remove = FALSE
)

oc_sf <- sf::st_transform(
  oc_sf,
  terra::crs(amb_bioma)
)

# ------------------------------------------------------------
# 10. Manter apenas presenças dentro do bioma
# ------------------------------------------------------------

intersecao <- sf::st_intersects(
  oc_sf,
  bioma,
  sparse = FALSE
)

oc_sf <- oc_sf[rowSums(intersecao) > 0, ]

if (nrow(oc_sf) == 0) {
  stop("Nenhuma presença permaneceu dentro do limite do bioma Amazônia.")
}

# ------------------------------------------------------------
# 11. Máscara ambiental pelo bioma
# ------------------------------------------------------------

bioma_vect <- terra::vect(bioma)

amb_bioma_mask <- amb_bioma |>
  terra::crop(bioma_vect) |>
  terra::mask(bioma_vect)

# ------------------------------------------------------------
# 12. Gerar background no bioma Amazônia
# ------------------------------------------------------------

set.seed(123)

n_pres <- nrow(oc_sf)

n_bg <- min(
  10000,
  max(1000, n_pres * 10)
)

bg_spat <- terra::spatSample(
  amb_bioma_mask[[1]],
  size = n_bg,
  method = "random",
  na.rm = TRUE,
  as.points = TRUE,
  values = FALSE
)

bg_df <- as.data.frame(
  terra::crds(bg_spat)
) |>
  rename(
    lon = x,
    lat = y
  )

bg_sf <- sf::st_as_sf(
  bg_df,
  coords = c("lon", "lat"),
  crs = terra::crs(amb_bioma_mask),
  remove = FALSE
)

# ------------------------------------------------------------
# 13. Extrair variáveis ambientais
# ------------------------------------------------------------

pres_vals <- terra::extract(
  amb_bioma_mask,
  terra::vect(oc_sf)
) |>
  select(-ID)

bg_vals <- terra::extract(
  amb_bioma_mask,
  terra::vect(bg_sf)
) |>
  select(-ID)

pres_df <- oc_sf |>
  sf::st_drop_geometry() |>
  select(lon, lat) |>
  bind_cols(pres_vals) |>
  mutate(
    pa = 1,
    tipo = "presenca"
  )

bg_df <- bg_sf |>
  sf::st_drop_geometry() |>
  select(lon, lat) |>
  bind_cols(bg_vals) |>
  mutate(
    pa = 0,
    tipo = "background"
  )

dados_modelo <- bind_rows(
  pres_df,
  bg_df
) |>
  drop_na(all_of(vars)) |>
  mutate(
    pa = as.numeric(pa)
  )

if (sum(dados_modelo$pa == 1) == 0) {
  stop("Nenhuma presença permaneceu após a extração ambiental.")
}

if (sum(dados_modelo$pa == 0) == 0) {
  stop("Nenhum ponto de background permaneceu após a extração ambiental.")
}

# ------------------------------------------------------------
# 14. Divisão treino-teste
# ------------------------------------------------------------

set.seed(123)

split <- rsample::initial_split(
  dados_modelo,
  prop = 0.70,
  strata = pa
)

treino <- rsample::training(split)
teste <- rsample::testing(split)

# ------------------------------------------------------------
# 15. Salvar dados processados
# ------------------------------------------------------------

write_csv(
  treino,
  "dados/unidade10/processados/treino_maxent_dinizia.csv"
)

write_csv(
  teste,
  "dados/unidade10/processados/teste_maxent_dinizia.csv"
)

write_csv(
  dados_modelo,
  "dados/unidade10/processados/dados_maxent_dinizia.csv"
)

write_csv(
  dados_modelo,
  "dados/unidade10/processados/dados_presenca_background_bioma_maxent_dinizia.csv"
)

variaveis_maxent <- tibble(
  ordem = seq_along(vars),
  variavel = vars,
  usada_no_modelo = TRUE
)

write_csv(
  variaveis_maxent,
  "resultados/unidade10/variaveis_maxent.csv"
)

write_csv(
  variaveis_maxent,
  "tabelas/unidade10/variaveis_usadas_maxent.csv"
)

# ------------------------------------------------------------
# 16. Exportar arquivos específicos para MaxEnt
# ------------------------------------------------------------

presencas_maxent <- treino |>
  filter(pa == 1) |>
  select(lon, lat, all_of(vars))

background_maxent <- treino |>
  filter(pa == 0) |>
  select(lon, lat, all_of(vars))

write_csv(
  presencas_maxent,
  "dados/unidade10/processados/presencas_treino_maxent_dinizia.csv"
)

write_csv(
  background_maxent,
  "dados/unidade10/processados/background_treino_maxent_dinizia.csv"
)

# ------------------------------------------------------------
# 17. Resumo
# ------------------------------------------------------------

resumo <- tibble(
  conjunto = c("Total", "Treino", "Teste"),
  n = c(nrow(dados_modelo), nrow(treino), nrow(teste)),
  presencas = c(
    sum(dados_modelo$pa == 1),
    sum(treino$pa == 1),
    sum(teste$pa == 1)
  ),
  background = c(
    sum(dados_modelo$pa == 0),
    sum(treino$pa == 0),
    sum(teste$pa == 0)
  ),
  n_variaveis = length(vars)
)

write_csv(
  resumo,
  "tabelas/unidade10/resumo_dados_maxent.csv"
)

# ------------------------------------------------------------
# 18. Mensagem final
# ------------------------------------------------------------

message("Dados presença-background preparados para MaxEnt no bioma Amazônia.")
message("Número de presenças dentro do bioma: ", n_pres)
message("Número de pontos de background gerados: ", n_bg)
message("Número de variáveis ambientais utilizadas: ", length(vars))
message("Variáveis: ", paste(vars, collapse = ", "))

print(resumo)
