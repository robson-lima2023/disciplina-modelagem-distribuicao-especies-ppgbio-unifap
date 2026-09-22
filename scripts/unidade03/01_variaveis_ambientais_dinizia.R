source("scripts/_bootstrap.R")

# ============================================================
# Unidade 3 - Variáveis ambientais para SDM
# Script 01: Variáveis ambientais reais para Dinizia excelsa
# ============================================================
pacotes <- c(
  "dplyr", "readr", "ggplot2", "sf", "terra", "geodata",
  "tidyr", "patchwork", "viridis", "tibble"
)

require_packages(pacotes)
invisible(lapply(pacotes, library, character.only = TRUE))

# ------------------------------------------------------------
# 1. Estrutura de pastas
# ------------------------------------------------------------

pastas <- c(
  "dados/unidade03/brutos",
  "dados/unidade03/processados",
  "figuras/unidade03",
  "tabelas/unidade03",
  "resultados/unidade03"
)

for (p in pastas) dir.create(p, recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# 2. Entradas
# ------------------------------------------------------------

arquivo_oc <- "dados/unidade02/processados/04_ocorrencias_rarefeitas_dinizia.csv"

if (!file.exists(arquivo_oc)) {
  arquivo_oc <- "dados/unidade02/processados/03_ocorrencias_ambiente_sem_duplicata_raster.csv"
}

if (!file.exists(arquivo_oc)) {
  stop("Execute antes a Unidade 2 para gerar as ocorrências limpas de Dinizia excelsa.")
}

possiveis_shp <- c(
  "dados/unidade01/brutos/amazon_biome_border.shp",
  "dados/unidade01/brutos/amazon_biome_border(1).shp",
  "dados/unidade02/brutos/amazon_biome_border.shp",
  "dados/unidade02/brutos/amazon_biome_border(1).shp",
  "dados/unidade03/brutos/amazon_biome_border.shp",
  "dados/unidade03/brutos/amazon_biome_border(1).shp"
)

arquivo_bioma <- possiveis_shp[file.exists(possiveis_shp)][1]

if (is.na(arquivo_bioma) || !file.exists(arquivo_bioma)) {
  stop("Shapefile do bioma Amazônia não encontrado.")
}

oc <- readr::read_csv(arquivo_oc, show_col_types = FALSE)

# ------------------------------------------------------------
# 3. Ler e corrigir o bioma Amazônia
# ------------------------------------------------------------

sf::sf_use_s2(FALSE)

amazonia <- sf::st_read(arquivo_bioma, quiet = TRUE) %>%
  sf::st_transform(4326) %>%
  sf::st_make_valid() %>%
  sf::st_buffer(0) %>%
  sf::st_collection_extract("POLYGON") %>%
  sf::st_make_valid()

amazonia_vect <- terra::makeValid(terra::vect(amazonia))

sf::sf_use_s2(TRUE)

oc_sf <- sf::st_as_sf(
  oc,
  coords = c("lon", "lat"),
  crs = 4326,
  remove = FALSE
)

# ------------------------------------------------------------
# 4. Baixar e preparar variáveis bioclimáticas WorldClim
# ------------------------------------------------------------

wc_dir <- "dados/unidade03/brutos/worldclim"
dir.create(wc_dir, recursive = TRUE, showWarnings = FALSE)

bio <- geodata::worldclim_global(
  var = "bio",
  res = 10,
  path = wc_dir
)

names(bio) <- paste0("bio", 1:19)

# Variáveis compatíveis com o estudo de caso e úteis para SDM
bio_sel <- bio[[c("bio3", "bio4", "bio5", "bio8", "bio12", "bio13", "bio14", "bio18", "bio19")]]

names(bio_sel) <- c(
  "bio3_isotermalidade",
  "bio4_sazonalidade_temperatura",
  "bio5_temp_max_mes_quente",
  "bio8_temp_media_trimestre_umido",
  "bio12_precipitacao_anual",
  "bio13_precipitacao_mes_umido",
  "bio14_precipitacao_mes_seco",
  "bio18_precipitacao_trimestre_quente",
  "bio19_precipitacao_trimestre_frio"
)

# Corrigir temperatura quando vier multiplicada por 10
variaveis_temp <- c(
  "bio5_temp_max_mes_quente",
  "bio8_temp_media_trimestre_umido"
)

for (v in variaveis_temp) {
  if (terra::global(bio_sel[[v]], "max", na.rm = TRUE)[1, 1] > 100) {
    bio_sel[[v]] <- bio_sel[[v]] / 10
  }
}

bio_amazonia <- bio_sel %>%
  terra::crop(amazonia_vect) %>%
  terra::mask(amazonia_vect)

terra::writeRaster(
  bio_amazonia,
  "dados/unidade03/processados/worldclim_bioclim_selecionadas_amazonia.tif",
  overwrite = TRUE
)

# ------------------------------------------------------------
# 5. Baixar e preparar elevação
# ------------------------------------------------------------

elev_dir <- "dados/unidade03/brutos/elevacao"
dir.create(elev_dir, recursive = TRUE, showWarnings = FALSE)

elev <- geodata::elevation_global(
  res = 10,
  path = elev_dir
)

names(elev) <- "elevacao"

elev_amazonia <- elev %>%
  terra::crop(amazonia_vect) %>%
  terra::mask(amazonia_vect)

# Reamostrar para a grade das variáveis bioclimáticas
elev_amazonia <- terra::resample(
  elev_amazonia,
  bio_amazonia[[1]],
  method = "bilinear"
)

# Derivar declividade e orientação
slope <- terra::terrain(
  elev_amazonia,
  v = "slope",
  unit = "degrees"
)

aspect <- terra::terrain(
  elev_amazonia,
  v = "aspect",
  unit = "degrees"
)

names(slope) <- "declividade"
names(aspect) <- "orientacao"

topografia <- c(elev_amazonia, slope, aspect)

terra::writeRaster(
  topografia,
  "dados/unidade03/processados/topografia_amazonia.tif",
  overwrite = TRUE
)

# ------------------------------------------------------------
# 6. Stack ambiental completo
# ------------------------------------------------------------

amb_stack <- c(
  bio_amazonia,
  topografia
)

terra::writeRaster(
  amb_stack,
  "dados/unidade03/processados/variaveis_ambientais_amazonia_unidade03.tif",
  overwrite = TRUE
)

# ------------------------------------------------------------
# 7. Extrair ambiente nas ocorrências
# ------------------------------------------------------------

#--------------------------------------------------
# Extrair variáveis ambientais
#--------------------------------------------------

valores_oc <- terra::extract(
  amb_stack,
  oc[, c("lon", "lat")]
)

oc_amb <- cbind(
  oc[, c("especie", "lon", "lat")],
  valores_oc[, -1]
)

# remover registros com NA
oc_amb <- oc_amb %>%
  tidyr::drop_na()

readr::write_csv(
  oc_amb,
  "dados/unidade03/processados/ocorrencias_dinizia_variaveis_ambientais.csv"
)

# ------------------------------------------------------------
# 8. Criar background ambiental amazônico
# ------------------------------------------------------------

set.seed(123)

background <- terra::spatSample(
  amb_stack,
  size = 10000,
  method = "random",
  na.rm = TRUE,
  xy = TRUE,
  values = TRUE
) %>%
  as.data.frame() %>%
  dplyr::rename(lon = x, lat = y)

readr::write_csv(
  background,
  "dados/unidade03/processados/background_ambiental_amazonia_unidade03.csv"
)

# ------------------------------------------------------------
# 9. Função de mapa individual com escala própria
# ------------------------------------------------------------

criar_mapa_ambiental <- function(raster_layer, titulo, legenda) {
  
  raster_df <- as.data.frame(
    raster_layer,
    xy = TRUE,
    na.rm = TRUE
  )
  
  names(raster_df) <- c("lon", "lat", "valor")
  
  ggplot() +
    geom_raster(
      data = raster_df,
      aes(x = lon, y = lat, fill = valor)
    ) +
    geom_sf(
      data = amazonia,
      fill = NA,
      color = "grey20",
      linewidth = 0.25
    ) +
    geom_sf(
      data = oc_sf,
      color = "black",
      fill = "red",
      shape = 21,
      size = 0.65,
      stroke = 0.12,
      alpha = 0.85
    ) +
    scale_fill_viridis_c(name = legenda) +
    coord_sf(expand = FALSE) +
    labs(
      title = titulo,
      x = "Longitude",
      y = "Latitude"
    ) +
    theme_bw(base_size = 8.5) +
    theme(
      plot.title = element_text(face = "bold", size = 9),
      panel.grid = element_line(color = "grey90"),
      legend.position = "right",
      legend.title = element_text(size = 7),
      legend.text = element_text(size = 6),
      axis.text = element_text(size = 6),
      axis.title = element_text(size = 7)
    )
}

# ------------------------------------------------------------
# 10. Mapas bioclimáticos principais
# ------------------------------------------------------------

p_bio3 <- criar_mapa_ambiental(
  bio_amazonia[["bio3_isotermalidade"]],
  "BIO3 - Isotermalidade",
  "Índice"
)

p_bio4 <- criar_mapa_ambiental(
  bio_amazonia[["bio4_sazonalidade_temperatura"]],
  "BIO4 - Sazonalidade da temperatura",
  "Índice"
)

p_bio5 <- criar_mapa_ambiental(
  bio_amazonia[["bio5_temp_max_mes_quente"]],
  "BIO5 - Temp. máxima do mês mais quente",
  "°C"
)

p_bio8 <- criar_mapa_ambiental(
  bio_amazonia[["bio8_temp_media_trimestre_umido"]],
  "BIO8 - Temp. média do trimestre úmido",
  "°C"
)

p_bio12 <- criar_mapa_ambiental(
  bio_amazonia[["bio12_precipitacao_anual"]],
  "BIO12 - Precipitação anual",
  "mm"
)

p_bio13 <- criar_mapa_ambiental(
  bio_amazonia[["bio13_precipitacao_mes_umido"]],
  "BIO13 - Precipitação do mês mais úmido",
  "mm"
)

p_bio14 <- criar_mapa_ambiental(
  bio_amazonia[["bio14_precipitacao_mes_seco"]],
  "BIO14 - Precipitação do mês mais seco",
  "mm"
)

p_bio18 <- criar_mapa_ambiental(
  bio_amazonia[["bio18_precipitacao_trimestre_quente"]],
  "BIO18 - Precipitação do trimestre quente",
  "mm"
)

p_bio19 <- criar_mapa_ambiental(
  bio_amazonia[["bio19_precipitacao_trimestre_frio"]],
  "BIO19 - Precipitação do trimestre frio",
  "mm"
)

mapas_bioclim <- (
  p_bio3 | p_bio4 | p_bio5
) / (
  p_bio8 | p_bio12 | p_bio13
) / (
  p_bio14 | p_bio18 | p_bio19
) +
  patchwork::plot_annotation(
    title = expression("Variáveis bioclimáticas selecionadas e ocorrências de " * italic("Dinizia excelsa")),
    subtitle = "Bioma Amazônia | WorldClim atual | Cada painel possui escala própria"
  )

ggsave(
  "figuras/unidade03/unidade03_variaveis_bioclimaticas_dinizia.png",
  plot = mapas_bioclim,
  width = 15,
  height = 12,
  dpi = 600
)

# ------------------------------------------------------------
# 11. Mapas topográficos
# ------------------------------------------------------------

p_elev <- criar_mapa_ambiental(
  topografia[["elevacao"]],
  "Elevação",
  "m"
)

p_slope <- criar_mapa_ambiental(
  topografia[["declividade"]],
  "Declividade",
  "graus"
)

p_aspect <- criar_mapa_ambiental(
  topografia[["orientacao"]],
  "Orientação",
  "graus"
)

mapas_topo <- (p_elev | p_slope | p_aspect) +
  patchwork::plot_annotation(
    title = expression("Variáveis topográficas e ocorrências de " * italic("Dinizia excelsa")),
    subtitle = "Bioma Amazônia | Elevação derivada de modelo digital de elevação"
  )

ggsave(
  "figuras/unidade03/unidade03_variaveis_topograficas_dinizia.png",
  plot = mapas_topo,
  width = 15,
  height = 5,
  dpi = 600
)

# ------------------------------------------------------------
# 12. Histogramas ambientais nos registros da espécie
# ------------------------------------------------------------

vars_plot <- names(amb_stack)

oc_long <- oc_amb %>%
  dplyr::select(dplyr::all_of(vars_plot)) %>%
  tidyr::pivot_longer(
    cols = everything(),
    names_to = "variavel",
    values_to = "valor"
  )

g_hist <- ggplot(
  oc_long,
  aes(x = valor)
) +
  geom_histogram(
    bins = 25,
    fill = "grey35",
    color = "white"
  ) +
  facet_wrap(~ variavel, scales = "free", ncol = 3) +
  labs(
    title = expression("Distribuição ambiental dos registros de " * italic("Dinizia excelsa")),
    x = "Valor ambiental",
    y = "Frequência"
  ) +
  theme_bw(base_size = 10) +
  theme(
    plot.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold", size = 8)
  )

ggsave(
  "figuras/unidade03/unidade03_histogramas_ambientais_dinizia.png",
  plot = g_hist,
  width = 12,
  height = 9,
  dpi = 600
)

# ------------------------------------------------------------
# 13. Resumo das variáveis ambientais
# ------------------------------------------------------------

resumo_variaveis <- oc_amb %>%
  summarise(
    n = n(),
    across(
      all_of(vars_plot),
      list(
        media = ~ mean(.x, na.rm = TRUE),
        sd = ~ sd(.x, na.rm = TRUE),
        min = ~ min(.x, na.rm = TRUE),
        max = ~ max(.x, na.rm = TRUE)
      )
    )
  )

readr::write_csv(
  resumo_variaveis,
  "tabelas/unidade03/resumo_variaveis_ambientais_dinizia.csv"
)

# ------------------------------------------------------------
# 14. Metadados das camadas
# ------------------------------------------------------------

metadados <- tibble::tibble(
  variavel = names(amb_stack),
  fonte = c(
    rep("WorldClim bioclim", 9),
    rep("Modelo digital de elevação", 3)
  ),
  resolucao = terra::res(amb_stack)[1],
  crs = terra::crs(amb_stack),
  n_celulas = terra::ncell(amb_stack)
)

readr::write_csv(
  metadados,
  "tabelas/unidade03/metadados_variaveis_ambientais_unidade03.csv"
)

# ------------------------------------------------------------
# 15. Salvar objetos principais
# ------------------------------------------------------------

saveRDS(
  list(
    ocorrencias_ambiente = oc_amb,
    background = background,
    bioclim = bio_amazonia,
    topografia = topografia,
    stack_ambiental = amb_stack,
    metadados = metadados
  ),
  "resultados/unidade03/objetos_variaveis_ambientais_dinizia.rds"
)

message("Unidade 3 concluída com variáveis ambientais reais para Dinizia excelsa.")
