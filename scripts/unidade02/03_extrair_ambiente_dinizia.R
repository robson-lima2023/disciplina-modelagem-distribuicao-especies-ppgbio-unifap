source("scripts/_bootstrap.R")

# ============================================================
# Unidade 2 - Dados de ocorrência
# Script 03: Extração climática e duplicatas por célula raster
# ============================================================

pacotes <- c("dplyr", "readr", "sf", "terra", "geodata", "tibble")

require_packages(pacotes)
invisible(lapply(pacotes, library, character.only = TRUE))

arquivo_oc <- "dados/unidade02/processados/02_ocorrencias_limpas_amazonia.csv"

possiveis_shp <- c(
  "dados/unidade01/brutos/amazon_biome_border.shp",
  "dados/unidade01/brutos/amazon_biome_border(1).shp",
  "dados/unidade02/brutos/amazon_biome_border.shp",
  "dados/unidade02/brutos/amazon_biome_border(1).shp"
)

arquivo_bioma <- possiveis_shp[file.exists(possiveis_shp)][1]

if (!file.exists(arquivo_oc)) {
  stop("Execute primeiro o script 02.")
}

oc <- read_csv(arquivo_oc, show_col_types = FALSE)

sf::sf_use_s2(FALSE)

amazonia <- sf::st_read(arquivo_bioma, quiet = TRUE) %>%
  sf::st_transform(4326) %>%
  sf::st_make_valid() %>%
  sf::st_buffer(0) %>%
  sf::st_collection_extract("POLYGON") %>%
  sf::st_make_valid()

amazonia_vect <- terra::makeValid(terra::vect(amazonia))

sf::sf_use_s2(TRUE)

# ------------------------------------------------------------
# 1. Baixar WorldClim
# ------------------------------------------------------------
# O artigo de referência usou 19 variáveis bioclimáticas e selecionou
# um subconjunto após controle de multicolinearidade. Aqui usamos
# as nove variáveis destacadas no estudo: BIO3, BIO4, BIO5, BIO8,
# BIO12, BIO13, BIO14, BIO18 e BIO19.

wc_dir <- "dados/unidade02/brutos/worldclim"
dir.create(wc_dir, recursive = TRUE, showWarnings = FALSE)

bio <- geodata::worldclim_global(
  var = "bio",
  res = 10,
  path = wc_dir
)

names(bio) <- paste0("bio", 1:19)

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

# Corrigir variáveis de temperatura se necessário
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
  "dados/unidade02/processados/worldclim_variaveis_selecionadas_amazonia_unidade02.tif",
  overwrite = TRUE
)

# ------------------------------------------------------------
# 2. Extrair valores ambientais
# ------------------------------------------------------------

oc_sf <- sf::st_as_sf(
  oc,
  coords = c("lon", "lat"),
  crs = 4326,
  remove = FALSE
)

valores <- terra::extract(
  bio_amazonia,
  terra::vect(oc_sf)
)

oc_env <- bind_cols(
  oc,
  valores %>% dplyr::select(-ID)
) %>%
  filter(if_all(starts_with("bio"), ~ !is.na(.x)))

# ------------------------------------------------------------
# 3. Remover duplicatas por célula raster
# ------------------------------------------------------------

xy_oc <- oc_env %>%
  dplyr::select(lon, lat) %>%
  as.data.frame()

xy_oc$lon <- as.numeric(xy_oc$lon)
xy_oc$lat <- as.numeric(xy_oc$lat)

celulas <- terra::cellFromXY(
  bio_amazonia[[1]],
  as.matrix(xy_oc)
)

oc_env <- oc_env %>%
  mutate(celula_raster = celulas) %>%
  arrange(fonte) %>%
  distinct(celula_raster, .keep_all = TRUE)

readr::write_csv(
  oc_env,
  "dados/unidade02/processados/03_ocorrencias_ambiente_sem_duplicata_raster.csv"
)

resumo <- tibble(
  etapa = c(
    "Registros limpos no bioma",
    "Registros com valores ambientais",
    "Registros após remoção de duplicatas por célula raster"
  ),
  n = c(
    nrow(oc),
    nrow(bind_cols(oc, valores %>% dplyr::select(-ID)) %>% filter(if_all(starts_with("bio"), ~ !is.na(.x)))),
    nrow(oc_env)
  )
)

readr::write_csv(
  resumo,
  "tabelas/unidade02/resumo_extracao_ambiental_dinizia.csv"
)

message("Script 03 concluído: ambiente extraído e duplicatas raster removidas.")
print(resumo)
