source("scripts/_bootstrap.R")

# ============================================================
# Unidade 2 - Dados de ocorrência
# Script 02: Limpeza espacial e recorte pelo bioma Amazônia
# ============================================================

pacotes <- c(
  "dplyr", "readr", "ggplot2", "sf", "terra",
  "CoordinateCleaner", "patchwork", "ggspatial"
)

require_packages(pacotes)
invisible(lapply(pacotes, library, character.only = TRUE))

arquivo_oc <- "dados/unidade02/processados/01_ocorrencias_compiladas_dinizia.csv"

possiveis_shp <- c(
  "dados/unidade01/brutos/amazon_biome_border.shp",
  "dados/unidade01/brutos/amazon_biome_border(1).shp",
  "dados/unidade02/brutos/amazon_biome_border.shp",
  "dados/unidade02/brutos/amazon_biome_border(1).shp"
)

arquivo_bioma <- possiveis_shp[file.exists(possiveis_shp)][1]

if (!file.exists(arquivo_oc)) {
  stop("Execute primeiro scripts/unidade02/01_compilar_ocorrencias_dinizia.R")
}

if (is.na(arquivo_bioma) || !file.exists(arquivo_bioma)) {
  stop("Shapefile do bioma Amazônia não encontrado.")
}

oc <- read_csv(arquivo_oc, show_col_types = FALSE)

# ------------------------------------------------------------
# 1. Filtros básicos de coordenadas
# ------------------------------------------------------------

oc_validas <- oc %>%
  filter(
    !is.na(lon),
    !is.na(lat),
    lon >= -180,
    lon <= 180,
    lat >= -90,
    lat <= 90,
    !(lon == 0 & lat == 0)
  ) %>%
  distinct(lon, lat, .keep_all = TRUE)

# ------------------------------------------------------------
# 2. CoordinateCleaner
# ------------------------------------------------------------

oc_cc <- CoordinateCleaner::clean_coordinates(
  x = oc_validas,
  lon = "lon",
  lat = "lat",
  species = "especie_padrao",
  tests = c("capitals", "centroids", "equal", "gbif", "institutions", "seas", "zeros"),
  value = "spatialvalid",
  verbose = TRUE
)

oc_limpas <- oc_validas %>%
  mutate(coordenada_valida = oc_cc$.summary) %>%
  filter(coordenada_valida)

# ------------------------------------------------------------
# 3. Ler bioma e corrigir geometrias
# ------------------------------------------------------------

sf::sf_use_s2(FALSE)

amazonia <- sf::st_read(arquivo_bioma, quiet = TRUE) %>%
  sf::st_transform(4326) %>%
  sf::st_make_valid() %>%
  sf::st_buffer(0) %>%
  sf::st_collection_extract("POLYGON") %>%
  sf::st_make_valid()

amazonia_vect <- terra::makeValid(terra::vect(amazonia))

# ------------------------------------------------------------
# 4. Filtrar ocorrências dentro da Amazônia
# ------------------------------------------------------------

oc_sf <- sf::st_as_sf(
  oc_limpas,
  coords = c("lon", "lat"),
  crs = 4326,
  remove = FALSE
)

oc_vect <- terra::vect(oc_sf)

oc_amazonia_vect <- terra::intersect(
  oc_vect,
  amazonia_vect
)

oc_amazonia <- sf::st_as_sf(oc_amazonia_vect)

oc_amazonia_df <- oc_amazonia %>%
  st_drop_geometry() %>%
  dplyr::select(especie_padrao, especie, lon, lat, fonte, origem, id_origem) %>%
  distinct(lon, lat, .keep_all = TRUE)

oc_amazonia <- sf::st_as_sf(
  oc_amazonia_df,
  coords = c("lon", "lat"),
  crs = 4326,
  remove = FALSE
)

sf::sf_use_s2(TRUE)

readr::write_csv(
  oc_limpas,
  "dados/unidade02/processados/02_ocorrencias_limpas_global.csv"
)

readr::write_csv(
  oc_amazonia_df,
  "dados/unidade02/processados/02_ocorrencias_limpas_amazonia.csv"
)

sf::st_write(
  oc_amazonia,
  "dados/unidade02/processados/dinizia_ocorrencias_limpas_amazonia.gpkg",
  delete_dsn = TRUE,
  quiet = TRUE
)

# ------------------------------------------------------------
# 5. Mapa bruto versus limpo
# ------------------------------------------------------------

oc_brutas_sf <- sf::st_as_sf(
  oc,
  coords = c("lon", "lat"),
  crs = 4326,
  remove = FALSE
)

p1 <- ggplot() +
  geom_sf(data = amazonia, fill = "grey95", color = "grey35", linewidth = 0.25) +
  geom_sf(data = oc_brutas_sf, color = "black", fill = "orange", shape = 21, size = 1.3, alpha = 0.75) +
  coord_sf(expand = FALSE) +
  labs(title = "A) Registros brutos", x = "Longitude", y = "Latitude") +
  theme_bw(base_size = 10) +
  theme(plot.title = element_text(face = "bold"))

p2 <- ggplot() +
  geom_sf(data = amazonia, fill = "grey95", color = "grey35", linewidth = 0.25) +
  geom_sf(data = oc_amazonia, color = "black", fill = "red", shape = 21, size = 1.3, alpha = 0.80) +
  coord_sf(expand = FALSE) +
  labs(title = "B) Registros limpos no bioma Amazônia", x = "Longitude", y = "Latitude") +
  theme_bw(base_size = 10) +
  theme(plot.title = element_text(face = "bold"))

fig <- p1 | p2

ggsave(
  "figuras/unidade02/unidade02_mapa_bruto_limpo_dinizia.png",
  plot = fig,
  width = 12,
  height = 6,
  dpi = 600
)

resumo_limpeza <- tibble::tibble(
  etapa = c(
    "Registros compilados",
    "Coordenadas únicas válidas",
    "Registros aprovados no CoordinateCleaner",
    "Registros dentro do bioma Amazônia"
  ),
  n = c(
    nrow(oc),
    nrow(oc_validas),
    nrow(oc_limpas),
    nrow(oc_amazonia_df)
  )
)

readr::write_csv(
  resumo_limpeza,
  "tabelas/unidade02/resumo_limpeza_espacial_dinizia.csv"
)

message("Script 02 concluído: limpeza espacial finalizada.")
print(resumo_limpeza)
