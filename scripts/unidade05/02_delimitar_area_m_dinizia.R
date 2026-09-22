source("scripts/_bootstrap.R")

# ============================================================
# Unidade 5 - Área acessível M e background
# Script 02: Delimitar área acessível M para Dinizia excelsa
# ============================================================

pacotes <- c(
  "dplyr", "readr", "sf", "terra", "ggplot2",
  "ggspatial", "tibble"
)

require_packages(pacotes)
invisible(lapply(pacotes, library, character.only = TRUE))

# ------------------------------------------------------------
# Entradas
# ------------------------------------------------------------

arquivo_oc <- "dados/unidade02/processados/04_ocorrencias_rarefeitas_dinizia.csv"

if (!file.exists(arquivo_oc)) {
  arquivo_oc <- "dados/unidade02/processados/03_ocorrencias_ambiente_sem_duplicata_raster.csv"
}

if (!file.exists(arquivo_oc)) {
  stop("Ocorrências finais não encontradas. Execute as Unidades 2 e 3 antes da Unidade 5.")
}

possiveis_shp <- c(
  "dados/unidade01/brutos/amazon_biome_border.shp",
  "dados/unidade01/brutos/amazon_biome_border(1).shp",
  "dados/unidade02/brutos/amazon_biome_border.shp",
  "dados/unidade02/brutos/amazon_biome_border(1).shp"
)

arquivo_bioma <- possiveis_shp[file.exists(possiveis_shp)][1]

if (is.na(arquivo_bioma) || !file.exists(arquivo_bioma)) {
  stop("Shapefile do bioma Amazônia não encontrado.")
}

oc <- readr::read_csv(arquivo_oc, show_col_types = FALSE)

# ------------------------------------------------------------
# Ler e corrigir o bioma Amazônia
# ------------------------------------------------------------

sf::sf_use_s2(FALSE)

amazonia <- sf::st_read(arquivo_bioma, quiet = TRUE) %>%
  sf::st_transform(4326) %>%
  sf::st_make_valid() %>%
  sf::st_buffer(0) %>%
  sf::st_collection_extract("POLYGON") %>%
  sf::st_make_valid()

# ------------------------------------------------------------
# Converter ocorrências
# ------------------------------------------------------------

oc_sf <- sf::st_as_sf(
  oc,
  coords = c("lon", "lat"),
  crs = 4326,
  remove = FALSE
)

# ------------------------------------------------------------
# Definir área M por buffer
# ------------------------------------------------------------
# Para construir uma área M operacional e reprodutível, usamos
# buffer de 300 km ao redor das ocorrências, recortado pelo bioma.
# Esse valor pode ser ajustado conforme conhecimento biogeográfico,
# dispersão, história natural e objetivo do estudo.
# ------------------------------------------------------------

buffer_km <- 300

crs_metrico <- 6933

oc_metrico <- oc_sf %>%
  sf::st_transform(crs_metrico)

amazonia_metrico <- amazonia %>%
  sf::st_transform(crs_metrico)

area_m_buffer <- oc_metrico %>%
  sf::st_union() %>%
  sf::st_buffer(dist = buffer_km * 1000) %>%
  sf::st_make_valid()

area_m <- sf::st_intersection(
  sf::st_as_sf(data.frame(id = 1, geometry = area_m_buffer)),
  amazonia_metrico
) %>%
  sf::st_make_valid() %>%
  sf::st_transform(4326)

# Dissolver eventuais fragmentos
area_m <- area_m %>%
  dplyr::summarise(geometry = sf::st_union(geometry)) %>%
  sf::st_make_valid()

sf::sf_use_s2(TRUE)

# ------------------------------------------------------------
# Salvar área M
# ------------------------------------------------------------

sf::st_write(
  area_m,
  "dados/unidade05/processados/area_m_dinizia.gpkg",
  delete_dsn = TRUE,
  quiet = TRUE
)

# ------------------------------------------------------------
# Tabela de metadados
# ------------------------------------------------------------

area_m_m2 <- area_m %>%
  sf::st_transform(crs_metrico) %>%
  sf::st_area() %>%
  as.numeric()

metadados_m <- tibble::tibble(
  especie = "Dinizia excelsa",
  metodo = "Buffer ao redor das ocorrências recortado pelo bioma Amazônia",
  buffer_km = buffer_km,
  area_m_km2 = area_m_m2 / 1e6,
  n_ocorrencias = nrow(oc)
)

readr::write_csv(
  metadados_m,
  "tabelas/unidade05/metadados_area_m_dinizia.csv"
)

# ------------------------------------------------------------
# Mapa da área M
# ------------------------------------------------------------

g <- ggplot() +
  geom_sf(
    data = amazonia,
    fill = "grey95",
    color = "grey40",
    linewidth = 0.25
  ) +
  geom_sf(
    data = area_m,
    fill = "steelblue",
    color = "blue4",
    alpha = 0.35,
    linewidth = 0.40
  ) +
  geom_sf(
    data = oc_sf,
    color = "black",
    fill = "red",
    shape = 21,
    size = 1.4,
    stroke = 0.20,
    alpha = 0.85
  ) +
  coord_sf(expand = FALSE) +
  ggspatial::annotation_scale(location = "bl", width_hint = 0.30) +
  ggspatial::annotation_north_arrow(
    location = "tr",
    which_north = "true",
    style = ggspatial::north_arrow_fancy_orienteering
  ) +
  labs(
    title = expression("Área acessível (M) para " * italic("Dinizia excelsa")),
    subtitle = paste0("Buffer de ", buffer_km, " km ao redor das ocorrências, recortado pelo bioma Amazônia"),
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_bw(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid = element_line(color = "grey90")
  )

ggsave(
  "figuras/unidade05/unidade05_area_m_dinizia.png",
  plot = g,
  width = 8.5,
  height = 7,
  dpi = 600
)

message("Área M delimitada com sucesso.")
print(metadados_m)
