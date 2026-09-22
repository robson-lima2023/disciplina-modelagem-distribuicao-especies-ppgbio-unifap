source("scripts/_bootstrap.R")

# ============================================================
# Unidade 5 - Área acessível M e background
# Script 03: Gerar background dentro da área M
# ============================================================

pacotes <- c(
  "dplyr", "readr", "sf", "terra", "ggplot2",
  "ggspatial", "tibble"
)

require_packages(pacotes)
invisible(lapply(pacotes, library, character.only = TRUE))

arquivo_m <- "dados/unidade05/processados/area_m_dinizia.gpkg"
arquivo_stack <- "dados/unidade03/processados/variaveis_ambientais_amazonia_unidade03.tif"
arquivo_oc <- "dados/unidade02/processados/04_ocorrencias_rarefeitas_dinizia.csv"

if (!file.exists(arquivo_m)) stop("Área M não encontrada. Execute o script 02.")
if (!file.exists(arquivo_stack)) stop("Stack ambiental da Unidade 3 não encontrado.")
if (!file.exists(arquivo_oc)) {
  arquivo_oc <- "dados/unidade02/processados/03_ocorrencias_ambiente_sem_duplicata_raster.csv"
}

area_m <- sf::st_read(arquivo_m, quiet = TRUE)
amb_stack <- terra::rast(arquivo_stack)
oc <- readr::read_csv(arquivo_oc, show_col_types = FALSE)

oc_sf <- sf::st_as_sf(
  oc,
  coords = c("lon", "lat"),
  crs = 4326,
  remove = FALSE
)

# ------------------------------------------------------------
# Selecionar variáveis finais da Unidade 4, se disponíveis
# ------------------------------------------------------------

arquivo_vars <- "resultados/unidade04/variaveis_selecionadas_final.csv"

if (file.exists(arquivo_vars)) {
  vars_final <- readr::read_csv(arquivo_vars, show_col_types = FALSE)$variavel
  vars_final <- vars_final[vars_final %in% names(amb_stack)]
  if (length(vars_final) >= 2) {
    amb_stack <- amb_stack[[vars_final]]
  }
}

# ------------------------------------------------------------
# Recortar stack ambiental para a área M
# ------------------------------------------------------------

area_m_vect <- terra::makeValid(terra::vect(area_m))

amb_m <- amb_stack %>%
  terra::crop(area_m_vect) %>%
  terra::mask(area_m_vect)

terra::writeRaster(
  amb_m,
  "dados/unidade05/processados/variaveis_ambientais_area_m.tif",
  overwrite = TRUE
)

# ------------------------------------------------------------
# Gerar background
# ------------------------------------------------------------

set.seed(123)

n_background <- 10000

bg <- terra::spatSample(
  amb_m,
  size = n_background,
  method = "random",
  na.rm = TRUE,
  xy = TRUE,
  values = TRUE
) %>%
  as.data.frame() %>%
  dplyr::rename(lon = x, lat = y) %>%
  dplyr::mutate(
    pa = 0,
    tipo = "Background_M"
  )

readr::write_csv(
  bg,
  "dados/unidade05/processados/background_area_m_dinizia.csv"
)

bg_sf <- sf::st_as_sf(
  bg,
  coords = c("lon", "lat"),
  crs = 4326,
  remove = FALSE
)

sf::st_write(
  bg_sf,
  "dados/unidade05/processados/background_area_m_dinizia.gpkg",
  delete_dsn = TRUE,
  quiet = TRUE
)

# ------------------------------------------------------------
# Mapa do background
# ------------------------------------------------------------

g <- ggplot() +
  geom_sf(
    data = area_m,
    fill = "grey95",
    color = "grey30",
    linewidth = 0.35
  ) +
  geom_sf(
    data = bg_sf,
    color = "grey40",
    size = 0.15,
    alpha = 0.35
  ) +
  geom_sf(
    data = oc_sf,
    color = "black",
    fill = "red",
    shape = 21,
    size = 1.3,
    stroke = 0.20,
    alpha = 0.90
  ) +
  coord_sf(expand = FALSE) +
  ggspatial::annotation_scale(location = "bl", width_hint = 0.30) +
  ggspatial::annotation_north_arrow(
    location = "tr",
    which_north = "true",
    style = ggspatial::north_arrow_fancy_orienteering
  ) +
  labs(
    title = expression("Background dentro da área M de " * italic("Dinizia excelsa")),
    subtitle = paste0(n_background, " pontos de background aleatórios"),
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_bw(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid = element_line(color = "grey90")
  )

ggsave(
  "figuras/unidade05/unidade05_background_area_m.png",
  plot = g,
  width = 8.5,
  height = 7,
  dpi = 600
)

resumo_bg <- tibble::tibble(
  produto = c("Número de pontos de background", "Número de variáveis ambientais"),
  valor = c(nrow(bg), length(names(amb_m)))
)

readr::write_csv(
  resumo_bg,
  "tabelas/unidade05/resumo_background_area_m.csv"
)

message("Background gerado dentro da área M.")
print(resumo_bg)
