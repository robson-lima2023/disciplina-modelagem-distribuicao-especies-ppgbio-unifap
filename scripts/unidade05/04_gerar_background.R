# ============================================================
# Unidade 5 - Área acessível (M), BAM e Background
# Script 04: Geração de pontos de background dentro de M
# ============================================================

pacotes <- c("sf", "dplyr", "ggplot2", "readr", "maps")

instalar <- pacotes[!pacotes %in% rownames(installed.packages())]
if (length(instalar) > 0) install.packages(instalar)

invisible(lapply(pacotes, library, character.only = TRUE))

dir.create("figuras/unidade05", recursive = TRUE, showWarnings = FALSE)
dir.create("dados/unidade05/processados", recursive = TRUE, showWarnings = FALSE)

oc_sf <- st_read(
  "dados/unidade05/processados/ocorrencias_unidade05.gpkg",
  quiet = TRUE
)

area_m <- st_read(
  "dados/unidade05/processados/area_m_500km.gpkg",
  quiet = TRUE
)

set.seed(123)

# ------------------------------------------------------------
# Sorteio de pontos de background
# ------------------------------------------------------------
# Esses pontos representam condições ambientais disponíveis
# dentro da área acessível M. Eles não são ausências.

background <- st_sample(
  area_m,
  size = 5000,
  type = "random"
)

background_sf <- st_as_sf(
  data.frame(id = seq_along(background)),
  geometry = background
)

background_coords <- st_coordinates(background_sf)

background_df <- data.frame(
  id = background_sf$id,
  lon = background_coords[, 1],
  lat = background_coords[, 2]
)

write_csv(
  background_df,
  "dados/unidade05/processados/background_m_500km.csv"
)

st_write(
  background_sf,
  "dados/unidade05/processados/background_m_500km.gpkg",
  delete_dsn = TRUE,
  quiet = TRUE
)

# ------------------------------------------------------------
# Mapa
# ------------------------------------------------------------

mundo <- map_data("world")

grafico <- ggplot() +
  geom_polygon(
    data = mundo,
    aes(x = long, y = lat, group = group),
    fill = "grey95",
    color = "grey70",
    linewidth = 0.2
  ) +
  geom_sf(
    data = area_m,
    fill = NA,
    color = "black",
    linewidth = 0.8
  ) +
  geom_sf(
    data = background_sf,
    color = "grey50",
    size = 0.25,
    alpha = 0.45
  ) +
  geom_sf(
    data = oc_sf,
    color = "black",
    size = 1.4
  ) +
  coord_sf(
    xlim = c(-95, -25),
    ylim = c(-45, 20)
  ) +
  labs(
    title = "Background sorteado dentro da área acessível M",
    subtitle = "Exemplo com M delimitado por buffer de 500 km",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold")
  )

print(grafico)

ggsave(
  "figuras/unidade05/unidade05_background_m.png",
  plot = grafico,
  width = 9,
  height = 7,
  dpi = 600
)

message("Background gerado dentro da área M.")
