# ============================================================
# Unidade 5 - Área acessível (M), BAM e Background
# Script 03: Delimitação da área acessível M
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

# ------------------------------------------------------------
# Transformação para projeção métrica global
# ------------------------------------------------------------
# EPSG:6933 é uma projeção global equal-area útil para operações
# de área e buffers em escala ampla. Para estudos locais, recomenda-se
# usar uma projeção regional adequada.

oc_proj <- st_transform(oc_sf, 6933)

# ------------------------------------------------------------
# Buffers alternativos ao redor dos pontos
# ------------------------------------------------------------

m_250 <- st_union(st_buffer(oc_proj, dist = 250000))
m_500 <- st_union(st_buffer(oc_proj, dist = 500000))
m_1000 <- st_union(st_buffer(oc_proj, dist = 1000000))

m_250 <- st_as_sf(data.frame(nome = "M_250km", geometry = st_geometry(m_250)))
m_500 <- st_as_sf(data.frame(nome = "M_500km", geometry = st_geometry(m_500)))
m_1000 <- st_as_sf(data.frame(nome = "M_1000km", geometry = st_geometry(m_1000)))

m_250_wgs <- st_transform(m_250, 4326)
m_500_wgs <- st_transform(m_500, 4326)
m_1000_wgs <- st_transform(m_1000, 4326)
oc_wgs <- st_transform(oc_sf, 4326)

st_write(m_250_wgs, "dados/unidade05/processados/area_m_250km.gpkg",
         delete_dsn = TRUE, quiet = TRUE)
st_write(m_500_wgs, "dados/unidade05/processados/area_m_500km.gpkg",
         delete_dsn = TRUE, quiet = TRUE)
st_write(m_1000_wgs, "dados/unidade05/processados/area_m_1000km.gpkg",
         delete_dsn = TRUE, quiet = TRUE)

# ------------------------------------------------------------
# Mapa didático dos buffers
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
    data = m_1000_wgs,
    fill = NA,
    color = "grey40",
    linewidth = 0.8
  ) +
  geom_sf(
    data = m_500_wgs,
    fill = NA,
    color = "grey20",
    linewidth = 0.8
  ) +
  geom_sf(
    data = m_250_wgs,
    fill = NA,
    color = "black",
    linewidth = 0.8
  ) +
  geom_sf(
    data = oc_wgs,
    size = 1.2,
    alpha = 0.7
  ) +
  coord_sf(
    xlim = c(-95, -25),
    ylim = c(-45, 20)
  ) +
  labs(
    title = "Delimitação didática da área acessível M",
    subtitle = "Buffers de 250, 500 e 1000 km ao redor dos registros",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold")
  )

print(grafico)

ggsave(
  "figuras/unidade05/unidade05_area_m_buffers.png",
  plot = grafico,
  width = 9,
  height = 7,
  dpi = 600
)

message("Áreas M delimitadas e figura exportada.")
