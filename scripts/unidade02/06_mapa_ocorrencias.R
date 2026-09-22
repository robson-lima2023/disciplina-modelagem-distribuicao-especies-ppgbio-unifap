# ============================================================
# Unidade 2 - Dados de Ocorrência em SDM
# Script 06: Mapa dos registros de ocorrência
# ============================================================

pacotes <- c("dplyr", "readr", "ggplot2", "maps")

instalar <- pacotes[!pacotes %in% rownames(installed.packages())]
if (length(instalar) > 0) install.packages(instalar)

invisible(lapply(pacotes, library, character.only = TRUE))

dir.create("figuras/unidade02", recursive = TRUE, showWarnings = FALSE)

oc <- read_csv(
  "dados/unidade02/processados/ocorrencias_03_sem_duplicatas.csv",
  show_col_types = FALSE
)

mundo <- map_data("world")

mapa <- ggplot() +
  geom_polygon(
    data = mundo,
    aes(x = long, y = lat, group = group),
    fill = "grey95",
    color = "grey65",
    linewidth = 0.2
  ) +
  geom_point(
    data = oc,
    aes(x = lon, y = lat),
    color = "black",
    alpha = 0.70,
    size = 1.3
  ) +
  coord_quickmap(
    xlim = c(-120, -30),
    ylim = c(-60, 35)
  ) +
  labs(
    title = "Registros de ocorrência após limpeza inicial",
    subtitle = "Dados obtidos via GBIF",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid = element_line(color = "grey90")
  )

print(mapa)

ggsave(
  filename = "figuras/unidade02/unidade02_mapa_ocorrencias.png",
  plot = mapa,
  width = 8,
  height = 6,
  dpi = 600
)

message("Mapa exportado para figuras/unidade02.")
