source("scripts/_bootstrap.R")

# ============================================================
# Unidade 2 - Dados de ocorrência
# Script 06: Fluxograma de preparação dos dados
# ============================================================

pacotes <- c("ggplot2")

require_packages(pacotes)
library(ggplot2)

fluxo <- data.frame(
  etapa = c(
    "Compilação\nLocal + GBIF",
    "Padronização\nTaxonômica e espacial",
    "Limpeza\nCoordinateCleaner",
    "Recorte\nBioma Amazônia",
    "Duplicatas\npor célula raster",
    "Rarefação\nespacial",
    "Extração\nclimática",
    "Base final\npara SDM"
  ),
  x = 1:8,
  y = 1
)

g <- ggplot(fluxo, aes(x = x, y = y)) +
  geom_segment(
    data = fluxo[-nrow(fluxo), ],
    aes(x = x + 0.35, xend = x + 0.65, y = y, yend = y),
    arrow = arrow(length = unit(0.18, "cm")),
    linewidth = 0.6
  ) +
  geom_label(
    aes(label = etapa),
    size = 3.2,
    label.size = 0.35,
    fill = "grey95"
  ) +
  xlim(0.5, 8.5) +
  ylim(0.6, 1.4) +
  labs(
    title = expression("Fluxo de preparação das ocorrências de " * italic("Dinizia excelsa")),
    subtitle = "Da compilação dos registros à base final para modelagem"
  ) +
  theme_void(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 10)
  )

ggsave(
  "figuras/unidade02/unidade02_fluxograma_preparacao_dinizia.png",
  plot = g,
  width = 12,
  height = 3,
  dpi = 600
)

message("Script 06 concluído: fluxograma gerado.")
