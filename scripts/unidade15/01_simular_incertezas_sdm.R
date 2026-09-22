# ============================================================
# Unidade 15 - Incertezas em SDM
# Script 01: Simular predições e fontes de incerteza
# ============================================================

setwd("C:/Users/rblfl/OneDrive/Documentos/Playground/Species-Distribution-Modeling")

pacotes <- c("dplyr", "readr", "ggplot2", "terra", "tidyr")

instalar <- pacotes[!pacotes %in% rownames(installed.packages())]
if (length(instalar) > 0) install.packages(instalar)

invisible(lapply(pacotes, library, character.only = TRUE))

dir.create("dados/unidade15/processados", recursive = TRUE, showWarnings = FALSE)
dir.create("figuras/unidade15", recursive = TRUE, showWarnings = FALSE)
dir.create("tabelas/unidade15", recursive = TRUE, showWarnings = FALSE)
dir.create("resultados/unidade15", recursive = TRUE, showWarnings = FALSE)

set.seed(123)

r_base <- terra::rast(
  ncols = 120,
  nrows = 100,
  xmin = -80,
  xmax = -40,
  ymin = -30,
  ymax = 10,
  crs = "EPSG:4326"
)

xy <- as.data.frame(terra::xyFromCell(r_base, 1:terra::ncell(r_base)))
names(xy) <- c("lon", "lat")

amb <- xy %>%
  mutate(
    temperatura = 25 + 0.20 * lat + sin(lon / 6) * 2,
    precipitacao = 1800 - 10 * abs(lon + 60) + cos(lat / 4) * 200,
    solo = 5.5 + 0.3 * sin(lon / 5)
  )

base_eta <- with(
  amb,
  -5 +
    2.8 * exp(-((temperatura - 25)^2) / (2 * 3^2)) +
    2.4 * exp(-((precipitacao - 1700)^2) / (2 * 350^2)) +
    0.7 * exp(-((solo - 5.6)^2) / (2 * 0.35^2))
)

predicoes <- amb %>%
  mutate(
    glm = plogis(base_eta + rnorm(n(), 0, 0.10)),
    gam = plogis(base_eta + 0.20 * sin(lat / 5) + rnorm(n(), 0, 0.12)),
    rf = plogis(base_eta + 0.25 * cos(lon / 7) + rnorm(n(), 0, 0.15)),
    brt = plogis(base_eta + 0.20 * sin((lon + lat) / 8) + rnorm(n(), 0, 0.15)),
    maxent = plogis(base_eta + 0.15 * cos(lat / 4) + rnorm(n(), 0, 0.12))
  )

write_csv(
  predicoes,
  "dados/unidade15/processados/predicoes_algoritmos_simuladas.csv"
)

plot_alg <- predicoes %>%
  select(lon, lat, glm, gam, rf, brt, maxent) %>%
  pivot_longer(
    cols = c(glm, gam, rf, brt, maxent),
    names_to = "algoritmo",
    values_to = "adequabilidade"
  )

g <- ggplot(
  plot_alg,
  aes(x = lon, y = lat, fill = adequabilidade)
) +
  geom_raster() +
  coord_equal() +
  facet_wrap(~ algoritmo, ncol = 3) +
  scale_fill_viridis_c(name = "Adequabilidade") +
  labs(
    title = "Predições simuladas por diferentes algoritmos",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid = element_blank()
  )

ggsave(
  "figuras/unidade15/unidade15_predicoes_algoritmos.png",
  plot = g,
  width = 10,
  height = 7,
  dpi = 600
)

message("Predições simuladas para incerteza criadas.")
