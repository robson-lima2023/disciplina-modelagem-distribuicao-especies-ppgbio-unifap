# ============================================================
# Unidade 15 - Incertezas em SDM
# Script 02: Incerteza entre algoritmos
# ============================================================

pacotes <- c("dplyr", "readr", "ggplot2", "terra")

instalar <- pacotes[!pacotes %in% rownames(installed.packages())]
if (length(instalar) > 0) install.packages(instalar)

invisible(lapply(pacotes, library, character.only = TRUE))

predicoes <- read_csv(
  "dados/unidade15/processados/predicoes_algoritmos_simuladas.csv",
  show_col_types = FALSE
)

algoritmos <- c("glm", "gam", "rf", "brt", "maxent")

inc_alg <- predicoes %>%
  mutate(
    consenso_medio = rowMeans(select(., all_of(algoritmos))),
    incerteza_sd = apply(select(., all_of(algoritmos)), 1, sd),
    incerteza_cv = incerteza_sd / consenso_medio
  )

write_csv(
  inc_alg,
  "dados/unidade15/processados/incerteza_algoritmos.csv"
)

g <- ggplot(
  inc_alg,
  aes(x = lon, y = lat, fill = incerteza_sd)
) +
  geom_raster() +
  coord_equal() +
  scale_fill_viridis_c(name = "SD") +
  labs(
    title = "Incerteza entre algoritmos",
    subtitle = "Desvio-padrão das predições por pixel",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid = element_blank()
  )

ggsave(
  "figuras/unidade15/unidade15_incerteza_algoritmos.png",
  plot = g,
  width = 8,
  height = 6,
  dpi = 600
)

message("Incerteza entre algoritmos calculada.")
