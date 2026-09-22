# ============================================================
# Unidade 15 - Incertezas em SDM
# Script 05: Incerteza associada à escala espacial
# ============================================================

pacotes <- c("dplyr", "readr", "ggplot2", "terra")

instalar <- pacotes[!pacotes %in% rownames(installed.packages())]
if (length(instalar) > 0) install.packages(instalar)

invisible(lapply(pacotes, library, character.only = TRUE))

base <- read_csv(
  "dados/unidade15/processados/predicoes_algoritmos_simuladas.csv",
  show_col_types = FALSE
)

r_fino <- terra::rast(
  ncols = 120,
  nrows = 100,
  xmin = -80,
  xmax = -40,
  ymin = -30,
  ymax = 10,
  crs = "EPSG:4326"
)

terra::values(r_fino) <- base$glm

# Agregar para resolução mais grosseira e reamostrar de volta
r_grosso <- terra::aggregate(r_fino, fact = 4, fun = mean)
r_grosso_reamostrado <- terra::resample(r_grosso, r_fino, method = "bilinear")

escala_df <- base %>%
  select(lon, lat) %>%
  mutate(
    pred_fina = terra::values(r_fino)[, 1],
    pred_grosseira = terra::values(r_grosso_reamostrado)[, 1],
    diferenca_escala = abs(pred_fina - pred_grosseira)
  )

write_csv(
  escala_df,
  "dados/unidade15/processados/incerteza_escala.csv"
)

g <- ggplot(
  escala_df,
  aes(x = lon, y = lat, fill = diferenca_escala)
) +
  geom_raster() +
  coord_equal() +
  scale_fill_viridis_c(name = "|Diferença|") +
  labs(
    title = "Incerteza associada à escala espacial",
    subtitle = "Diferença absoluta entre predição fina e agregada",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid = element_blank()
  )

ggsave(
  "figuras/unidade15/unidade15_incerteza_escala.png",
  plot = g,
  width = 8,
  height = 6,
  dpi = 600
)

message("Incerteza de escala espacial calculada.")
