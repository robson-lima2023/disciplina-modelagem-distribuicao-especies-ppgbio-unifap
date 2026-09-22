# ============================================================
# Unidade 17 - Projeto Integrador
# Script 02: Dados de ocorrência e ambiente
# ============================================================

pacotes <- c("dplyr", "readr", "ggplot2", "terra")

instalar <- pacotes[!pacotes %in% rownames(installed.packages())]
if (length(instalar) > 0) install.packages(instalar)

invisible(lapply(pacotes, library, character.only = TRUE))

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
    temperatura = 25 + 0.20 * lat + sin(lon / 6) * 2 + rnorm(n(), 0, 1.1),
    precipitacao = 1800 - 10 * abs(lon + 60) + cos(lat / 4) * 180 + rnorm(n(), 0, 90),
    elevacao = 600 + 250 * sin((lon + 70) / 5) + 120 * cos(lat / 6),
    solo = 5.5 + 0.3 * sin(lon / 5) + rnorm(n(), 0, 0.2)
  ) %>%
  mutate(
    eta =
      -5 +
      2.8 * exp(-((temperatura - 25)^2) / (2 * 3^2)) +
      2.4 * exp(-((precipitacao - 1700)^2) / (2 * 350^2)) -
      0.0012 * elevacao +
      0.7 * exp(-((solo - 5.6)^2) / (2 * 0.35^2)),
    adequabilidade_real = plogis(eta),
    peso_presenca = adequabilidade_real / max(adequabilidade_real)
  )

presencas <- amb %>%
  sample_frac(size = 1, weight = peso_presenca) %>%
  slice(1:350) %>%
  mutate(pa = 1)

background <- amb %>%
  slice_sample(n = 3500) %>%
  mutate(pa = 0)

dados_modelo <- bind_rows(presencas, background) %>%
  select(pa, lon, lat, temperatura, precipitacao, elevacao, solo)

write_csv(amb, "dados/unidade17/processados/ambiente_integrador.csv")
write_csv(dados_modelo, "dados/unidade17/processados/dados_modelo_integrador.csv")

g <- ggplot() +
  geom_raster(
    data = amb,
    aes(x = lon, y = lat, fill = adequabilidade_real)
  ) +
  geom_point(
    data = presencas,
    aes(x = lon, y = lat),
    size = 0.6,
    alpha = 0.65
  ) +
  coord_equal() +
  scale_fill_viridis_c(name = "Adequabilidade") +
  labs(
    title = "Ocorrências simuladas e ambiente",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid = element_blank()
  )

ggsave(
  "figuras/unidade17/unidade17_ocorrencias_ambiente.png",
  plot = g,
  width = 8,
  height = 6,
  dpi = 600
)

message("Dados de ocorrência e ambiente preparados.")
