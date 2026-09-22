# ============================================================
# Unidade 14 - Transferência espacial e temporal em SDM
# Script 02: Simular ambientes de calibração e projeção
# ============================================================

pacotes <- c("dplyr", "readr", "ggplot2", "terra")

instalar <- pacotes[!pacotes %in% rownames(installed.packages())]
if (length(instalar) > 0) install.packages(instalar)

invisible(lapply(pacotes, library, character.only = TRUE))

dir.create("dados/unidade14/processados", recursive = TRUE, showWarnings = FALSE)
dir.create("figuras/unidade14", recursive = TRUE, showWarnings = FALSE)
dir.create("resultados/unidade14", recursive = TRUE, showWarnings = FALSE)

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

calibracao <- xy %>%
  mutate(
    temperatura = 25 + 0.18 * lat + sin(lon / 7) * 1.8 + rnorm(n(), 0, 0.9),
    precipitacao = 1800 - 9 * abs(lon + 60) + cos(lat / 5) * 160 + rnorm(n(), 0, 80),
    sazonalidade = 45 + 0.35 * abs(lat) + rnorm(n(), 0, 2.5),
    dominio = "Calibracao"
  )

projecao <- calibracao %>%
  mutate(
    temperatura = temperatura + 3.5 + 0.04 * lat,
    precipitacao = precipitacao * 0.88 - 80,
    sazonalidade = sazonalidade + 8 + 0.10 * abs(lat),
    dominio = "Projecao"
  )

dados_amb <- bind_rows(calibracao, projecao)

write_csv(
  dados_amb,
  "dados/unidade14/processados/ambientes_calibracao_projecao.csv"
)

amostra_plot <- dados_amb %>%
  group_by(dominio) %>%
  slice_sample(n = 3000) %>%
  ungroup()

g <- ggplot(
  amostra_plot,
  aes(x = temperatura, y = precipitacao, shape = dominio)
) +
  geom_point(alpha = 0.45, size = 1) +
  labs(
    title = "Espaço ambiental de calibração e projeção",
    subtitle = "Exemplo didático com deslocamento climático futuro",
    x = "Temperatura simulada",
    y = "Precipitação simulada",
    shape = "Domínio"
  ) +
  theme_bw(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

ggsave(
  "figuras/unidade14/unidade14_ambientes_calibracao_projecao.png",
  plot = g,
  width = 8,
  height = 6,
  dpi = 600
)

message("Ambientes de calibração e projeção simulados.")
