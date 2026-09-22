# ============================================================
# Unidade 13 - Projeções Climáticas Futuras em SDM
# Script 02: Ajustar modelo no clima atual
# ============================================================

pacotes <- c("dplyr", "readr", "terra", "ggplot2")

instalar <- pacotes[!pacotes %in% rownames(installed.packages())]
if (length(instalar) > 0) install.packages(instalar)

invisible(lapply(pacotes, library, character.only = TRUE))

climas <- read_csv(
  "dados/unidade13/processados/clima_atual_futuro_simulado.csv",
  show_col_types = FALSE
)

atual <- climas %>%
  filter(periodo == "Atual")

set.seed(123)

atual <- atual %>%
  mutate(
    eta =
      -6 +
      2.8 * exp(-((temperatura - 25)^2) / (2 * 3^2)) +
      2.4 * exp(-((precipitacao - 1700)^2) / (2 * 350^2)) -
      0.025 * sazonalidade,
    adequabilidade_real = plogis(eta),
    peso_presenca = adequabilidade_real / max(adequabilidade_real)
  )

presencas <- atual %>%
  sample_frac(size = 1, weight = peso_presenca) %>%
  slice(1:350) %>%
  mutate(pa = 1)

background <- atual %>%
  slice_sample(n = 3500) %>%
  mutate(pa = 0)

dados_modelo <- bind_rows(presencas, background) %>%
  select(pa, lon, lat, temperatura, precipitacao, sazonalidade)

modelo_glm <- glm(
  pa ~ temperatura + I(temperatura^2) +
    precipitacao + I(precipitacao^2) +
    sazonalidade,
  data = dados_modelo,
  family = binomial(link = "logit")
)

saveRDS(
  modelo_glm,
  "resultados/unidade13/modelo_glm_clima_atual.rds"
)

write_csv(
  dados_modelo,
  "dados/unidade13/processados/dados_modelo_clima_atual.csv"
)

atual$adequabilidade_atual <- predict(
  modelo_glm,
  newdata = atual,
  type = "response"
)

write_csv(
  atual,
  "dados/unidade13/processados/predicao_adequabilidade_atual.csv"
)

r_base <- terra::rast(
  ncols = 120,
  nrows = 100,
  xmin = -80,
  xmax = -40,
  ymin = -30,
  ymax = 10,
  crs = "EPSG:4326"
)

r_atual <- r_base
terra::values(r_atual) <- atual$adequabilidade_atual

terra::writeRaster(
  r_atual,
  "resultados/unidade13/adequabilidade_atual.tif",
  overwrite = TRUE
)

png(
  "figuras/unidade13/unidade13_adequabilidade_atual.png",
  width = 1800,
  height = 1400,
  res = 220
)

plot(r_atual, main = "Adequabilidade ambiental - clima atual")

points(
  presencas$lon,
  presencas$lat,
  pch = 16,
  cex = 0.4
)

dev.off()

message("Modelo ajustado no clima atual.")
