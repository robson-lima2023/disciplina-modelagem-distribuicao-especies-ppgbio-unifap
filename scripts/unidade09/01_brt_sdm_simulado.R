# ============================================================
# Unidade 9 - Boosted Regression Trees em SDM
# Script 01: Ajuste de BRT com dados simulados
# ============================================================

pacotes <- c(
  "dplyr",
  "readr",
  "ggplot2",
  "terra",
  "gbm",
  "pdp"
)

instalar <- pacotes[!pacotes %in% rownames(installed.packages())]
if (length(instalar) > 0) install.packages(instalar)

invisible(lapply(pacotes, library, character.only = TRUE))

dir.create("dados/unidade09/processados", recursive = TRUE, showWarnings = FALSE)
dir.create("figuras/unidade09", recursive = TRUE, showWarnings = FALSE)
dir.create("tabelas/unidade09", recursive = TRUE, showWarnings = FALSE)
dir.create("resultados/unidade09", recursive = TRUE, showWarnings = FALSE)

set.seed(123)

# ------------------------------------------------------------
# 1. Criar uma grade espacial simulada
# ------------------------------------------------------------

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

# ------------------------------------------------------------
# 2. Simular variáveis ambientais
# ------------------------------------------------------------

amb <- xy %>%
  mutate(
    temperatura = 25 + 0.20 * lat + sin(lon / 6) * 2 + rnorm(n(), 0, 1.2),
    precipitacao = 1800 - 10 * abs(lon + 60) + cos(lat / 4) * 200 + rnorm(n(), 0, 100),
    elevacao = 600 + 250 * sin((lon + 70) / 5) + 120 * cos(lat / 6),
    solo = 5.5 + 0.3 * sin(lon / 5) + rnorm(n(), 0, 0.2)
  )

# ------------------------------------------------------------
# 3. Simular adequabilidade real
# ------------------------------------------------------------

amb <- amb %>%
  mutate(
    eta =
      -5 +
      2.7 * exp(-((temperatura - 25)^2) / (2 * 3^2)) +
      2.4 * exp(-((precipitacao - 1700)^2) / (2 * 350^2)) -
      0.0013 * elevacao +
      0.8 * exp(-((solo - 5.6)^2) / (2 * 0.35^2)) +
      0.0008 * (temperatura - 24) * (solo - 5.5),
    adequabilidade_real = plogis(eta),
    peso_presenca = adequabilidade_real / max(adequabilidade_real)
  )

# ------------------------------------------------------------
# 4. Gerar presenças e background
# ------------------------------------------------------------

presencas <- amb %>%
  sample_frac(size = 1, weight = peso_presenca) %>%
  slice(1:350) %>%
  mutate(pa = 1)

background <- amb %>%
  slice_sample(n = 3500) %>%
  mutate(pa = 0)

dados_modelo <- bind_rows(presencas, background) %>%
  select(pa, lon, lat, temperatura, precipitacao, elevacao, solo)

write_csv(
  dados_modelo,
  "dados/unidade09/processados/dados_brt_presenca_background.csv"
)

# ------------------------------------------------------------
# 5. Ajustar Boosted Regression Trees
# ------------------------------------------------------------
# distribution = "bernoulli" indica resposta binária.
# n.trees define o número máximo de árvores.
# interaction.depth controla a profundidade das árvores.
# shrinkage é a taxa de aprendizado.
# bag.fraction introduz estocasticidade.
# cv.folds permite validação cruzada interna.

modelo_brt <- gbm::gbm(
  formula = pa ~ temperatura + precipitacao + elevacao + solo,
  data = dados_modelo,
  distribution = "bernoulli",
  n.trees = 3000,
  interaction.depth = 3,
  shrinkage = 0.01,
  bag.fraction = 0.6,
  train.fraction = 1.0,
  cv.folds = 5,
  n.minobsinnode = 10,
  verbose = FALSE
)

melhor_iter <- gbm::gbm.perf(
  modelo_brt,
  method = "cv",
  plot.it = FALSE
)

saveRDS(
  modelo_brt,
  "resultados/unidade09/modelo_brt.rds"
)

write_csv(
  data.frame(melhor_iteracao = melhor_iter),
  "tabelas/unidade09/melhor_iteracao_brt.csv"
)

# ------------------------------------------------------------
# 6. Importância relativa das variáveis
# ------------------------------------------------------------

importancia <- summary(
  modelo_brt,
  n.trees = melhor_iter,
  plotit = FALSE
) %>%
  as.data.frame()

names(importancia) <- c("variavel", "importancia_relativa")

write_csv(
  importancia,
  "tabelas/unidade09/importancia_variaveis_brt.csv"
)

g_imp <- ggplot(
  importancia,
  aes(x = reorder(variavel, importancia_relativa), y = importancia_relativa)
) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Importância relativa das variáveis - BRT",
    x = NULL,
    y = "Importância relativa (%)"
  ) +
  theme_bw(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

ggsave(
  "figuras/unidade09/unidade09_importancia_variaveis_brt.png",
  plot = g_imp,
  width = 8,
  height = 5,
  dpi = 600
)

# ------------------------------------------------------------
# 7. Curvas de resposta parcial
# ------------------------------------------------------------

pred_fun_brt <- function(object, newdata) {
  predict(
    object,
    newdata = newdata,
    n.trees = melhor_iter,
    type = "response"
  )
}

p_temp <- pdp::partial(
  object = modelo_brt,
  pred.var = "temperatura",
  train = dados_modelo,
  pred.fun = pred_fun_brt,
  grid.resolution = 80,
  recursive = FALSE
)

g_temp <- ggplot(
  p_temp,
  aes(x = temperatura, y = yhat)
) +
  geom_line(linewidth = 1) +
  labs(
    title = "Resposta parcial - BRT",
    subtitle = "Temperatura",
    x = "Temperatura simulada",
    y = "Adequabilidade relativa"
  ) +
  theme_bw(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

ggsave(
  "figuras/unidade09/unidade09_resposta_temperatura_brt.png",
  plot = g_temp,
  width = 8,
  height = 5,
  dpi = 600
)

p_prec <- pdp::partial(
  object = modelo_brt,
  pred.var = "precipitacao",
  train = dados_modelo,
  pred.fun = pred_fun_brt,
  grid.resolution = 80,
  recursive = FALSE
)

g_prec <- ggplot(
  p_prec,
  aes(x = precipitacao, y = yhat)
) +
  geom_line(linewidth = 1) +
  labs(
    title = "Resposta parcial - BRT",
    subtitle = "Precipitação",
    x = "Precipitação simulada",
    y = "Adequabilidade relativa"
  ) +
  theme_bw(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

ggsave(
  "figuras/unidade09/unidade09_resposta_precipitacao_brt.png",
  plot = g_prec,
  width = 8,
  height = 5,
  dpi = 600
)

# ------------------------------------------------------------
# 8. Predição espacial
# ------------------------------------------------------------

pred_brt <- predict(
  modelo_brt,
  newdata = amb,
  n.trees = melhor_iter,
  type = "response"
)

r_pred <- r_base
terra::values(r_pred) <- pred_brt

terra::writeRaster(
  r_pred,
  "resultados/unidade09/adequabilidade_brt.tif",
  overwrite = TRUE
)

png(
  "figuras/unidade09/unidade09_mapa_adequabilidade_brt.png",
  width = 1800,
  height = 1400,
  res = 220
)

plot(
  r_pred,
  main = "Adequabilidade ambiental estimada por BRT"
)

points(
  presencas$lon,
  presencas$lat,
  pch = 16,
  cex = 0.4
)

dev.off()

message("Script 01 concluído.")
message(paste("Melhor número de árvores:", melhor_iter))
