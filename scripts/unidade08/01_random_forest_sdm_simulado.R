# ============================================================
# Unidade 8 - Random Forest em SDM
# Script 01: Ajuste de Random Forest com dados simulados
# ============================================================

pacotes <- c(
  "dplyr",
  "readr",
  "ggplot2",
  "terra",
  "ranger",
  "pdp"
)

instalar <- pacotes[!pacotes %in% rownames(installed.packages())]
if (length(instalar) > 0) install.packages(instalar)

invisible(lapply(pacotes, library, character.only = TRUE))

dir.create("dados/unidade08/processados", recursive = TRUE, showWarnings = FALSE)
dir.create("figuras/unidade08", recursive = TRUE, showWarnings = FALSE)
dir.create("tabelas/unidade08", recursive = TRUE, showWarnings = FALSE)
dir.create("resultados/unidade08", recursive = TRUE, showWarnings = FALSE)

set.seed(123)

# ------------------------------------------------------------
# 1. Criar grade espacial simulada
# ------------------------------------------------------------

r_base <- rast(
  ncols = 120,
  nrows = 100,
  xmin = -80,
  xmax = -40,
  ymin = -30,
  ymax = 10,
  crs = "EPSG:4326"
)

xy <- as.data.frame(xyFromCell(r_base, 1:ncell(r_base)))
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
      2.8 * exp(-((temperatura - 25)^2) / (2 * 3^2)) +
      2.3 * exp(-((precipitacao - 1700)^2) / (2 * 350^2)) -
      0.0013 * elevacao +
      0.7 * exp(-((solo - 5.6)^2) / (2 * 0.35^2)),
    adequabilidade_real = plogis(eta),
    peso_presenca = adequabilidade_real / max(adequabilidade_real)
  )

# ------------------------------------------------------------
# 4. Gerar presenças e background
# ------------------------------------------------------------

presencas <- amb %>%
  sample_frac(size = 1, weight = peso_presenca) %>%
  slice(1:350) %>%
  mutate(pa = "presenca")

background <- amb %>%
  slice_sample(n = 3500) %>%
  mutate(pa = "background")

dados_modelo <- bind_rows(presencas, background) %>%
  select(pa, lon, lat, temperatura, precipitacao, elevacao, solo) %>%
  mutate(pa = factor(pa, levels = c("background", "presenca")))

write_csv(
  dados_modelo,
  "dados/unidade08/processados/dados_rf_presenca_background.csv"
)

# ------------------------------------------------------------
# 5. Ajustar Random Forest
# ------------------------------------------------------------
# Usamos probability = TRUE para obter probabilidade da classe presença.
# class.weights reduz o efeito do desbalanceamento entre background
# e presença.

modelo_rf <- ranger(
  pa ~ temperatura + precipitacao + elevacao + solo,
  data = dados_modelo,
  probability = TRUE,
  num.trees = 800,
  mtry = 2,
  min.node.size = 10,
  importance = "permutation",
  class.weights = c(
    background = 1,
    presenca = nrow(background) / nrow(presencas)
  ),
  seed = 123
)

saveRDS(
  modelo_rf,
  "resultados/unidade08/modelo_random_forest.rds"
)

# ------------------------------------------------------------
# 6. Importância de variáveis
# ------------------------------------------------------------

importancia <- data.frame(
  variavel = names(modelo_rf$variable.importance),
  importancia = as.numeric(modelo_rf$variable.importance)
) %>%
  arrange(desc(importancia))

write_csv(
  importancia,
  "tabelas/unidade08/importancia_variaveis_rf.csv"
)

g_imp <- ggplot(
  importancia,
  aes(x = reorder(variavel, importancia), y = importancia)
) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Importância de variáveis - Random Forest",
    x = NULL,
    y = "Importância por permutação"
  ) +
  theme_bw(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

ggsave(
  "figuras/unidade08/unidade08_importancia_variaveis_rf.png",
  plot = g_imp,
  width = 8,
  height = 5,
  dpi = 600
)

# ------------------------------------------------------------
# 7. Curvas de resposta parcial
# ------------------------------------------------------------

pred_fun <- function(object, newdata) {
  predict(object, data = newdata)$predictions[, "presenca"]
}

p_temp <- pdp::partial(
  object = modelo_rf,
  pred.var = "temperatura",
  train = dados_modelo,
  pred.fun = pred_fun,
  grid.resolution = 50
)

g_temp <- ggplot(
  p_temp,
  aes(x = temperatura, y = yhat)
) +
  geom_line(linewidth = 1) +
  labs(
    title = "Resposta parcial - Random Forest",
    subtitle = "Temperatura",
    x = "Temperatura simulada",
    y = "Adequabilidade relativa"
  ) +
  theme_bw(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

ggsave(
  "figuras/unidade08/unidade08_resposta_temperatura_rf.png",
  plot = g_temp,
  width = 8,
  height = 5,
  dpi = 600
)

p_prec <- pdp::partial(
  object = modelo_rf,
  pred.var = "precipitacao",
  train = dados_modelo,
  pred.fun = pred_fun,
  grid.resolution = 50
)

g_prec <- ggplot(
  p_prec,
  aes(x = precipitacao, y = yhat)
) +
  geom_line(linewidth = 1) +
  labs(
    title = "Resposta parcial - Random Forest",
    subtitle = "Precipitação",
    x = "Precipitação simulada",
    y = "Adequabilidade relativa"
  ) +
  theme_bw(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

ggsave(
  "figuras/unidade08/unidade08_resposta_precipitacao_rf.png",
  plot = g_prec,
  width = 8,
  height = 5,
  dpi = 600
)

# ------------------------------------------------------------
# 8. Predição espacial
# ------------------------------------------------------------

pred_rf <- predict(
  modelo_rf,
  data = amb
)$predictions[, "presenca"]

r_pred <- r_base
values(r_pred) <- pred_rf

writeRaster(
  r_pred,
  "resultados/unidade08/adequabilidade_random_forest.tif",
  overwrite = TRUE
)

png(
  "figuras/unidade08/unidade08_mapa_adequabilidade_rf.png",
  width = 1800,
  height = 1400,
  res = 220
)

plot(
  r_pred,
  main = "Adequabilidade ambiental estimada por Random Forest"
)

points(
  presencas$lon,
  presencas$lat,
  pch = 16,
  cex = 0.4
)

dev.off()

message("Script 01 concluído.")
