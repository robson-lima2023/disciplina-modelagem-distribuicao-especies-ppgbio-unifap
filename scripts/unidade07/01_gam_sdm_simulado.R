# ============================================================
# Unidade 7 - GAM em Modelagem de Distribuição de Espécies
# Script 01: Ajuste de GAM com dados simulados
# ============================================================

pacotes <- c(
  "dplyr",
  "readr",
  "ggplot2",
  "terra",
  "mgcv"
)

instalar <- pacotes[!pacotes %in% rownames(installed.packages())]
if (length(instalar) > 0) install.packages(instalar)

invisible(lapply(pacotes, library, character.only = TRUE))

dir.create("dados/unidade07/processados", recursive = TRUE, showWarnings = FALSE)
dir.create("figuras/unidade07", recursive = TRUE, showWarnings = FALSE)
dir.create("tabelas/unidade07", recursive = TRUE, showWarnings = FALSE)
dir.create("resultados/unidade07", recursive = TRUE, showWarnings = FALSE)

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
    elevacao = 600 + 250 * sin((lon + 70) / 5) + 120 * cos(lat / 6)
  )

# ------------------------------------------------------------
# 3. Simular resposta ecológica não linear
# ------------------------------------------------------------
# A espécie hipotética possui resposta não linear:
# - ótimo térmico intermediário;
# - maior adequabilidade em precipitação intermediária;
# - penalização em elevações muito altas.

amb <- amb %>%
  mutate(
    eta =
      -5 +
      2.5 * exp(-((temperatura - 25)^2) / (2 * 3^2)) +
      2.2 * exp(-((precipitacao - 1700)^2) / (2 * 350^2)) -
      0.0015 * elevacao,
    adequabilidade_real = plogis(eta)
  )

# ------------------------------------------------------------
# 4. Gerar presenças e background
# ------------------------------------------------------------

amb <- amb %>%
  mutate(peso_presenca = adequabilidade_real / max(adequabilidade_real))

presencas <- amb %>%
  sample_frac(size = 1, weight = peso_presenca) %>%
  slice(1:350) %>%
  mutate(pa = 1)

background <- amb %>%
  slice_sample(n = 3500) %>%
  mutate(pa = 0)

dados_modelo <- bind_rows(presencas, background) %>%
  select(pa, lon, lat, temperatura, precipitacao, elevacao)

write_csv(
  dados_modelo,
  "dados/unidade07/processados/dados_gam_presenca_background.csv"
)

# ------------------------------------------------------------
# 5. Ajustar GAM binomial
# ------------------------------------------------------------

modelo_gam <- mgcv::gam(
  pa ~
    s(temperatura, k = 6) +
    s(precipitacao, k = 6) +
    s(elevacao, k = 5),
  data = dados_modelo,
  family = binomial(link = "logit"),
  method = "REML"
)

saveRDS(
  modelo_gam,
  "resultados/unidade07/modelo_gam.rds"
)

sink("resultados/unidade07/resumo_modelo_gam.txt")
print(summary(modelo_gam))
sink()

# ------------------------------------------------------------
# 6. Curvas de resposta
# ------------------------------------------------------------

media_prec <- mean(dados_modelo$precipitacao)
media_elev <- mean(dados_modelo$elevacao)
media_temp <- mean(dados_modelo$temperatura)

novo_temp <- data.frame(
  temperatura = seq(min(dados_modelo$temperatura),
                    max(dados_modelo$temperatura),
                    length.out = 250),
  precipitacao = media_prec,
  elevacao = media_elev
)

novo_temp$pred <- predict(modelo_gam, newdata = novo_temp, type = "response")

g_temp <- ggplot(novo_temp, aes(x = temperatura, y = pred)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Curva de resposta do GAM",
    subtitle = "Efeito suave da temperatura",
    x = "Temperatura simulada",
    y = "Adequabilidade relativa"
  ) +
  theme_bw(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

ggsave(
  "figuras/unidade07/unidade07_curva_gam_temperatura.png",
  plot = g_temp,
  width = 8,
  height = 5,
  dpi = 600
)

novo_prec <- data.frame(
  temperatura = media_temp,
  precipitacao = seq(min(dados_modelo$precipitacao),
                     max(dados_modelo$precipitacao),
                     length.out = 250),
  elevacao = media_elev
)

novo_prec$pred <- predict(modelo_gam, newdata = novo_prec, type = "response")

g_prec <- ggplot(novo_prec, aes(x = precipitacao, y = pred)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Curva de resposta do GAM",
    subtitle = "Efeito suave da precipitação",
    x = "Precipitação simulada",
    y = "Adequabilidade relativa"
  ) +
  theme_bw(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

ggsave(
  "figuras/unidade07/unidade07_curva_gam_precipitacao.png",
  plot = g_prec,
  width = 8,
  height = 5,
  dpi = 600
)

# ------------------------------------------------------------
# 7. Predição espacial
# ------------------------------------------------------------

amb$pred_gam <- predict(
  modelo_gam,
  newdata = amb,
  type = "response"
)

r_pred <- r_base
values(r_pred) <- amb$pred_gam

writeRaster(
  r_pred,
  "resultados/unidade07/adequabilidade_gam.tif",
  overwrite = TRUE
)

png(
  "figuras/unidade07/unidade07_mapa_adequabilidade_gam.png",
  width = 1800,
  height = 1400,
  res = 220
)

plot(
  r_pred,
  main = "Adequabilidade ambiental estimada por GAM"
)

points(
  presencas$lon,
  presencas$lat,
  pch = 16,
  cex = 0.4
)

dev.off()

message("Script 01 concluído.")
