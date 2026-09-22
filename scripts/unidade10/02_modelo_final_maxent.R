# ============================================================
# Unidade 10 - Maxent em SDM
# Script 02: Modelo final, importância e mapa
# ============================================================

pacotes <- c(
  "dplyr",
  "readr",
  "ggplot2",
  "terra",
  "maxnet",
  "pROC"
)

instalar <- pacotes[!pacotes %in% rownames(installed.packages())]
if (length(instalar) > 0) install.packages(instalar)

invisible(lapply(pacotes, library, character.only = TRUE))

objetos <- readRDS("resultados/unidade10/objetos_base_maxent.rds")

amb <- objetos$amb

r_base <- terra::rast(
  ncols = 120,
  nrows = 100,
  xmin = -80,
  xmax = -40,
  ymin = -30,
  ymax = 10,
  crs = "EPSG:4326"
)
dados_modelo <- objetos$dados_modelo
dados_treino <- objetos$dados_treino
dados_teste <- objetos$dados_teste
preditores <- objetos$preditores
resultados_tuning <- objetos$resultados_tuning

# ------------------------------------------------------------
# 1. Selecionar melhor combinação
# ------------------------------------------------------------
# Critério didático:
# maior AUC e, em caso de empate, menor número de coeficientes.

melhor <- resultados_tuning %>%
  arrange(desc(auc), ncoef) %>%
  slice(1)

feature_final <- melhor$feature_class
regmult_final <- melhor$regmult

x_treino <- dados_treino %>% select(all_of(preditores))
p_treino <- dados_treino$pa

formula_final <- maxnet::maxnet.formula(
  p = p_treino,
  data = x_treino,
  classes = feature_final
)

modelo_final <- maxnet::maxnet(
  p = p_treino,
  data = x_treino,
  f = formula_final,
  regmult = regmult_final
)

saveRDS(
  modelo_final,
  "resultados/unidade10/modelo_maxent_final.rds"
)

write_csv(
  melhor,
  "tabelas/unidade10/modelo_maxent_final_parametros.csv"
)

# ------------------------------------------------------------
# 2. Importância por permutação
# ------------------------------------------------------------

x_teste <- dados_teste %>% select(all_of(preditores))
p_teste <- dados_teste$pa

pred_base <- predict(
  modelo_final,
  newdata = x_teste,
  type = "cloglog"
)

auc_base <- as.numeric(
  pROC::auc(
    pROC::roc(
      response = p_teste,
      predictor = pred_base,
      quiet = TRUE
    )
  )
)

set.seed(789)

importancia <- lapply(preditores, function(v) {
  
  x_perm <- x_teste
  x_perm[[v]] <- sample(x_perm[[v]])
  
  pred_perm <- predict(
    modelo_final,
    newdata = x_perm,
    type = "cloglog"
  )
  
  auc_perm <- as.numeric(
    pROC::auc(
      pROC::roc(
        response = p_teste,
        predictor = pred_perm,
        quiet = TRUE
      )
    )
  )
  
  data.frame(
    variavel = v,
    auc_base = auc_base,
    auc_permutado = auc_perm,
    queda_auc = auc_base - auc_perm
  )
}) %>%
  bind_rows() %>%
  arrange(desc(queda_auc))

write_csv(
  importancia,
  "tabelas/unidade10/importancia_permutacao_maxent.csv"
)

g_imp <- ggplot(
  importancia,
  aes(x = reorder(variavel, queda_auc), y = queda_auc)
) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Importância por permutação - Maxent",
    x = NULL,
    y = "Queda no AUC após permutação"
  ) +
  theme_bw(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

ggsave(
  "figuras/unidade10/unidade10_importancia_variaveis_maxent.png",
  plot = g_imp,
  width = 8,
  height = 5,
  dpi = 600
)

# ------------------------------------------------------------
# 3. Curvas de resposta
# ------------------------------------------------------------

media_prec <- mean(dados_modelo$precipitacao)
media_elev <- mean(dados_modelo$elevacao)
media_solo <- mean(dados_modelo$solo)
media_temp <- mean(dados_modelo$temperatura)

novo_temp <- data.frame(
  temperatura = seq(
    min(dados_modelo$temperatura),
    max(dados_modelo$temperatura),
    length.out = 250
  ),
  precipitacao = media_prec,
  elevacao = media_elev,
  solo = media_solo
)

novo_temp$pred <- predict(
  modelo_final,
  newdata = novo_temp,
  type = "cloglog"
)

g_temp <- ggplot(
  novo_temp,
  aes(x = temperatura, y = pred)
) +
  geom_line(linewidth = 1) +
  labs(
    title = "Curva de resposta - Maxent",
    subtitle = "Temperatura",
    x = "Temperatura simulada",
    y = "Adequabilidade relativa"
  ) +
  theme_bw(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

ggsave(
  "figuras/unidade10/unidade10_resposta_temperatura_maxent.png",
  plot = g_temp,
  width = 8,
  height = 5,
  dpi = 600
)

novo_prec <- data.frame(
  temperatura = media_temp,
  precipitacao = seq(
    min(dados_modelo$precipitacao),
    max(dados_modelo$precipitacao),
    length.out = 250
  ),
  elevacao = media_elev,
  solo = media_solo
)

novo_prec$pred <- predict(
  modelo_final,
  newdata = novo_prec,
  type = "cloglog"
)

g_prec <- ggplot(
  novo_prec,
  aes(x = precipitacao, y = pred)
) +
  geom_line(linewidth = 1) +
  labs(
    title = "Curva de resposta - Maxent",
    subtitle = "Precipitação",
    x = "Precipitação simulada",
    y = "Adequabilidade relativa"
  ) +
  theme_bw(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

ggsave(
  "figuras/unidade10/unidade10_resposta_precipitacao_maxent.png",
  plot = g_prec,
  width = 8,
  height = 5,
  dpi = 600
)

# ------------------------------------------------------------
# 4. Predição espacial
# ------------------------------------------------------------

pred_espacial <- predict(
  modelo_final,
  newdata = amb %>% select(all_of(preditores)),
  type = "cloglog"
)

r_pred <- r_base
terra::values(r_pred) <- pred_espacial

terra::writeRaster(
  r_pred,
  "resultados/unidade10/adequabilidade_maxent.tif",
  overwrite = TRUE
)

png(
  "figuras/unidade10/unidade10_mapa_adequabilidade_maxent.png",
  width = 1800,
  height = 1400,
  res = 220
)

plot(
  r_pred,
  main = "Adequabilidade ambiental estimada por Maxent"
)

points(
  dados_modelo$lon[dados_modelo$pa == 1],
  dados_modelo$lat[dados_modelo$pa == 1],
  pch = 16,
  cex = 0.4
)

dev.off()

message("Script 02 concluído.")
print(melhor)
