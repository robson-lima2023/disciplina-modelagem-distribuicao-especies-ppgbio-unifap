# ============================================================
# Unidade 17 - Projeto Integrador
# Script 05: Avaliação e ensemble
# ============================================================

pacotes <- c("dplyr", "readr", "ggplot2", "terra", "pROC", "gbm")

instalar <- pacotes[!pacotes %in% rownames(installed.packages())]
if (length(instalar) > 0) install.packages(instalar)

invisible(lapply(pacotes, library, character.only = TRUE))

dados <- read_csv("dados/unidade17/processados/dados_modelo_integrador.csv", show_col_types = FALSE)
amb <- read_csv("dados/unidade17/processados/ambiente_integrador.csv", show_col_types = FALSE)
modelos <- readRDS("resultados/unidade17/modelos_integrador.rds")

pred_glm <- predict(modelos$glm, newdata = amb, type = "response")
pred_gam <- predict(modelos$gam, newdata = amb, type = "response")
pred_rf <- predict(modelos$rf, data = amb)$predictions[, "presenca"]
pred_brt <- predict(modelos$brt, newdata = amb, n.trees = modelos$melhor_brt, type = "response")

predicoes <- amb %>%
  select(lon, lat) %>%
  mutate(
    glm = as.numeric(pred_glm),
    gam = as.numeric(pred_gam),
    rf  = as.numeric(pred_rf),
    brt = as.numeric(pred_brt)
  )

mat_pred <- as.matrix(
  predicoes[, c("glm", "gam", "rf", "brt")]
)

predicoes$ensemble <- rowMeans(mat_pred, na.rm = TRUE)
predicoes$incerteza <- apply(mat_pred, 1, sd, na.rm = TRUE)

readr::write_csv(
  predicoes,
  "dados/unidade17/processados/predicoes_ensemble_integrador.csv"
)

# Avaliação nos pontos usados
pred_pontos <- dados %>%
  mutate(
    pred_glm = predict(modelos$glm, newdata = dados, type = "response"),
    pred_gam = predict(modelos$gam, newdata = dados, type = "response"),
    pred_rf = predict(modelos$rf, data = dados)$predictions[, "presenca"],
    pred_brt = predict(modelos$brt, newdata = dados, n.trees = modelos$melhor_brt, type = "response"),
    pred_ensemble = rowMeans(data.frame(pred_glm, pred_gam, pred_rf, pred_brt))
  )

avaliacao <- data.frame(
  modelo = c("GLM", "GAM", "Random Forest", "BRT", "Ensemble"),
  AUC = c(
    as.numeric(pROC::auc(pROC::roc(pred_pontos$pa, pred_pontos$pred_glm, quiet = TRUE))),
    as.numeric(pROC::auc(pROC::roc(pred_pontos$pa, pred_pontos$pred_gam, quiet = TRUE))),
    as.numeric(pROC::auc(pROC::roc(pred_pontos$pa, pred_pontos$pred_rf, quiet = TRUE))),
    as.numeric(pROC::auc(pROC::roc(pred_pontos$pa, pred_pontos$pred_brt, quiet = TRUE))),
    as.numeric(pROC::auc(pROC::roc(pred_pontos$pa, pred_pontos$pred_ensemble, quiet = TRUE)))
  )
)

write_csv(avaliacao, "tabelas/unidade17/avaliacao_modelos_integrador.csv")

# Figuras
g1 <- ggplot(predicoes, aes(x = lon, y = lat, fill = ensemble)) +
  geom_raster() +
  coord_equal() +
  scale_fill_viridis_c(name = "Adequabilidade") +
  labs(title = "Ensemble médio", x = "Longitude", y = "Latitude") +
  theme_bw(base_size = 12) +
  theme(plot.title = element_text(face = "bold"), panel.grid = element_blank())

ggsave("figuras/unidade17/unidade17_ensemble_medio.png", g1, width = 8, height = 6, dpi = 600)

g2 <- ggplot(predicoes, aes(x = lon, y = lat, fill = incerteza)) +
  geom_raster() +
  coord_equal() +
  scale_fill_viridis_c(name = "SD") +
  labs(title = "Incerteza do ensemble", x = "Longitude", y = "Latitude") +
  theme_bw(base_size = 12) +
  theme(plot.title = element_text(face = "bold"), panel.grid = element_blank())

ggsave("figuras/unidade17/unidade17_incerteza_ensemble.png", g2, width = 8, height = 6, dpi = 600)

message("Avaliação e ensemble concluídos.")
print(avaliacao)
