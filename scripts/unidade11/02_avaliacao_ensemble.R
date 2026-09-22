# ============================================================
# Unidade 11 - Modelagem Ensemble em SDM
# Script 02: Avaliação e ensemble ponderado
# ============================================================

pacotes <- c(
  "dplyr",
  "readr",
  "ggplot2",
  "terra",
  "pROC"
)

instalar <- pacotes[!pacotes %in% rownames(installed.packages())]
if (length(instalar) > 0) install.packages(instalar)

invisible(lapply(pacotes, library, character.only = TRUE))

objetos <- readRDS("resultados/unidade11/objetos_ensemble.rds")

amb <- objetos$amb
dados_modelo <- objetos$dados_modelo
predicoes <- objetos$predicoes

# ------------------------------------------------------------
# 1. Extrair predições nos pontos de calibração
# ------------------------------------------------------------
# Como usamos uma simulação em grade, buscamos a célula mais próxima
# combinando lon/lat. Para fins didáticos, fazemos junção pelas
# coordenadas arredondadas.

amb_id <- amb %>%
  mutate(
    id_cell = row_number(),
    lon_r = round(lon, 6),
    lat_r = round(lat, 6)
  )

dados_id <- dados_modelo %>%
  mutate(
    lon_r = round(lon, 6),
    lat_r = round(lat, 6)
  ) %>%
  left_join(
    amb_id %>% select(id_cell, lon_r, lat_r),
    by = c("lon_r", "lat_r")
  )

pred_pontos <- predicoes[dados_id$id_cell, ]

avaliacao <- data.frame(
  modelo = c("GLM", "GAM", "Random Forest", "BRT", "Maxent", "Ensemble médio"),
  AUC = c(
    as.numeric(pROC::auc(pROC::roc(dados_modelo$pa, pred_pontos$glm, quiet = TRUE))),
    as.numeric(pROC::auc(pROC::roc(dados_modelo$pa, pred_pontos$gam, quiet = TRUE))),
    as.numeric(pROC::auc(pROC::roc(dados_modelo$pa, pred_pontos$rf, quiet = TRUE))),
    as.numeric(pROC::auc(pROC::roc(dados_modelo$pa, pred_pontos$brt, quiet = TRUE))),
    as.numeric(pROC::auc(pROC::roc(dados_modelo$pa, pred_pontos$maxent, quiet = TRUE))),
    as.numeric(pROC::auc(pROC::roc(dados_modelo$pa, pred_pontos$ensemble_medio, quiet = TRUE)))
  )
)

write_csv(
  avaliacao,
  "tabelas/unidade11/avaliacao_auc_ensemble.csv"
)

g_auc <- ggplot(
  avaliacao,
  aes(x = reorder(modelo, AUC), y = AUC)
) +
  geom_col() +
  coord_flip() +
  ylim(0, 1) +
  labs(
    title = "Avaliação dos modelos do ensemble",
    subtitle = "AUC calculado em dados simulados",
    x = NULL,
    y = "AUC"
  ) +
  theme_bw(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

ggsave(
  "figuras/unidade11/unidade11_comparacao_auc_ensemble.png",
  plot = g_auc,
  width = 8,
  height = 5,
  dpi = 600
)

# ------------------------------------------------------------
# 2. Ensemble ponderado por AUC
# ------------------------------------------------------------

pesos <- avaliacao %>%
  filter(modelo %in% c("GLM", "GAM", "Random Forest", "BRT", "Maxent")) %>%
  mutate(peso = AUC / sum(AUC))

predicoes$ensemble_ponderado <-
  predicoes$glm    * pesos$peso[pesos$modelo == "GLM"] +
  predicoes$gam    * pesos$peso[pesos$modelo == "GAM"] +
  predicoes$rf     * pesos$peso[pesos$modelo == "Random Forest"] +
  predicoes$brt    * pesos$peso[pesos$modelo == "BRT"] +
  predicoes$maxent * pesos$peso[pesos$modelo == "Maxent"]

write_csv(
  pesos,
  "tabelas/unidade11/pesos_auc_ensemble.csv"
)

write_csv(
  predicoes,
  "dados/unidade11/processados/predicoes_modelos_ensemble_atualizado.csv"
)

r <- terra::rast(
  ncols = 120,
  nrows = 100,
  xmin = -80,
  xmax = -40,
  ymin = -30,
  ymax = 10,
  crs = "EPSG:4326"
)

terra::values(r) <- predicoes$ensemble_ponderado

terra::writeRaster(
  r,
  "resultados/unidade11/ensemble_ponderado_auc.tif",
  overwrite = TRUE
)

png(
  "figuras/unidade11/unidade11_ensemble_ponderado.png",
  width = 1800,
  height = 1400,
  res = 220
)

plot(r, main = "Ensemble ponderado por AUC")

points(
  dados_modelo$lon[dados_modelo$pa == 1],
  dados_modelo$lat[dados_modelo$pa == 1],
  pch = 16,
  cex = 0.4
)

dev.off()

message("Avaliação e ensemble ponderado concluídos.")
print(avaliacao)
