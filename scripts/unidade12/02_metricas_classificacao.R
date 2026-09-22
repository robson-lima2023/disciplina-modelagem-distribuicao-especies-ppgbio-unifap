# ============================================================
# Unidade 12 - Avaliação de modelos em SDM
# Script 02: AUC, TSS, Kappa, sensibilidade e especificidade
# ============================================================

pacotes <- c("dplyr", "readr", "ggplot2", "pROC", "tidyr")

instalar <- pacotes[!pacotes %in% rownames(installed.packages())]
if (length(instalar) > 0) install.packages(instalar)

invisible(lapply(pacotes, library, character.only = TRUE))

dados <- read_csv(
  "dados/unidade12/processados/dados_avaliacao_modelos.csv",
  show_col_types = FALSE
)

modelos <- c("glm", "gam", "rf", "brt", "maxent", "ensemble")

calcular_metricas <- function(obs, pred, modelo) {
  
  auc_val <- as.numeric(
    pROC::auc(
      pROC::roc(
        response = obs,
        predictor = pred,
        quiet = TRUE
      )
    )
  )
  
  thresholds <- seq(0, 1, by = 0.01)
  
  tabela <- lapply(thresholds, function(th) {
    
    classe <- ifelse(pred >= th, 1, 0)
    
    vp <- sum(obs == 1 & classe == 1)
    vn <- sum(obs == 0 & classe == 0)
    fp <- sum(obs == 0 & classe == 1)
    fn <- sum(obs == 1 & classe == 0)
    
    sens <- ifelse((vp + fn) == 0, NA, vp / (vp + fn))
    spec <- ifelse((vn + fp) == 0, NA, vn / (vn + fp))
    tss <- sens + spec - 1
    
    po <- (vp + vn) / (vp + vn + fp + fn)
    
    p_yes_obs <- (vp + fn) / (vp + vn + fp + fn)
    p_yes_pred <- (vp + fp) / (vp + vn + fp + fn)
    p_no_obs <- (vn + fp) / (vp + vn + fp + fn)
    p_no_pred <- (vn + fn) / (vp + vn + fp + fn)
    
    pe <- p_yes_obs * p_yes_pred + p_no_obs * p_no_pred
    
    kappa <- ifelse((1 - pe) == 0, NA, (po - pe) / (1 - pe))
    
    data.frame(
      modelo = modelo,
      threshold = th,
      VP = vp,
      VN = vn,
      FP = fp,
      FN = fn,
      sensibilidade = sens,
      especificidade = spec,
      TSS = tss,
      Kappa = kappa
    )
  }) %>%
    bind_rows()
  
  melhor <- tabela %>%
    filter(!is.na(TSS)) %>%
    arrange(desc(TSS)) %>%
    slice(1) %>%
    mutate(AUC = auc_val)
  
  list(
    thresholds = tabela,
    melhor = melhor
  )
}

resultados <- lapply(modelos, function(m) {
  calcular_metricas(
    obs = dados$pa,
    pred = dados[[m]],
    modelo = m
  )
})

tabela_thresholds <- bind_rows(lapply(resultados, `[[`, "thresholds"))
tabela_metricas <- bind_rows(lapply(resultados, `[[`, "melhor"))

write_csv(
  tabela_thresholds,
  "tabelas/unidade12/metricas_por_threshold.csv"
)

write_csv(
  tabela_metricas,
  "tabelas/unidade12/metricas_modelos_melhor_tss.csv"
)

g_auc <- ggplot(
  tabela_metricas,
  aes(x = reorder(modelo, AUC), y = AUC)
) +
  geom_col() +
  coord_flip() +
  ylim(0, 1) +
  labs(
    title = "Comparação de AUC entre modelos",
    x = NULL,
    y = "AUC"
  ) +
  theme_bw(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

ggsave(
  "figuras/unidade12/unidade12_comparacao_auc.png",
  plot = g_auc,
  width = 8,
  height = 5,
  dpi = 600
)

g_tss <- ggplot(
  tabela_metricas,
  aes(x = reorder(modelo, TSS), y = TSS)
) +
  geom_col() +
  coord_flip() +
  ylim(0, 1) +
  labs(
    title = "Comparação de TSS máximo entre modelos",
    x = NULL,
    y = "TSS máximo"
  ) +
  theme_bw(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

ggsave(
  "figuras/unidade12/unidade12_comparacao_tss.png",
  plot = g_tss,
  width = 8,
  height = 5,
  dpi = 600
)

message("Métricas de avaliação calculadas.")
print(tabela_metricas)
