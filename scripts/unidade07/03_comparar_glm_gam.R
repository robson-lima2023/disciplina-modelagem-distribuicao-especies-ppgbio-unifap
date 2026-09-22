# ============================================================
# Unidade 7 - GAM em SDM
# Script 03: Comparação didática GLM versus GAM
# ============================================================

pacotes <- c("dplyr", "readr", "ggplot2", "mgcv")

instalar <- pacotes[!pacotes %in% rownames(installed.packages())]
if (length(instalar) > 0) install.packages(instalar)

invisible(lapply(pacotes, library, character.only = TRUE))

dir.create("figuras/unidade07", recursive = TRUE, showWarnings = FALSE)
dir.create("tabelas/unidade07", recursive = TRUE, showWarnings = FALSE)

dados <- read_csv(
  "dados/unidade07/processados/dados_gam_presenca_background.csv",
  show_col_types = FALSE
)

modelo_glm <- glm(
  pa ~ temperatura + I(temperatura^2) +
    precipitacao + I(precipitacao^2) +
    elevacao,
  data = dados,
  family = binomial(link = "logit")
)

modelo_gam <- readRDS("resultados/unidade07/modelo_gam.rds")

dados$pred_glm <- predict(modelo_glm, newdata = dados, type = "response")
dados$pred_gam <- predict(modelo_gam, newdata = dados, type = "response")

comparacao <- data.frame(
  modelo = c("GLM quadrático", "GAM"),
  AIC = c(AIC(modelo_glm), AIC(modelo_gam))
)

write_csv(
  comparacao,
  "tabelas/unidade07/comparacao_aic_glm_gam.csv"
)

amostra <- dados %>%
  slice_sample(n = 1000)

g_comp <- ggplot(
  amostra,
  aes(x = pred_glm, y = pred_gam)
) +
  geom_point(alpha = 0.45) +
  geom_abline(
    intercept = 0,
    slope = 1,
    linetype = "dashed"
  ) +
  labs(
    title = "Comparação entre predições GLM e GAM",
    subtitle = "Cada ponto representa uma célula/ponto amostrado",
    x = "Predição GLM",
    y = "Predição GAM"
  ) +
  theme_bw(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

ggsave(
  "figuras/unidade07/unidade07_comparacao_glm_gam.png",
  plot = g_comp,
  width = 7,
  height = 6,
  dpi = 600
)

message("Comparação GLM versus GAM concluída.")
print(comparacao)
