# ============================================================
# Unidade 10 - Maxent em SDM
# Script 01: Simulação, ajuste e tuning com maxnet
# ============================================================

setwd("C:/Users/rblfl/OneDrive/Documentos/Playground/Species-Distribution-Modeling")


pacotes <- c(
  "dplyr",
  "readr",
  "ggplot2",
  "terra",
  "maxnet",
  "pROC",
  "purrr"
)

instalar <- pacotes[!pacotes %in% rownames(installed.packages())]
if (length(instalar) > 0) install.packages(instalar)

invisible(lapply(pacotes, library, character.only = TRUE))

dir.create("dados/unidade10/processados", recursive = TRUE, showWarnings = FALSE)
dir.create("figuras/unidade10", recursive = TRUE, showWarnings = FALSE)
dir.create("tabelas/unidade10", recursive = TRUE, showWarnings = FALSE)
dir.create("resultados/unidade10", recursive = TRUE, showWarnings = FALSE)

set.seed(123)

# ------------------------------------------------------------
# 1. Criar grade espacial simulada
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
# 3. Simular adequabilidade verdadeira
# ------------------------------------------------------------

amb <- amb %>%
  mutate(
    eta =
      -5 +
      2.9 * exp(-((temperatura - 25)^2) / (2 * 3^2)) +
      2.5 * exp(-((precipitacao - 1700)^2) / (2 * 350^2)) -
      0.0012 * elevacao +
      0.8 * exp(-((solo - 5.6)^2) / (2 * 0.35^2)),
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
  "dados/unidade10/processados/dados_maxent_presenca_background.csv"
)

# ------------------------------------------------------------
# 5. Separar treino e teste
# ------------------------------------------------------------

set.seed(456)

dados_modelo <- dados_modelo %>%
  mutate(id = row_number())

treino_ids <- dados_modelo %>%
  group_by(pa) %>%
  sample_frac(0.70) %>%
  pull(id)

dados_treino <- dados_modelo %>% filter(id %in% treino_ids)
dados_teste  <- dados_modelo %>% filter(!id %in% treino_ids)

preditores <- c("temperatura", "precipitacao", "elevacao", "solo")

x_treino <- dados_treino %>% select(all_of(preditores))
p_treino <- dados_treino$pa

x_teste <- dados_teste %>% select(all_of(preditores))
p_teste <- dados_teste$pa

# ------------------------------------------------------------
# 6. Tuning de feature classes e regularização
# ------------------------------------------------------------

grade_tuning <- expand.grid(
  feature_class = c("l", "lq", "lqh", "lqph"),
  regmult = c(0.5, 1, 2, 3),
  stringsAsFactors = FALSE
)

ajustar_maxnet <- function(feature_class, regmult) {
  
  formula_maxnet <- maxnet::maxnet.formula(
    p = p_treino,
    data = x_treino,
    classes = feature_class
  )
  
  modelo <- maxnet::maxnet(
    p = p_treino,
    data = x_treino,
    f = formula_maxnet,
    regmult = regmult
  )
  
  pred_teste <- predict(
    modelo,
    newdata = x_teste,
    type = "cloglog"
  )
  
  auc <- as.numeric(
    pROC::auc(
      pROC::roc(
        response = p_teste,
        predictor = pred_teste,
        quiet = TRUE
      )
    )
  )
  
  ncoef <- sum(modelo$betas != 0)
  
  data.frame(
    feature_class = feature_class,
    regmult = regmult,
    auc = auc,
    ncoef = ncoef
  )
}

resultados_tuning <- purrr::pmap_dfr(
  grade_tuning,
  ajustar_maxnet
) %>%
  arrange(desc(auc), ncoef)

write_csv(
  resultados_tuning,
  "tabelas/unidade10/tuning_maxent_maxnet.csv"
)

# ------------------------------------------------------------
# 7. Figura do tuning
# ------------------------------------------------------------

g_tuning <- ggplot(
  resultados_tuning,
  aes(
    x = factor(regmult),
    y = auc,
    group = feature_class
  )
) +
  geom_line(aes(linetype = feature_class)) +
  geom_point(aes(shape = feature_class), size = 2.5) +
  labs(
    title = "Tuning de Maxent via maxnet",
    subtitle = "Feature classes e multiplicador de regularização",
    x = "Regularization multiplier",
    y = "AUC de teste",
    linetype = "Feature class",
    shape = "Feature class"
  ) +
  theme_bw(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

ggsave(
  "figuras/unidade10/unidade10_tuning_maxent_auc.png",
  plot = g_tuning,
  width = 8,
  height = 5,
  dpi = 600
)

# ------------------------------------------------------------
# 8. Salvar objetos para scripts seguintes
# ------------------------------------------------------------

saveRDS(
  list(
    amb = amb,
    r_base = r_base,
    dados_modelo = dados_modelo,
    dados_treino = dados_treino,
    dados_teste = dados_teste,
    preditores = preditores,
    resultados_tuning = resultados_tuning
  ),
  "resultados/unidade10/objetos_base_maxent.rds"
)

message("Script 01 concluído.")
print(resultados_tuning)
