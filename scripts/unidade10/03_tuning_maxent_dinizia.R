source("scripts/_bootstrap.R")

# ============================================================
# Unidade 10 - Maxent em SDM
# Script 03: Tuning de feature classes e regularização
# ============================================================

pacotes <- c(
  "dplyr", "readr", "tidyr", "maxnet",
  "pROC", "ggplot2", "purrr", "tibble"
)

require_packages(pacotes)
invisible(lapply(pacotes, library, character.only = TRUE))
dir.create("figuras/unidade10", recursive = TRUE, showWarnings = FALSE)
dir.create("tabelas/unidade10", recursive = TRUE, showWarnings = FALSE)
dir.create("resultados/unidade10", recursive = TRUE, showWarnings = FALSE)

arquivo_treino <- "dados/unidade10/processados/treino_maxent_dinizia.csv"
arquivo_teste  <- "dados/unidade10/processados/teste_maxent_dinizia.csv"
arquivo_vars   <- "resultados/unidade10/variaveis_maxent.csv"

arquivos <- c(arquivo_treino, arquivo_teste, arquivo_vars)

if (any(!file.exists(arquivos))) {
  stop(
    "Arquivos ausentes:\n",
    paste(arquivos[!file.exists(arquivos)], collapse = "\n")
  )
}

treino <- read_csv(arquivo_treino, show_col_types = FALSE)
teste  <- read_csv(arquivo_teste, show_col_types = FALSE)

vars <- read_csv(
  arquivo_vars,
  show_col_types = FALSE
)$variavel |>
  unique()

vars <- vars[vars %in% names(treino)]

if (length(vars) < 2) {
  stop("Número insuficiente de variáveis ambientais para tuning do MaxEnt.")
}

if (!"pa" %in% names(treino) || !"pa" %in% names(teste)) {
  stop("A coluna 'pa' precisa existir nos conjuntos de treino e teste.")
}

treino <- treino |>
  mutate(pa = as.numeric(pa)) |>
  drop_na(pa, all_of(vars))

teste <- teste |>
  mutate(pa = as.numeric(pa)) |>
  drop_na(pa, all_of(vars))

if (!all(treino$pa %in% c(0, 1)) || !all(teste$pa %in% c(0, 1))) {
  stop("A variável 'pa' deve conter apenas 0 = background e 1 = presença.")
}

if (length(unique(treino$pa)) < 2 || length(unique(teste$pa)) < 2) {
  stop("Treino e teste precisam conter presenças e background.")
}

x_treino <- treino |>
  select(all_of(vars)) |>
  as.data.frame()

x_teste <- teste |>
  select(all_of(vars)) |>
  as.data.frame()

for (v in vars) {
  x_treino[[v]] <- as.numeric(x_treino[[v]])
  x_teste[[v]] <- as.numeric(x_teste[[v]])
}

p_treino <- as.numeric(treino$pa)
p_teste  <- as.numeric(teste$pa)

# ------------------------------------------------------------
# Grade de tuning
# ------------------------------------------------------------

grade <- tidyr::expand_grid(
  fc = c("l", "lq", "lqh", "lqhp"),
  regmult = c(0.5, 1, 1.5, 2, 3, 4)
)

# ------------------------------------------------------------
# Função para ajustar e avaliar MaxEnt
# ------------------------------------------------------------

ajustar_avaliar <- function(fc, regmult) {
  
  resultado_erro <- tibble(
    fc = fc,
    regmult = regmult,
    AUC_treino = NA_real_,
    AUC_teste = NA_real_,
    delta_auc = NA_real_,
    n_coef = NA_integer_,
    score = NA_real_,
    status = "erro"
  )
  
  mod <- tryCatch(
    {
      f <- maxnet::maxnet.formula(
        p = p_treino,
        data = x_treino,
        classes = fc
      )
      
      maxnet::maxnet(
        p = p_treino,
        data = x_treino,
        f = f,
        regmult = regmult
      )
    },
    error = function(e) {
      message(
        "Erro no tuning MaxEnt | fc = ", fc,
        " | regmult = ", regmult,
        " | ", e$message
      )
      NULL
    }
  )
  
  if (is.null(mod)) {
    return(resultado_erro)
  }
  
  pred_treino <- tryCatch(
    as.numeric(
      predict(
        mod,
        newdata = x_treino,
        type = "cloglog"
      )
    ),
    error = function(e) rep(NA_real_, length(p_treino))
  )
  
  pred_teste <- tryCatch(
    as.numeric(
      predict(
        mod,
        newdata = x_teste,
        type = "cloglog"
      )
    ),
    error = function(e) rep(NA_real_, length(p_teste))
  )
  
  if (
    all(is.na(pred_treino)) ||
    all(is.na(pred_teste)) ||
    length(unique(pred_teste[!is.na(pred_teste)])) < 2
  ) {
    return(resultado_erro)
  }
  
  auc_treino <- as.numeric(
    pROC::auc(
      pROC::roc(
        response = p_treino,
        predictor = pred_treino,
        levels = c(0, 1),
        direction = "<",
        quiet = TRUE
      )
    )
  )
  
  auc_teste <- as.numeric(
    pROC::auc(
      pROC::roc(
        response = p_teste,
        predictor = pred_teste,
        levels = c(0, 1),
        direction = "<",
        quiet = TRUE
      )
    )
  )
  
  n_coef <- sum(mod$betas != 0)
  delta_auc <- auc_treino - auc_teste
  
  tibble(
    fc = fc,
    regmult = regmult,
    AUC_treino = auc_treino,
    AUC_teste = auc_teste,
    delta_auc = delta_auc,
    n_coef = n_coef,
    score = NA_real_,
    status = "ok"
  )
}

# ------------------------------------------------------------
# Rodar tuning
# ------------------------------------------------------------

lista_resultados <- vector("list", nrow(grade))

for (i in seq_len(nrow(grade))) {
  
  message(
    "Rodando MaxEnt tuning ",
    i, "/", nrow(grade),
    " | fc = ", grade$fc[i],
    " | regmult = ", grade$regmult[i]
  )
  
  lista_resultados[[i]] <- ajustar_avaliar(
    fc = grade$fc[i],
    regmult = grade$regmult[i]
  )
}

resultados_tuning <- bind_rows(lista_resultados)

resultados_validos <- resultados_tuning |>
  filter(
    status == "ok",
    !is.na(AUC_teste),
    !is.na(AUC_treino),
    !is.na(n_coef)
  )

if (nrow(resultados_validos) == 0) {
  stop("Nenhuma combinação de MaxEnt foi ajustada com sucesso.")
}

max_coef <- max(resultados_validos$n_coef, na.rm = TRUE)

resultados_tuning <- resultados_tuning |>
  mutate(
    score = if_else(
      status == "ok",
      AUC_teste -
        pmax(delta_auc, 0) * 0.50 -
        (n_coef / max_coef) * 0.02,
      NA_real_
    )
  ) |>
  arrange(desc(score), n_coef, regmult)

melhor <- resultados_tuning |>
  filter(status == "ok") |>
  slice(1)

write_csv(
  resultados_tuning,
  "tabelas/unidade10/tuning_maxent_dinizia.csv"
)

write_csv(
  melhor,
  "resultados/unidade10/melhor_tuning_maxent.csv"
)

resumo_tuning <- tibble(
  modelo = "MaxEnt",
  dominio_calibracao = "Bioma Amazônia",
  n_modelos_testados = nrow(resultados_tuning),
  n_modelos_validos = nrow(resultados_validos),
  melhor_fc = melhor$fc,
  melhor_regmult = melhor$regmult,
  melhor_auc_treino = melhor$AUC_treino,
  melhor_auc_teste = melhor$AUC_teste,
  melhor_delta_auc = melhor$delta_auc,
  melhor_n_coef = melhor$n_coef,
  melhor_score = melhor$score,
  n_variaveis = length(vars),
  n_treino = nrow(treino),
  n_teste = nrow(teste),
  n_presencas_treino = sum(treino$pa == 1),
  n_background_treino = sum(treino$pa == 0),
  n_presencas_teste = sum(teste$pa == 1),
  n_background_teste = sum(teste$pa == 0)
)

write_csv(
  resumo_tuning,
  "tabelas/unidade10/resumo_tuning_maxent.csv"
)

# ------------------------------------------------------------
# Gráfico do tuning
# ------------------------------------------------------------

g_auc <- ggplot(
  resultados_tuning |> filter(status == "ok"),
  aes(
    x = regmult,
    y = AUC_teste,
    color = fc,
    group = fc
  )
) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  geom_point(
    data = melhor,
    aes(x = regmult, y = AUC_teste),
    size = 4,
    shape = 21,
    fill = "white",
    color = "black"
  ) +
  labs(
    title = expression("Tuning do MaxEnt para " * italic("Dinizia excelsa")),
    subtitle = "Seleção baseada em AUC teste, penalização de sobreajuste e complexidade",
    x = "Regularization multiplier",
    y = "AUC no conjunto de teste",
    color = "Feature classes"
  ) +
  theme_bw(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave(
  "figuras/unidade10/unidade10_tuning_maxent.png",
  plot = g_auc,
  width = 8,
  height = 6,
  dpi = 600
)

g_score <- ggplot(
  resultados_tuning |> filter(status == "ok"),
  aes(
    x = regmult,
    y = score,
    color = fc,
    group = fc
  )
) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  geom_point(
    data = melhor,
    aes(x = regmult, y = score),
    size = 4,
    shape = 21,
    fill = "white",
    color = "black"
  ) +
  labs(
    title = "Score de seleção do MaxEnt",
    subtitle = "Score = AUC teste - penalização por sobreajuste - penalização por complexidade",
    x = "Regularization multiplier",
    y = "Score",
    color = "Feature classes"
  ) +
  theme_bw(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave(
  "figuras/unidade10/unidade10_tuning_score_maxent.png",
  plot = g_score,
  width = 8,
  height = 6,
  dpi = 600
)

message("Tuning MaxEnt concluído.")
message("Melhor feature class: ", melhor$fc)
message("Melhor regularization multiplier: ", melhor$regmult)
message("AUC teste: ", round(melhor$AUC_teste, 3))
message("Delta AUC treino-teste: ", round(melhor$delta_auc, 3))

print(melhor)

