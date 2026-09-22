source("scripts/_bootstrap.R")

# ============================================================
# Livro: Modelagem de Distribuição de Espécies em R
# Capítulo: 12 - Avaliação de modelos
# Script: 10_validacao_espacial_glm.R
# Objetivo: Reajustar e avaliar o GLM dentro de cada fold espacial.
# Entrada: Folds espaciais e variáveis selecionadas na Unidade 4.
# Saída: Predições e métricas independentes por fold.
# Dependências: dplyr, readr, purrr, pROC, ggplot2
# Autor: Robson Borges de Lima
# ============================================================

pacotes <- c("dplyr", "readr", "purrr", "pROC", "ggplot2", "tidyr")
require_packages(pacotes)
invisible(lapply(pacotes, library, character.only = TRUE))

arquivo_folds <- "dados/unidade12/processados/folds_espaciais_dinizia.csv"
arquivo_vars <- "dados/unidade04/processados/variaveis_selecionadas_final.csv"
assert_files_exist(c(arquivo_folds, arquivo_vars), "entradas da validação espacial")

folds <- readr::read_csv(arquivo_folds, show_col_types = FALSE)
vars_tab <- readr::read_csv(arquivo_vars, show_col_types = FALSE)

if (!"variavel" %in% names(vars_tab)) {
  stop("A tabela de variáveis selecionadas precisa conter a coluna 'variavel'.", call. = FALSE)
}

vars <- intersect(unique(vars_tab$variavel), names(folds))
if (length(vars) < 2L) {
  stop("Menos de duas variáveis selecionadas estão presentes nos folds.", call. = FALSE)
}

metricas_limiar <- function(obs, pred, threshold) {
  bin <- as.integer(pred >= threshold)
  tp <- sum(bin == 1L & obs == 1L)
  tn <- sum(bin == 0L & obs == 0L)
  fp <- sum(bin == 1L & obs == 0L)
  fn <- sum(bin == 0L & obs == 1L)
  sens <- if ((tp + fn) > 0L) tp / (tp + fn) else NA_real_
  esp <- if ((tn + fp) > 0L) tn / (tn + fp) else NA_real_
  data.frame(threshold = threshold, sensibilidade = sens, especificidade = esp, TSS = sens + esp - 1)
}

avaliar_fold <- function(fold_teste) {
  treino <- folds |> dplyr::filter(fold != fold_teste)
  teste <- folds |> dplyr::filter(fold == fold_teste)

  if (dplyr::n_distinct(treino$pa) < 2L || dplyr::n_distinct(teste$pa) < 2L) {
    stop("Fold ", fold_teste, " não contém as duas classes.", call. = FALSE)
  }

  medias <- vapply(treino[vars], mean, numeric(1), na.rm = TRUE)
  desvios <- vapply(treino[vars], stats::sd, numeric(1), na.rm = TRUE)
  vars_validas <- vars[is.finite(desvios) & desvios > 0]
  if (length(vars_validas) < 2L) stop("Fold ", fold_teste, " sem preditores suficientes.", call. = FALSE)

  for (v in vars_validas) {
    nome_z <- paste0(v, "_z")
    treino[[nome_z]] <- (treino[[v]] - medias[[v]]) / desvios[[v]]
    teste[[nome_z]] <- (teste[[v]] - medias[[v]]) / desvios[[v]]
  }

  vars_z <- paste0(vars_validas, "_z")
  termos <- c(paste0("`", vars_z, "`"), paste0("I(`", vars_z, "`^2)"))
  formula_glm <- stats::as.formula(paste("pa ~", paste(termos, collapse = " + ")))

  modelo <- stats::glm(
    formula_glm,
    data = treino,
    family = stats::binomial(link = "logit"),
    control = stats::glm.control(maxit = 100)
  )

  pred <- stats::predict(modelo, newdata = teste, type = "response")
  validos <- is.finite(pred) & !is.na(teste$pa)
  teste <- teste[validos, , drop = FALSE]
  pred <- pmin(pmax(pred[validos], 0), 1)

  roc_obj <- pROC::roc(teste$pa, pred, levels = c(0, 1), direction = "<", quiet = TRUE)
  thresholds <- seq(max(0.001, min(pred)), min(0.999, max(pred)), length.out = 200)
  tabela_threshold <- dplyr::bind_rows(lapply(thresholds, function(x) metricas_limiar(teste$pa, pred, x)))
  melhor <- tabela_threshold |> dplyr::arrange(dplyr::desc(TSS)) |> dplyr::slice(1)
  convergiu <- isTRUE(modelo$converged)

  list(
    predicoes = teste |>
      dplyr::transmute(id_registro, lon, lat, pa, fold = fold_teste, pred_glm_espacial = pred),
    metricas = dplyr::tibble(
      modelo = "GLM",
      estrategia = "blocos_espaciais",
      fold = fold_teste,
      AUC = as.numeric(pROC::auc(roc_obj)),
      TSS = melhor$TSS,
      limiar_TSS = melhor$threshold,
      Sensibilidade = melhor$sensibilidade,
      Especificidade = melhor$especificidade,
      Brier = mean((teste$pa - pred)^2),
      n_treino = nrow(treino),
      n_teste = nrow(teste),
      presencas_teste = sum(teste$pa == 1L),
      background_teste = sum(teste$pa == 0L),
      convergiu = convergiu,
      n_predicoes_extremas = sum(pred <= 1e-6 | pred >= 1 - 1e-6)
    )
  )
}

resultados <- lapply(sort(unique(folds$fold)), avaliar_fold)
predicoes <- dplyr::bind_rows(lapply(resultados, `[[`, "predicoes"))
metricas <- dplyr::bind_rows(lapply(resultados, `[[`, "metricas"))

resumo <- metricas |>
  dplyr::summarise(
    dplyr::across(
      c(AUC, TSS, Sensibilidade, Especificidade, Brier),
      list(media = ~ mean(.x, na.rm = TRUE), sd = ~ stats::sd(.x, na.rm = TRUE))
    )
  )

metricas_long <- metricas |>
  tidyr::pivot_longer(c(AUC, TSS, Sensibilidade, Especificidade), names_to = "metrica", values_to = "valor")

figura <- ggplot2::ggplot(metricas_long, ggplot2::aes(metrica, valor)) +
  ggplot2::geom_boxplot(fill = "#DDEBF7", color = "#174A7E", width = 0.55) +
  ggplot2::geom_jitter(width = 0.08, height = 0, size = 2, color = "#174A7E") +
  ggplot2::coord_cartesian(ylim = c(0, 1)) +
  ggplot2::labs(title = "Desempenho do GLM sob validacao espacial", subtitle = "Cada ponto representa um fold espacial", x = NULL, y = "Valor") +
  ggplot2::theme_minimal(base_size = 11)

readr::write_csv(predicoes, "dados/unidade12/processados/predicoes_validacao_espacial_glm.csv")
readr::write_csv(metricas, "tabelas/unidade12/metricas_validacao_espacial_glm_por_fold.csv")
readr::write_csv(resumo, "tabelas/unidade12/resumo_validacao_espacial_glm.csv")
ggplot2::ggsave("figuras/unidade12/unidade12_validacao_espacial_glm.png", figura, width = 8, height = 5.5, dpi = 320)

message("Validacao espacial do GLM concluida com reajuste independente em cada fold.")
