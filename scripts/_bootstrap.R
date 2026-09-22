# ============================================================
# Livro: Modelagem de Distribuição de Espécies em R
# Script: _bootstrap.R
# Objetivo: Validar a raiz do projeto e oferecer caminhos portáveis.
# Entrada: Estrutura de diretórios do projeto.
# Saída: Funções auxiliares; nenhuma saída analítica.
# Dependências: R >= 4.1; pacote here.
# Autor: Robson Borges de Lima
# ============================================================

if (!requireNamespace("here", quietly = TRUE)) {
  stop(
    "O pacote 'here' é necessário. Instale-o uma vez com install.packages('here').",
    call. = FALSE
  )
}

project_path <- function(...) here::here(...)

assert_project_root <- function() {
  markers <- c(
    project_path("apostila_sdm_r.tex"),
    project_path("ueap-sdm.cls"),
    project_path("scripts")
  )
  missing <- markers[!file.exists(markers) & !dir.exists(markers)]
  if (length(missing) > 0L) {
    stop("Raiz do projeto inválida. Ausentes: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  invisible(TRUE)
}

assert_files_exist <- function(paths, context = "arquivos de entrada") {
  missing <- paths[!file.exists(paths)]
  if (length(missing) > 0L) {
    stop("Não foi possível localizar ", context, ":\n- ", paste(missing, collapse = "\n- "), call. = FALSE)
  }
  invisible(paths)
}

ensure_dirs <- function(paths) {
  invisible(lapply(paths, dir.create, recursive = TRUE, showWarnings = FALSE))
}

require_packages <- function(packages) {
  missing <- packages[
    !vapply(packages, requireNamespace, logical(1), quietly = TRUE)
  ]

  if (length(missing) > 0L) {
    stop(
      "Pacotes ausentes: ", paste(missing, collapse = ", "), ".\n",
      "Instale-os explicitamente antes de executar o pipeline. ",
      "Os scripts do livro não instalam dependências automaticamente.",
      call. = FALSE
    )
  }

  invisible(packages)
}

book_seed <- function(seed = 202606L) {
  if (length(seed) != 1L || is.na(seed)) {
    stop("A seed deve ser um único número inteiro não ausente.", call. = FALSE)
  }
  set.seed(as.integer(seed))
  invisible(as.integer(seed))
}

assert_project_root()
