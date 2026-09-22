source("scripts/_bootstrap.R")

# ============================================================
# Livro: Modelagem de Distribuição de Espécies em R
# Capítulo: 12 - Avaliação de modelos
# Script: 09_criar_folds_espaciais.R
# Objetivo: Criar folds geograficamente separados para validação espacial.
# Entrada: Base presença-background completa da Unidade 6.
# Saída: Tabela de folds, resumo, grade espacial e mapa diagnóstico.
# Dependências: dplyr, readr, sf, ggplot2
# Autor: Robson Borges de Lima
# ============================================================

pacotes <- c("dplyr", "readr", "sf", "ggplot2", "tidyr")
require_packages(pacotes)
invisible(lapply(pacotes, library, character.only = TRUE))

arquivo_dados <- "dados/unidade06/processados/dados_presenca_background_bioma_glm_dinizia.csv"
arquivo_saida <- "dados/unidade12/processados/folds_espaciais_dinizia.csv"

assert_files_exist(arquivo_dados, "base completa para validação espacial")
ensure_dirs(c("dados/unidade12/processados", "tabelas/unidade12", "resultados/unidade12", "figuras/unidade12"))

k_folds <- 5L
tamanho_bloco_km <- 300
seed_folds <- 20261209L

dados <- readr::read_csv(arquivo_dados, show_col_types = FALSE) |>
  dplyr::mutate(
    lon = as.numeric(lon),
    lat = as.numeric(lat),
    pa = as.integer(pa)
  ) |>
  dplyr::filter(
    is.finite(lon), is.finite(lat), pa %in% c(0L, 1L)
  ) |>
  dplyr::distinct(lon, lat, pa, .keep_all = TRUE) |>
  dplyr::mutate(id_registro = dplyr::row_number())

if (nrow(dados) == 0L || dplyr::n_distinct(dados$pa) < 2L) {
  stop("A base precisa conter presenças e background válidos.", call. = FALSE)
}

pontos <- sf::st_as_sf(dados, coords = c("lon", "lat"), crs = 4326, remove = FALSE) |>
  sf::st_transform(5880)

grade <- sf::st_make_grid(
  pontos,
  cellsize = tamanho_bloco_km * 1000,
  square = TRUE
) |>
  sf::st_as_sf() |>
  dplyr::mutate(id_bloco = dplyr::row_number())

pontos_grade <- sf::st_join(
  pontos,
  grade[, "id_bloco"],
  join = sf::st_within,
  left = FALSE
)

blocos_ocupados <- sort(unique(pontos_grade$id_bloco))
if (length(blocos_ocupados) < k_folds) {
  stop(
    "O tamanho de bloco produziu menos blocos ocupados que folds. ",
    "Reduza tamanho_bloco_km ou k_folds.",
    call. = FALSE
  )
}

book_seed(seed_folds)
ordem_blocos <- sample(blocos_ocupados, length(blocos_ocupados), replace = FALSE)
alocacao <- data.frame(
  id_bloco = ordem_blocos,
  fold = rep(seq_len(k_folds), length.out = length(ordem_blocos))
)

pontos_grade <- pontos_grade |>
  dplyr::left_join(alocacao, by = "id_bloco")

folds <- pontos_grade |>
  sf::st_drop_geometry() |>
  dplyr::select(id_registro, lon, lat, pa, tipo, id_bloco, fold, dplyr::everything()) |>
  dplyr::arrange(fold, dplyr::desc(pa), id_registro)

resumo <- folds |>
  dplyr::count(fold, pa, name = "n") |>
  tidyr::complete(fold = seq_len(k_folds), pa = 0:1, fill = list(n = 0L)) |>
  dplyr::mutate(classe = ifelse(pa == 1L, "presenca", "background"))

folds_invalidos <- resumo |>
  dplyr::filter(n == 0L) |>
  dplyr::pull(fold) |>
  unique()

if (length(folds_invalidos) > 0L) {
  stop(
    "Ha folds sem uma das classes: ", paste(folds_invalidos, collapse = ", "),
    ". Ajuste tamanho_bloco_km, k_folds ou seed_folds.",
    call. = FALSE
  )
}

grade_folds <- grade |>
  dplyr::inner_join(alocacao, by = "id_bloco") |>
  dplyr::mutate(fold = factor(fold))

mapa <- ggplot2::ggplot() +
  ggplot2::geom_sf(data = grade_folds, ggplot2::aes(fill = fold), color = "white", linewidth = 0.15) +
  ggplot2::geom_sf(data = pontos_grade, ggplot2::aes(shape = factor(pa)), size = 0.8, alpha = 0.7) +
  ggplot2::scale_fill_manual(values = c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2")) +
  ggplot2::scale_shape_manual(values = c("0" = 1, "1" = 16), name = "Classe", labels = c("Background", "Presenca")) +
  ggplot2::labs(
    title = "Folds espaciais para validacao de Dinizia excelsa",
    subtitle = paste(k_folds, "folds; blocos de", tamanho_bloco_km, "km"),
    fill = "Fold"
  ) +
  ggplot2::theme_minimal(base_size = 11)

readr::write_csv(folds, arquivo_saida)
readr::write_csv(resumo, "tabelas/unidade12/resumo_folds_espaciais.csv")
sf::st_write(grade_folds, "resultados/unidade12/folds_espaciais_grade.gpkg", delete_dsn = TRUE, quiet = TRUE)
ggplot2::ggsave("figuras/unidade12/unidade12_folds_espaciais.png", mapa, width = 9, height = 7, dpi = 320)

message("Folds espaciais criados. A dimensao de bloco deve ser justificada para a aplicacao final.")
