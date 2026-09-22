source("scripts/_bootstrap.R")

# ============================================================
# Unidade 7 - GAM em SDM
# Script 06: Curvas de resposta parcial do GAM
# Autor: Robson Borges de Lima
# ============================================================

# ------------------------------------------------------------
# 1. Pacotes
# ------------------------------------------------------------

pacotes <- c(
  "dplyr", "readr", "ggplot2", "tidyr",
  "purrr", "mgcv", "patchwork", "stringr", "tibble"
)

require_packages(pacotes)
invisible(lapply(pacotes, library, character.only = TRUE))

# ------------------------------------------------------------
# 2. Diretório raiz do projeto
# ------------------------------------------------------------
# ------------------------------------------------------------
# 3. Diretórios de saída
# ------------------------------------------------------------

dir.create("dados/unidade07/processados", recursive = TRUE, showWarnings = FALSE)
dir.create("figuras/unidade07", recursive = TRUE, showWarnings = FALSE)
dir.create("tabelas/unidade07", recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# 4. Arquivos de entrada
# ------------------------------------------------------------

arquivo_modelo <- "resultados/unidade07/modelo_gam_dinizia.rds"
arquivo_dados <- "dados/unidade07/processados/dados_gam_dinizia_padronizados.csv"
arquivo_parametros <- "resultados/unidade07/parametros_padronizacao_gam.csv"

if (!file.exists(arquivo_modelo)) {
  stop("Modelo GAM não encontrado. Execute o Script 03 da Unidade 7.")
}

if (!file.exists(arquivo_dados)) {
  stop("Dados padronizados não encontrados. Execute o Script 02 da Unidade 7.")
}

if (!file.exists(arquivo_parametros)) {
  stop("Parâmetros de padronização não encontrados. Execute o Script 02 da Unidade 7.")
}

# ------------------------------------------------------------
# 5. Leitura dos dados
# ------------------------------------------------------------

modelo <- readRDS(arquivo_modelo)

dados <- read_csv(
  arquivo_dados,
  show_col_types = FALSE
)

parametros <- read_csv(
  arquivo_parametros,
  show_col_types = FALSE
)

# ------------------------------------------------------------
# 6. Checagem dos parâmetros
# ------------------------------------------------------------

colunas_param <- c("variavel", "media_treino", "sd_treino", "variavel_z")

if (!all(colunas_param %in% names(parametros))) {
  stop(
    "O arquivo de parâmetros precisa conter as colunas: ",
    paste(colunas_param, collapse = ", ")
  )
}

parametros <- parametros |>
  distinct(variavel, variavel_z, .keep_all = TRUE) |>
  filter(
    variavel %in% names(dados),
    variavel_z %in% names(dados)
  )

if (nrow(parametros) < 2) {
  stop("Número insuficiente de variáveis para gerar curvas de resposta.")
}

if (any(is.na(parametros$sd_treino)) || any(parametros$sd_treino == 0)) {
  stop("Há variáveis com desvio-padrão inválido nos parâmetros de padronização.")
}

vars <- parametros$variavel
vars_z <- parametros$variavel_z

# ------------------------------------------------------------
# 7. Função auxiliar para nomes legíveis
# ------------------------------------------------------------

nome_legivel <- function(x) {
  x |>
    stringr::str_replace_all("_", " ") |>
    stringr::str_replace_all("bio", "BIO") |>
    stringr::str_to_sentence()
}

# ------------------------------------------------------------
# 8. Base ambiental média das variáveis padronizadas
# ------------------------------------------------------------

base_media <- dados |>
  summarise(
    across(
      all_of(vars_z),
      mean,
      na.rm = TRUE
    )
  )

# ------------------------------------------------------------
# 9. Função para curva de resposta parcial
# ------------------------------------------------------------

gerar_curva <- function(var_original, var_z) {
  
  valores <- dados[[var_original]]
  
  min_var <- stats::quantile(
    valores,
    probs = 0.01,
    na.rm = TRUE
  )
  
  max_var <- stats::quantile(
    valores,
    probs = 0.99,
    na.rm = TRUE
  )
  
  seq_original <- seq(
    min_var,
    max_var,
    length.out = 150
  )
  
  novo <- base_media[rep(1, length(seq_original)), ]
  
  media <- parametros$media_treino[parametros$variavel == var_original]
  sdv <- parametros$sd_treino[parametros$variavel == var_original]
  
  novo[[var_z]] <- (seq_original - media) / sdv
  
  pred <- predict(
    modelo,
    newdata = novo,
    type = "response"
  )
  
  tibble(
    variavel = var_original,
    variavel_z = var_z,
    variavel_legenda = nome_legivel(var_original),
    valor = as.numeric(seq_original),
    valor_z = as.numeric(novo[[var_z]]),
    adequabilidade = as.numeric(pred)
  )
}

# ------------------------------------------------------------
# 10. Gerar curvas
# ------------------------------------------------------------

curvas <- purrr::map2_dfr(
  vars,
  vars_z,
  gerar_curva
)

write_csv(
  curvas,
  "dados/unidade07/processados/curvas_resposta_gam.csv"
)

# ------------------------------------------------------------
# 11. Resumo das curvas
# ------------------------------------------------------------

resumo_curvas <- curvas |>
  group_by(variavel, variavel_z, variavel_legenda) |>
  summarise(
    n_pontos = n(),
    valor_min = min(valor, na.rm = TRUE),
    valor_max = max(valor, na.rm = TRUE),
    adequabilidade_min = min(adequabilidade, na.rm = TRUE),
    adequabilidade_max = max(adequabilidade, na.rm = TRUE),
    adequabilidade_media = mean(adequabilidade, na.rm = TRUE),
    .groups = "drop"
  )

write_csv(
  resumo_curvas,
  "tabelas/unidade07/resumo_curvas_resposta_gam.csv"
)

# ------------------------------------------------------------
# 12. Criar gráficos individuais
# ------------------------------------------------------------

lista_graficos <- curvas |>
  group_split(variavel) |>
  purrr::map(function(df) {
    
    nome_var <- unique(df$variavel_legenda)
    
    ggplot(
      df,
      aes(x = valor, y = adequabilidade)
    ) +
      geom_line(
        linewidth = 1.0,
        color = "black"
      ) +
      labs(
        title = nome_var,
        x = "Gradiente ambiental",
        y = "Adequabilidade"
      ) +
      scale_y_continuous(
        limits = c(0, 1),
        breaks = seq(0, 1, by = 0.25)
      ) +
      theme_bw(base_size = 10) +
      theme(
        plot.title = element_text(face = "bold", size = 10, hjust = 0.5),
        axis.title = element_text(size = 9),
        axis.text = element_text(size = 8),
        panel.grid.minor = element_blank()
      )
  })

# ------------------------------------------------------------
# 13. Combinar curvas com patchwork
# ------------------------------------------------------------

n_graficos <- length(lista_graficos)
n_col <- ifelse(n_graficos <= 4, 2, 3)
altura <- ifelse(n_graficos <= 6, 7, 9)

g_curvas <- patchwork::wrap_plots(
  lista_graficos,
  ncol = n_col
) +
  patchwork::plot_annotation(
    title = expression("Curvas de resposta parcial do GAM para " * italic("Dinizia excelsa")),
    subtitle = paste0(
      "Modelo calibrado com variáveis ambientais selecionadas por VIF | n = ",
      n_graficos,
      " variáveis"
    ),
    theme = theme(
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 10)
    )
  )

# ------------------------------------------------------------
# 14. Salvar figura composta
# ------------------------------------------------------------

ggsave(
  "figuras/unidade07/unidade07_curvas_resposta_gam.png",
  plot = g_curvas,
  width = 12,
  height = altura,
  dpi = 600
)

# ------------------------------------------------------------
# 15. Salvar curvas individuais
# ------------------------------------------------------------

variaveis_unicas <- curvas |>
  distinct(variavel) |>
  pull(variavel)

for (i in seq_along(lista_graficos)) {
  
  ggsave(
    filename = paste0(
      "figuras/unidade07/unidade07_curva_resposta_gam_",
      variaveis_unicas[i],
      ".png"
    ),
    plot = lista_graficos[[i]],
    width = 5,
    height = 4,
    dpi = 600
  )
}

# ------------------------------------------------------------
# 16. Mensagem final
# ------------------------------------------------------------

message("Curvas de resposta parcial do GAM geradas com sucesso.")
message("Número de variáveis com curvas geradas: ", n_graficos)
message("Figura salva em: figuras/unidade07/unidade07_curvas_resposta_gam.png")

print(resumo_curvas)
