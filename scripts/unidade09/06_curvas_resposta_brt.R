source("scripts/_bootstrap.R")

# ============================================================
# Unidade 9 - BRT em SDM
# Script 06: Curvas de resposta parcial do BRT
# ============================================================

# ------------------------------------------------------------
# 1. Pacotes
# ------------------------------------------------------------

pacotes <- c(
  "dplyr", "readr", "ggplot2", "purrr",
  "gbm", "tibble", "tidyr", "patchwork", "stringr"
)

require_packages(pacotes)
invisible(lapply(pacotes, library, character.only = TRUE))
dir.create("dados/unidade09/processados", recursive = TRUE, showWarnings = FALSE)
dir.create("figuras/unidade09", recursive = TRUE, showWarnings = FALSE)
dir.create("tabelas/unidade09", recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# 2. Arquivos de entrada
# ------------------------------------------------------------

arquivo_modelo <- "resultados/unidade09/modelo_brt_dinizia.rds"
arquivo_parametros <- "tabelas/unidade09/parametros_brt.csv"
arquivo_dados <- "dados/unidade09/processados/dados_brt_dinizia.csv"
arquivo_vars <- "resultados/unidade09/variaveis_brt.csv"
arquivo_importancia <- "tabelas/unidade09/importancia_variaveis_brt.csv"

arquivos <- c(
  arquivo_modelo,
  arquivo_parametros,
  arquivo_dados,
  arquivo_vars
)

if (any(!file.exists(arquivos))) {
  stop(
    "Arquivos ausentes:\n",
    paste(arquivos[!file.exists(arquivos)], collapse = "\n")
  )
}

# ------------------------------------------------------------
# 3. Leitura dos dados
# ------------------------------------------------------------

modelo <- readRDS(arquivo_modelo)

parametros <- read_csv(
  arquivo_parametros,
  show_col_types = FALSE
)

melhor_iter <- as.numeric(
  parametros$valor[parametros$parametro == "best.trees"]
)

if (length(melhor_iter) == 0 || is.na(melhor_iter)) {
  stop("Número ótimo de árvores não encontrado em parametros_brt.csv.")
}

dados <- read_csv(
  arquivo_dados,
  show_col_types = FALSE
)

vars <- read_csv(
  arquivo_vars,
  show_col_types = FALSE
)$variavel |>
  unique()

vars <- vars[vars %in% names(dados)]

if (length(vars) < 2) {
  stop("Número insuficiente de variáveis ambientais para gerar curvas de resposta.")
}

# ------------------------------------------------------------
# 4. Importância das variáveis
# ------------------------------------------------------------

if (file.exists(arquivo_importancia)) {
  
  importancia <- read_csv(
    arquivo_importancia,
    show_col_types = FALSE
  )
  
  if (!"importancia_relativa" %in% names(importancia)) {
    importancia <- importancia |>
      mutate(
        importancia_relativa = importancia / max(importancia, na.rm = TRUE)
      )
  }
  
  ordem_vars <- importancia |>
    filter(variavel %in% vars) |>
    arrange(desc(importancia_relativa)) |>
    pull(variavel)
  
  ordem_vars <- c(
    ordem_vars,
    setdiff(vars, ordem_vars)
  )
  
} else {
  
  warning("Tabela de importância não encontrada. As curvas serão ordenadas pela ordem das variáveis.")
  
  importancia <- tibble(
    variavel = vars,
    importancia = NA_real_,
    importancia_relativa = NA_real_
  )
  
  ordem_vars <- vars
}

vars <- ordem_vars

# ------------------------------------------------------------
# 5. Rótulos ecológicos
# ------------------------------------------------------------

rotulos_bioclim <- c(
  bio1  = "BIO1 - Temperatura média anual",
  bio2  = "BIO2 - Amplitude térmica média diária",
  bio3  = "BIO3 - Isotermalidade",
  bio4  = "BIO4 - Sazonalidade da temperatura",
  bio5  = "BIO5 - Temperatura máxima do mês mais quente",
  bio6  = "BIO6 - Temperatura mínima do mês mais frio",
  bio7  = "BIO7 - Amplitude térmica anual",
  bio8  = "BIO8 - Temperatura média do trimestre mais úmido",
  bio9  = "BIO9 - Temperatura média do trimestre mais seco",
  bio10 = "BIO10 - Temperatura média do trimestre mais quente",
  bio11 = "BIO11 - Temperatura média do trimestre mais frio",
  bio12 = "BIO12 - Precipitação anual",
  bio13 = "BIO13 - Precipitação do mês mais úmido",
  bio14 = "BIO14 - Precipitação do mês mais seco",
  bio15 = "BIO15 - Sazonalidade da precipitação",
  bio16 = "BIO16 - Precipitação do trimestre mais úmido",
  bio17 = "BIO17 - Precipitação do trimestre mais seco",
  bio18 = "BIO18 - Precipitação do trimestre mais quente",
  bio19 = "BIO19 - Precipitação do trimestre mais frio"
)

nome_legivel <- function(x) {
  
  if (x %in% names(rotulos_bioclim)) {
    return(rotulos_bioclim[[x]])
  }
  
  x |>
    str_replace_all("_", " ") |>
    str_replace_all("bio", "BIO") |>
    str_to_sentence()
}

# ------------------------------------------------------------
# 6. Perfil ambiental médio
# ------------------------------------------------------------

base_media <- dados |>
  summarise(
    across(
      all_of(vars),
      mean,
      na.rm = TRUE
    )
  )

# ------------------------------------------------------------
# 7. Função para curva de resposta parcial do BRT
# ------------------------------------------------------------

gerar_curva_brt <- function(var) {
  
  valores <- dados[[var]]
  
  min_var <- quantile(
    valores,
    probs = 0.01,
    na.rm = TRUE
  )
  
  max_var <- quantile(
    valores,
    probs = 0.99,
    na.rm = TRUE
  )
  
  seq_var <- seq(
    min_var,
    max_var,
    length.out = 150
  )
  
  novo <- base_media[rep(1, length(seq_var)), ]
  novo[[var]] <- seq_var
  
  pred <- predict(
    modelo,
    newdata = novo,
    n.trees = melhor_iter,
    type = "response"
  )
  
  imp_rel <- importancia |>
    filter(variavel == var) |>
    pull(importancia_relativa)
  
  if (length(imp_rel) == 0) {
    imp_rel <- NA_real_
  }
  
  tibble(
    variavel = var,
    variavel_legenda = nome_legivel(var),
    valor = as.numeric(seq_var),
    adequabilidade = as.numeric(pred),
    importancia_relativa = imp_rel[1]
  )
}

# ------------------------------------------------------------
# 8. Gerar curvas
# ------------------------------------------------------------

curvas <- map_dfr(
  vars,
  gerar_curva_brt
) |>
  mutate(
    variavel = factor(
      variavel,
      levels = vars
    )
  )

write_csv(
  curvas,
  "dados/unidade09/processados/curvas_resposta_brt.csv"
)

# ------------------------------------------------------------
# 9. Resumo quantitativo das curvas
# ------------------------------------------------------------

resumo_curvas <- curvas |>
  group_by(
    variavel,
    variavel_legenda
  ) |>
  summarise(
    n_pontos = n(),
    valor_min = min(valor, na.rm = TRUE),
    valor_max = max(valor, na.rm = TRUE),
    adequabilidade_min = min(adequabilidade, na.rm = TRUE),
    adequabilidade_max = max(adequabilidade, na.rm = TRUE),
    adequabilidade_media = mean(adequabilidade, na.rm = TRUE),
    amplitude_resposta = adequabilidade_max - adequabilidade_min,
    importancia_relativa = first(importancia_relativa),
    .groups = "drop"
  ) |>
  arrange(desc(importancia_relativa))

write_csv(
  resumo_curvas,
  "tabelas/unidade09/resumo_curvas_resposta_brt.csv"
)

# ------------------------------------------------------------
# 10. Gráficos individuais
# ------------------------------------------------------------

lista_graficos <- curvas |>
  group_split(variavel) |>
  map(function(df) {
    
    nome_var <- unique(df$variavel_legenda)
    var_atual <- as.character(unique(df$variavel))
    
    dados_rug <- dados |>
      select(all_of(var_atual)) |>
      rename(valor = all_of(var_atual)) |>
      drop_na()
    
    ggplot(
      df,
      aes(x = valor, y = adequabilidade)
    ) +
      geom_line(
        linewidth = 1.1,
        color = "black"
      ) +
      geom_hline(
        yintercept = mean(df$adequabilidade, na.rm = TRUE),
        linetype = "dashed",
        color = "grey40",
        linewidth = 0.35
      ) +
      geom_rug(
        data = dados_rug,
        aes(x = valor),
        inherit.aes = FALSE,
        sides = "b",
        alpha = 0.12
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
# 11. Combinar curvas com patchwork
# ------------------------------------------------------------

n_graficos <- length(lista_graficos)
n_col <- ifelse(n_graficos <= 4, 2, 3)
altura <- ifelse(n_graficos <= 6, 7, 9)

g_curvas <- patchwork::wrap_plots(
  lista_graficos,
  ncol = n_col
) +
  patchwork::plot_annotation(
    title = expression("Curvas de resposta parcial do BRT para " * italic("Dinizia excelsa")),
    subtitle = paste0(
      "Curvas ordenadas pela importância relativa das variáveis | best.trees = ",
      melhor_iter,
      " | n = ",
      n_graficos,
      " variáveis"
    ),
    theme = theme(
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 10)
    )
  )

# ------------------------------------------------------------
# 12. Salvar figura composta
# ------------------------------------------------------------

ggsave(
  "figuras/unidade09/unidade09_curvas_resposta_brt.png",
  plot = g_curvas,
  width = 12,
  height = altura,
  dpi = 600
)

# ------------------------------------------------------------
# 13. Salvar curvas individuais
# ------------------------------------------------------------

variaveis_unicas <- levels(curvas$variavel)

for (i in seq_along(lista_graficos)) {
  
  ggsave(
    filename = paste0(
      "figuras/unidade09/unidade09_curva_resposta_brt_",
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
# 14. Mensagem final
# ------------------------------------------------------------

message("Curvas de resposta parcial do BRT geradas com sucesso.")
message("Número de variáveis com curvas geradas: ", n_graficos)
message("Número ótimo de árvores: ", melhor_iter)
message("Figura salva em: figuras/unidade09/unidade09_curvas_resposta_brt.png")

print(resumo_curvas)
