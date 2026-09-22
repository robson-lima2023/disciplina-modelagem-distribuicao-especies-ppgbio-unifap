source("scripts/_bootstrap.R")

# ============================================================
# Unidade 10 - Maxent em SDM
# Script 07: Curvas de resposta parcial do MaxEnt
# ============================================================

pacotes <- c(
  "dplyr", "readr", "ggplot2", "purrr",
  "maxnet", "tibble", "tidyr",
  "patchwork", "stringr"
)

require_packages(pacotes)
invisible(lapply(pacotes, library, character.only = TRUE))
dir.create("dados/unidade10/processados", recursive = TRUE, showWarnings = FALSE)
dir.create("figuras/unidade10", recursive = TRUE, showWarnings = FALSE)
dir.create("tabelas/unidade10", recursive = TRUE, showWarnings = FALSE)

arquivo_modelo <- "resultados/unidade10/modelo_maxent_dinizia.rds"
arquivo_dados <- "dados/unidade10/processados/dados_maxent_dinizia.csv"
arquivo_vars <- "resultados/unidade10/variaveis_maxent.csv"
arquivo_importancia <- "tabelas/unidade10/importancia_variaveis_maxent.csv"

arquivos <- c(arquivo_modelo, arquivo_dados, arquivo_vars)

if (any(!file.exists(arquivos))) {
  stop(
    "Arquivos ausentes:\n",
    paste(arquivos[!file.exists(arquivos)], collapse = "\n")
  )
}

modelo <- readRDS(arquivo_modelo)

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
# Importância das variáveis
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
# Rótulos ecológicos
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
# Perfil ambiental médio
# ------------------------------------------------------------

base_media <- dados |>
  summarise(
    across(
      all_of(vars),
      mean,
      na.rm = TRUE
    )
  ) |>
  as.data.frame()

# ------------------------------------------------------------
# Curvas de resposta
# ------------------------------------------------------------

gerar_curva_maxent <- function(var) {
  
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
  
  novo <- base_media[rep(1, length(seq_var)), , drop = FALSE]
  novo[[var]] <- as.numeric(seq_var)
  
  for (v in vars) {
    novo[[v]] <- as.numeric(novo[[v]])
  }
  
  pred <- predict(
    modelo,
    newdata = novo[, vars, drop = FALSE],
    type = "cloglog",
    clamp = FALSE
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

curvas <- map_dfr(
  vars,
  gerar_curva_maxent
) |>
  mutate(
    variavel = factor(
      variavel,
      levels = vars
    )
  )

write_csv(
  curvas,
  "dados/unidade10/processados/curvas_resposta_maxent.csv"
)

# ------------------------------------------------------------
# Resumo quantitativo das curvas + importância das variáveis
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
    .groups = "drop"
  ) |>
  mutate(
    importancia_relativa = ifelse(
      sum(amplitude_resposta, na.rm = TRUE) > 0,
      100 * amplitude_resposta / sum(amplitude_resposta, na.rm = TRUE),
      0
    )
  ) |>
  arrange(desc(importancia_relativa))

write_csv(
  resumo_curvas,
  "tabelas/unidade10/resumo_curvas_resposta_maxent.csv"
)

write_csv(
  resumo_curvas |>
    select(
      variavel,
      variavel_legenda,
      importancia = amplitude_resposta,
      importancia_relativa
    ),
  "tabelas/unidade10/importancia_variaveis_maxent.csv"
)

# ------------------------------------------------------------
# Figura de importância das variáveis baseada nas curvas
# ------------------------------------------------------------

g_importancia <- ggplot(
  resumo_curvas,
  aes(
    x = reorder(variavel_legenda, importancia_relativa),
    y = importancia_relativa
  )
) +
  geom_col(
    fill = "grey55",
    color = "grey25",
    linewidth = 0.15
  ) +
  coord_flip() +
  labs(
    title = expression("Importância das variáveis no MaxEnt para " * italic("Dinizia excelsa")),
    subtitle = "Importância estimada pela amplitude das curvas de resposta parcial",
    x = NULL,
    y = "Importância relativa (%)"
  ) +
  scale_y_continuous(
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.05))
  ) +
  theme_bw(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )

ggsave(
  "figuras/unidade10/unidade10_importancia_variaveis_maxent.png",
  plot = g_importancia,
  width = 8.5,
  height = 6,
  dpi = 600
)

# ------------------------------------------------------------
# Gráficos individuais
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
# Figura composta
# ------------------------------------------------------------

n_graficos <- length(lista_graficos)
n_col <- ifelse(n_graficos <= 4, 2, 3)
altura <- ifelse(n_graficos <= 6, 7, 9)

g_curvas <- patchwork::wrap_plots(
  lista_graficos,
  ncol = n_col
) +
  patchwork::plot_annotation(
    title = expression("Curvas de resposta parcial do MaxEnt para " * italic("Dinizia excelsa")),
    subtitle = paste0(
      "Curvas ordenadas pela importância relativa das variáveis | n = ",
      n_graficos,
      " variáveis"
    ),
    theme = theme(
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 10)
    )
  )

ggsave(
  "figuras/unidade10/unidade10_curvas_resposta_maxent.png",
  plot = g_curvas,
  width = 12,
  height = altura,
  dpi = 600
)

# ------------------------------------------------------------
# Salvar curvas individuais
# ------------------------------------------------------------

variaveis_unicas <- levels(curvas$variavel)

for (i in seq_along(lista_graficos)) {
  
  ggsave(
    filename = paste0(
      "figuras/unidade10/unidade10_curva_resposta_maxent_",
      variaveis_unicas[i],
      ".png"
    ),
    plot = lista_graficos[[i]],
    width = 5,
    height = 4,
    dpi = 600
  )
}

message("Curvas de resposta parcial do MaxEnt geradas com sucesso.")
message("Número de variáveis com curvas geradas: ", n_graficos)
message("Figura salva em: figuras/unidade10/unidade10_curvas_resposta_maxent.png")

print(resumo_curvas)

