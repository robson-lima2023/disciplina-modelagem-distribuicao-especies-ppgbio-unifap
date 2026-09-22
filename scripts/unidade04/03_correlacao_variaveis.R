source("scripts/_bootstrap.R")

# ============================================================
# Unidade 4 - Multicolinearidade em SDM
# Script 03: Correlação entre variáveis ambientais
# ============================================================

# ------------------------------------------------------------
# 1. Pacotes
# ------------------------------------------------------------

pacotes <- c("dplyr", "readr", "ggplot2", "tidyr", "corrplot", "tibble")

require_packages(pacotes)
invisible(lapply(pacotes, library, character.only = TRUE))

# ------------------------------------------------------------
# 2. Diretório do projeto
# ------------------------------------------------------------
# ------------------------------------------------------------
# 3. Ler matriz ambiental do background
# ------------------------------------------------------------

arquivo_bg <- "dados/unidade04/processados/02_background_variaveis_ambientais.csv"

if (!file.exists(arquivo_bg)) {
  stop("Arquivo 02_background_variaveis_ambientais.csv não encontrado. Execute primeiro o Script 02 da Unidade 4.")
}

bg_vars <- read_csv(arquivo_bg, show_col_types = FALSE)

# ------------------------------------------------------------
# 4. Manter apenas variáveis numéricas
# ------------------------------------------------------------

bg_num <- bg_vars %>%
  dplyr::select(where(is.numeric)) %>%
  tidyr::drop_na()

if (ncol(bg_num) < 2) {
  stop("A matriz ambiental possui menos de duas variáveis numéricas para calcular correlação.")
}

# ------------------------------------------------------------
# 5. Calcular matriz de correlação
# ------------------------------------------------------------

cor_mat <- cor(
  bg_num,
  method = "pearson",
  use = "pairwise.complete.obs"
)

write.csv(
  cor_mat,
  "tabelas/unidade04/matriz_correlacao_variaveis.csv",
  row.names = TRUE
)

# ------------------------------------------------------------
# 6. Converter matriz para formato longo
# ------------------------------------------------------------

cor_long <- as.data.frame(cor_mat) %>%
  tibble::rownames_to_column("var1") %>%
  tidyr::pivot_longer(
    cols = -var1,
    names_to = "var2",
    values_to = "correlacao"
  ) %>%
  dplyr::mutate(
    correlacao_abs = abs(correlacao)
  )

write_csv(
  cor_long,
  "tabelas/unidade04/matriz_correlacao_variaveis_long.csv"
)

# ------------------------------------------------------------
# 7. Identificar pares altamente correlacionados
# ------------------------------------------------------------

limiar_cor <- 0.70

pares_altos <- cor_long %>%
  dplyr::filter(var1 != var2) %>%
  dplyr::mutate(
    par = paste(pmin(var1, var2), pmax(var1, var2), sep = " x ")
  ) %>%
  dplyr::distinct(par, .keep_all = TRUE) %>%
  dplyr::filter(correlacao_abs >= limiar_cor) %>%
  dplyr::arrange(desc(correlacao_abs)) %>%
  dplyr::select(
    par,
    var1,
    var2,
    correlacao,
    correlacao_abs
  )

write_csv(
  pares_altos,
  "tabelas/unidade04/pares_alta_correlacao.csv"
)

# ------------------------------------------------------------
# 8. Resumo da correlação
# ------------------------------------------------------------

resumo_correlacao <- tibble(
  criterio = c(
    "Número de variáveis ambientais avaliadas",
    "Número total de pares únicos avaliados",
    "Limiar adotado para alta correlação absoluta",
    "Número de pares com alta correlação absoluta"
  ),
  valor = c(
    ncol(bg_num),
    length(cor_mat[upper.tri(cor_mat)]),
    limiar_cor,
    nrow(pares_altos)
  )
)

write_csv(
  resumo_correlacao,
  "tabelas/unidade04/resumo_correlacao_variaveis.csv"
)

# ------------------------------------------------------------
# 9. Heatmap com ggplot2
# ------------------------------------------------------------

ordem_vars <- colnames(cor_mat)

cor_long_plot <- cor_long %>%
  dplyr::mutate(
    var1 = factor(var1, levels = ordem_vars),
    var2 = factor(var2, levels = ordem_vars)
  )

g_heat <- ggplot(
  cor_long_plot,
  aes(x = var1, y = var2, fill = correlacao)
) +
  geom_tile(color = "white", linewidth = 0.2) +
  geom_text(aes(label = round(correlacao, 2)), size = 2.4) +
  scale_fill_gradient2(
    limits = c(-1, 1),
    midpoint = 0,
    name = "r de Pearson"
  ) +
  coord_equal() +
  labs(
    title = "Matriz de correlação entre variáveis ambientais",
    subtitle = "Diagnóstico inicial de multicolinearidade com base no background ambiental da Amazônia",
    x = NULL,
    y = NULL
  ) +
  theme_bw(base_size = 10) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 9),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  )

ggsave(
  "figuras/unidade04/unidade04_matriz_correlacao.png",
  plot = g_heat,
  width = 10,
  height = 8,
  dpi = 600
)

# ------------------------------------------------------------
# 10. Corrplot
# ------------------------------------------------------------

png(
  "figuras/unidade04/unidade04_corrplot_correlacao.png",
  width = 2600,
  height = 2200,
  res = 300
)

corrplot::corrplot(
  cor_mat,
  method = "color",
  type = "upper",
  order = "hclust",
  tl.col = "black",
  tl.cex = 0.75,
  number.cex = 0.55,
  addCoef.col = "black",
  col = corrplot::COL2("RdBu", 200)
)

dev.off()

# ------------------------------------------------------------
# 11. Mensagens finais
# ------------------------------------------------------------

message("Correlação entre variáveis ambientais calculada com sucesso.")
message("Resumo da análise de correlação:")

print(resumo_correlacao)

message("Pares com |r| >= ", limiar_cor, ":")

print(pares_altos)
