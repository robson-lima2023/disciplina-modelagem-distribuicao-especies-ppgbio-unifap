source("scripts/_bootstrap.R")

# ============================================================
# Unidade 4 - Multicolinearidade em SDM
# Script 06: PCA das variáveis ambientais
# ============================================================

pacotes <- c("dplyr", "readr", "ggplot2", "factoextra", "tibble", "tidyr")

require_packages(pacotes)
invisible(lapply(pacotes, library, character.only = TRUE))

bg_vars <- read_csv(
  "dados/unidade04/processados/02_background_variaveis_ambientais.csv",
  show_col_types = FALSE
)

set.seed(123)

bg_pca <- bg_vars %>%
  sample_n(size = min(10000, nrow(bg_vars))) %>%
  drop_na()

pca <- prcomp(
  bg_pca,
  center = TRUE,
  scale. = TRUE
)

variancia <- tibble(
  componente = paste0("PC", seq_along(pca$sdev)),
  proporcao = (pca$sdev^2) / sum(pca$sdev^2),
  acumulada = cumsum(proporcao)
)

write_csv(
  variancia,
  "tabelas/unidade04/pca_variancia_explicada.csv"
)

loadings <- as.data.frame(pca$rotation) %>%
  rownames_to_column("variavel")

write_csv(
  loadings,
  "tabelas/unidade04/pca_loadings_variaveis.csv"
)

g_biplot <- factoextra::fviz_pca_biplot(
  pca,
  repel = TRUE,
  col.var = "black",
  col.ind = "grey70",
  alpha.ind = 0.25,
  title = "PCA das variáveis ambientais"
) +
  theme_bw(base_size = 11) +
  theme(plot.title = element_text(face = "bold"))

ggsave(
  "figuras/unidade04/unidade04_pca_biplot_variaveis.png",
  plot = g_biplot,
  width = 8,
  height = 6,
  dpi = 600
)

g_var <- ggplot(
  variancia,
  aes(x = componente, y = proporcao)
) +
  geom_col() +
  geom_line(aes(y = acumulada, group = 1), linewidth = 0.8) +
  geom_point(aes(y = acumulada), size = 2) +
  labs(
    title = "Variância explicada pela PCA",
    subtitle = "Barras = proporção por componente; linha = variância acumulada",
    x = "Componente principal",
    y = "Proporção da variância"
  ) +
  theme_bw(base_size = 11) +
  theme(plot.title = element_text(face = "bold"))

ggsave(
  "figuras/unidade04/unidade04_pca_variancia.png",
  plot = g_var,
  width = 8,
  height = 5,
  dpi = 600
)

saveRDS(
  pca,
  "resultados/unidade04/pca_variaveis_ambientais.rds"
)

message("PCA das variáveis ambientais concluída.")
print(variancia)
