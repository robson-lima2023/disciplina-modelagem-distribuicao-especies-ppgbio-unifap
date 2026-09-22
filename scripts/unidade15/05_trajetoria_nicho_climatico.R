source("scripts/_bootstrap.R")

# ============================================================
# Unidade 15 - Paleoclima e nicho climático passado
# Script 05: Trajetória do nicho climático
# ============================================================

pacotes <- c(
  "dplyr", "readr", "terra", "ggplot2",
  "tidyr", "tibble", "ggrepel", "patchwork"
)

require_packages(pacotes)
invisible(lapply(pacotes, library, character.only = TRUE))
dir.create("dados/unidade15/processados", recursive = TRUE, showWarnings = FALSE)
dir.create("figuras/unidade15", recursive = TRUE, showWarnings = FALSE)
dir.create("tabelas/unidade15", recursive = TRUE, showWarnings = FALSE)
dir.create("resultados/unidade15", recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# 1. Configurações
# ------------------------------------------------------------

periodos <- tibble::tibble(
  periodo = c("lig", "lgm", "holoceno_medio", "atual"),
  periodo_legenda = c(
    "Último Interglacial",
    "Último Máximo Glacial",
    "Holoceno Médio",
    "Atual"
  ),
  ordem = c(1, 2, 3, 4)
)

arquivo_atual <- "dados/unidade04/processados/variaveis_ambientais_selecionadas_bioma.tif"

if (!file.exists(arquivo_atual)) {
  candidatos <- list.files(
    "dados",
    pattern = "variaveis.*bioma.*\\.tif$|ambientais.*bioma.*\\.tif$",
    recursive = TRUE,
    full.names = TRUE,
    ignore.case = TRUE
  )
  if (length(candidatos) > 0) arquivo_atual <- candidatos[1]
}

arquivo_vars <- "resultados/unidade04/variaveis_selecionadas_final.csv"

if (!file.exists(arquivo_vars)) {
  arquivo_vars <- "resultados/unidade10/variaveis_maxent.csv"
}

if (!file.exists(arquivo_atual)) {
  stop("Raster ambiental atual não encontrado.")
}

if (!file.exists(arquivo_vars)) {
  stop("Lista de variáveis selecionadas não encontrada.")
}

n_amostras <- 5000

# ------------------------------------------------------------
# 2. Ler variáveis
# ------------------------------------------------------------

vars <- readr::read_csv(
  arquivo_vars,
  show_col_types = FALSE
)$variavel

r_atual <- terra::rast(arquivo_atual)

vars <- vars[vars %in% names(r_atual)]

if (length(vars) < 2) {
  stop("Número insuficiente de variáveis ambientais para PCA.")
}

# ------------------------------------------------------------
# 3. Carregar rasters atual e paleoclimáticos
# ------------------------------------------------------------

rasters <- list()

rasters[["atual"]] <- r_atual[[vars]]

for (p in c("lig", "lgm", "holoceno_medio")) {
  
  arquivo <- paste0(
    "dados/unidade15/processados/variaveis_paleoclimaticas_",
    p,
    ".tif"
  )
  
  if (!file.exists(arquivo)) {
    stop("Raster paleoclimático não encontrado para ", p, ". Execute o Script 02.")
  }
  
  r <- terra::rast(arquivo)
  
  vars_disp <- vars[vars %in% names(r)]
  
  if (length(vars_disp) != length(vars)) {
    stop(
      "Variáveis ausentes em ",
      p,
      ": ",
      paste(setdiff(vars, vars_disp), collapse = ", ")
    )
  }
  
  rasters[[p]] <- r[[vars]]
}

# ------------------------------------------------------------
# 4. Amostrar o espaço climático
# ------------------------------------------------------------

set.seed(123)

amostras <- lapply(names(rasters), function(nm) {
  
  df <- terra::spatSample(
    rasters[[nm]],
    size = n_amostras,
    method = "random",
    na.rm = TRUE,
    xy = FALSE,
    values = TRUE
  ) |>
    as.data.frame()
  
  df$periodo <- nm
  
  df
}) |>
  bind_rows()

amostras <- amostras |>
  left_join(
    periodos,
    by = "periodo"
  ) |>
  tidyr::drop_na(
    dplyr::all_of(vars)
  )

if (nrow(amostras) == 0) {
  stop("Amostras ambientais vazias após remover NA.")
}

# ------------------------------------------------------------
# 5. PCA do espaço climático
# ------------------------------------------------------------

amb <- amostras |>
  select(all_of(vars))

pca <- prcomp(
  amb,
  center = TRUE,
  scale. = TRUE
)

var_exp <- summary(pca)$importance[2, 1:2] * 100

scores <- as.data.frame(
  pca$x[, 1:2]
) |>
  bind_cols(
    amostras |>
      select(periodo, periodo_legenda, ordem)
  )

centroides <- scores |>
  group_by(
    periodo,
    periodo_legenda,
    ordem
  ) |>
  summarise(
    PC1 = mean(PC1, na.rm = TRUE),
    PC2 = mean(PC2, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(ordem)

loadings <- as.data.frame(
  pca$rotation[, 1:2]
) |>
  tibble::rownames_to_column("variavel") |>
  dplyr::mutate(
    comprimento = sqrt(PC1^2 + PC2^2)
  ) |>
  dplyr::arrange(
    dplyr::desc(comprimento)
  )

n_loadings <- min(8, nrow(loadings))

loadings <- loadings |>
  dplyr::slice_head(
    n = n_loadings
  )

readr::write_csv(
  scores,
  "dados/unidade15/processados/pca_scores_paleoclima.csv"
)

readr::write_csv(
  centroides,
  "tabelas/unidade15/centroides_nicho_climatico_paleoclima.csv"
)

readr::write_csv(
  loadings,
  "tabelas/unidade15/loadings_pca_nicho_climatico.csv"
)

saveRDS(
  pca,
  "resultados/unidade15/pca_nicho_climatico_paleoclima.rds"
)

# ------------------------------------------------------------
# 6. Figura PCA principal
# ------------------------------------------------------------

cores_periodos <- c(
  "Último Interglacial" = "#D7301F",
  "Último Máximo Glacial" = "#4575B4",
  "Holoceno Médio" = "#FDAE61",
  "Atual" = "#1A9850"
)

g_pca <- ggplot(
  scores,
  aes(
    x = PC1,
    y = PC2,
    color = periodo_legenda
  )
) +
  stat_ellipse(
    linewidth = 0.70,
    alpha = 0.80,
    level = 0.80
  ) +
  geom_point(
    alpha = 0.08,
    size = 0.35
  ) +
  geom_path(
    data = centroides,
    aes(
      x = PC1,
      y = PC2,
      group = 1
    ),
    inherit.aes = FALSE,
    linewidth = 1.0,
    color = "black",
    arrow = arrow(length = unit(0.22, "cm"))
  ) +
  geom_point(
    data = centroides,
    aes(
      x = PC1,
      y = PC2,
      fill = periodo_legenda
    ),
    inherit.aes = FALSE,
    shape = 21,
    color = "black",
    size = 4,
    stroke = 0.35
  ) +
  ggrepel::geom_text_repel(
    data = centroides,
    aes(
      x = PC1,
      y = PC2,
      label = periodo_legenda
    ),
    inherit.aes = FALSE,
    size = 3.3,
    fontface = "bold",
    max.overlaps = Inf
  ) +
  scale_color_manual(
    values = cores_periodos,
    name = "Período"
  ) +
  scale_fill_manual(
    values = cores_periodos,
    name = "Período"
  ) +
  labs(
    title = expression("Trajetória do nicho climático de " * italic("Dinizia excelsa")),
    subtitle = "Espaço climático atual e paleoclimático representado por PCA",
    x = paste0("PC1 (", round(var_exp[1], 1), "%)"),
    y = paste0("PC2 (", round(var_exp[2], 1), "%)")
  ) +
  theme_bw(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

ggsave(
  "figuras/unidade15/unidade15_pca_nicho_passado.png",
  plot = g_pca,
  width = 9,
  height = 7,
  dpi = 600
)

# ------------------------------------------------------------
# 7. Biplot dos principais gradientes ambientais
# ------------------------------------------------------------

escala_setas <- 4

g_biplot <- g_pca +
  geom_segment(
    data = loadings,
    aes(
      x = 0,
      y = 0,
      xend = PC1 * escala_setas,
      yend = PC2 * escala_setas
    ),
    inherit.aes = FALSE,
    arrow = arrow(length = unit(0.18, "cm")),
    color = "grey20",
    linewidth = 0.45
  ) +
  ggrepel::geom_text_repel(
    data = loadings,
    aes(
      x = PC1 * escala_setas,
      y = PC2 * escala_setas,
      label = variavel
    ),
    inherit.aes = FALSE,
    size = 3,
    color = "grey10",
    max.overlaps = Inf
  ) +
  labs(
    title = expression("Biplot climático da trajetória histórica de " * italic("Dinizia excelsa")),
    subtitle = "Setas indicam os principais gradientes ambientais associados aos eixos da PCA"
  )

ggsave(
  "figuras/unidade15/unidade15_biplot_nicho_passado.png",
  plot = g_biplot,
  width = 10,
  height = 7.5,
  dpi = 600
)

# ------------------------------------------------------------
# 8. Distância entre centroides
# ------------------------------------------------------------

dist_centroides <- as.matrix(
  dist(
    centroides |>
      select(PC1, PC2)
  )
)

rownames(dist_centroides) <- centroides$periodo_legenda
colnames(dist_centroides) <- centroides$periodo_legenda

dist_long <- as.data.frame(as.table(dist_centroides)) |>
  rename(
    periodo_1 = Var1,
    periodo_2 = Var2,
    distancia = Freq
  )

readr::write_csv(
  dist_long,
  "tabelas/unidade15/distancia_centroides_nicho_climatico.csv"
)

# ------------------------------------------------------------
# 9. Figura composta
# ------------------------------------------------------------

fig_composta <- g_pca + g_biplot +
  patchwork::plot_annotation(
    title = expression("Reconstrução da trajetória climática histórica de " * italic("Dinizia excelsa")),
    subtitle = "Análise multivariada dos espaços climáticos passado e presente"
  ) &
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 10)
  )

ggsave(
  "figuras/unidade15/unidade15_trajetoria_nicho_patchwork.png",
  plot = fig_composta,
  width = 17,
  height = 7.5,
  dpi = 600
)

message("Trajetória do nicho climático calculada com sucesso.")
print(centroides)
