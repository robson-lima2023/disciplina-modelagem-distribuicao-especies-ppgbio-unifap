source("scripts/_bootstrap.R")

# ============================================================
# Unidade 11 - Modelagem Ensemble em SDM
# Script 04: Ensemble ponderado por desempenho
# ============================================================

# ------------------------------------------------------------
# 1. Pacotes
# ------------------------------------------------------------

pacotes <- c(
  "dplyr",
  "readr",
  "terra",
  "sf",
  "ggplot2",
  "ggspatial",
  "viridis",
  "tibble",
  "tidyr"
)

require_packages(pacotes)
invisible(lapply(pacotes, library, character.only = TRUE))

sf::sf_use_s2(FALSE)

# ------------------------------------------------------------
# 2. Diretório raiz do projeto
# ------------------------------------------------------------
# ------------------------------------------------------------
# 3. Diretórios de saída
# ------------------------------------------------------------

dir.create("dados/unidade11/processados", recursive = TRUE, showWarnings = FALSE)
dir.create("figuras/unidade11", recursive = TRUE, showWarnings = FALSE)
dir.create("resultados/unidade11", recursive = TRUE, showWarnings = FALSE)
dir.create("tabelas/unidade11", recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# 4. Arquivos de entrada
# ------------------------------------------------------------

arquivo_stack <- "dados/unidade11/processados/stack_predicoes_modelos.tif"

arquivo_bioma <- "dados/unidade01/brutos/amazon_biome_border.shp"

arquivo_m <- "dados/unidade05/processados/area_m_dinizia.gpkg"

arquivo_oc <- "dados/unidade05/processados/presencas_area_m_dinizia.csv"

if (!file.exists(arquivo_stack)) {
  stop("Stack de predições não encontrado. Execute o Script 02 da Unidade 11.")
}

if (!file.exists(arquivo_bioma)) {
  stop("Limite do bioma Amazônia não encontrado: ", arquivo_bioma)
}

if (!file.exists(arquivo_oc)) {
  stop("Arquivo de ocorrências não encontrado: ", arquivo_oc)
}

if (!file.exists(arquivo_m)) {
  warning("Área M não encontrada. O mapa será gerado sem contorno da área M.")
}

# ------------------------------------------------------------
# 5. Leitura dos dados
# ------------------------------------------------------------

stack_modelos <- terra::rast(
  arquivo_stack
)

bioma_raw <- sf::st_read(
  arquivo_bioma,
  quiet = TRUE
)

oc <- read_csv(
  arquivo_oc,
  show_col_types = FALSE
)

area_m <- NULL

if (file.exists(arquivo_m)) {
  area_m <- sf::st_read(
    arquivo_m,
    quiet = TRUE
  )
}

if (terra::nlyr(stack_modelos) < 2) {
  stop("O stack deve conter pelo menos dois modelos para gerar ensemble ponderado.")
}

nomes_modelos <- names(stack_modelos)

# ------------------------------------------------------------
# 6. Função para ler AUC dos modelos
# ------------------------------------------------------------

ler_auc <- function(arquivo, nome_modelo) {
  
  if (!file.exists(arquivo)) {
    warning("Arquivo de avaliação não encontrado para ", nome_modelo, ": ", arquivo)
    return(
      tibble(
        modelo = nome_modelo,
        AUC = NA_real_,
        origem_auc = NA_character_
      )
    )
  }
  
  tab <- read_csv(
    arquivo,
    show_col_types = FALSE
  )
  
  candidatos_auc <- c(
    "AUC_teste_random_split",
    "AUC_teste",
    "AUC",
    "auc_teste",
    "auc"
  )
  
  col_auc <- candidatos_auc[candidatos_auc %in% names(tab)][1]
  
  if (is.na(col_auc)) {
    warning("Nenhuma coluna de AUC encontrada em: ", arquivo)
    return(
      tibble(
        modelo = nome_modelo,
        AUC = NA_real_,
        origem_auc = NA_character_
      )
    )
  }
  
  tibble(
    modelo = nome_modelo,
    AUC = as.numeric(tab[[col_auc]][1]),
    origem_auc = col_auc
  )
}

# ------------------------------------------------------------
# 7. Ler métricas dos modelos
# ------------------------------------------------------------

metricas <- bind_rows(
  ler_auc("tabelas/unidade06/avaliacao_glm_dinizia.csv", "GLM"),
  ler_auc("tabelas/unidade07/avaliacao_gam_dinizia.csv", "GAM"),
  ler_auc("tabelas/unidade08/avaliacao_rf_dinizia.csv", "RF"),
  ler_auc("tabelas/unidade09/avaliacao_brt_dinizia.csv", "BRT"),
  ler_auc("tabelas/unidade10/avaliacao_maxent_dinizia.csv", "MaxEnt")
) |>
  filter(modelo %in% nomes_modelos)

if (nrow(metricas) == 0) {
  stop("Nenhuma métrica de desempenho foi encontrada para os modelos do stack.")
}

# ------------------------------------------------------------
# 8. Corrigir AUCs ausentes e calcular pesos
# ------------------------------------------------------------

if (all(is.na(metricas$AUC))) {
  
  warning("Todos os AUCs estão ausentes. Serão usados pesos iguais.")
  
  metricas <- metricas |>
    mutate(
      AUC = 1,
      origem_auc = "peso_igual_por_auc_ausente"
    )
  
} else if (any(is.na(metricas$AUC))) {
  
  auc_medio <- mean(metricas$AUC, na.rm = TRUE)
  
  warning("Alguns AUCs estão ausentes. Valores ausentes serão substituídos pelo AUC médio.")
  
  metricas <- metricas |>
    mutate(
      AUC = ifelse(is.na(AUC), auc_medio, AUC),
      origem_auc = ifelse(
        is.na(origem_auc),
        "auc_medio_imputado",
        origem_auc
      )
    )
}

metricas <- metricas |>
  mutate(
    AUC = pmin(pmax(AUC, 0.5), 1),
    peso_bruto = pmax(AUC - 0.5, 0.001),
    peso = peso_bruto / sum(peso_bruto),
    interpretacao_peso = case_when(
      peso >= 0.30 ~ "Peso alto",
      peso >= 0.20 ~ "Peso intermediário",
      TRUE ~ "Peso baixo"
    )
  ) |>
  arrange(desc(peso))

write_csv(
  metricas,
  "tabelas/unidade11/pesos_ensemble_ponderado.csv"
)

# ------------------------------------------------------------
# 9. Calcular ensemble ponderado
# ------------------------------------------------------------

stack_ordenado <- stack_modelos[[metricas$modelo]]

pesos <- metricas$peso

ensemble_ponderado <- terra::app(
  stack_ordenado,
  fun = function(x) {
    stats::weighted.mean(
      x,
      w = pesos,
      na.rm = TRUE
    )
  }
)

ensemble_ponderado <- terra::clamp(
  ensemble_ponderado,
  lower = 0,
  upper = 1,
  values = TRUE
)

names(ensemble_ponderado) <- "ensemble_ponderado"

terra::writeRaster(
  ensemble_ponderado,
  "resultados/unidade11/ensemble_ponderado_dinizia.tif",
  overwrite = TRUE
)

# ------------------------------------------------------------
# 10. Converter ensemble para tabela
# ------------------------------------------------------------

df <- as.data.frame(
  ensemble_ponderado,
  xy = TRUE,
  na.rm = TRUE
) |>
  rename(
    lon = x,
    lat = y,
    adequabilidade = ensemble_ponderado
  )

write_csv(
  df,
  "dados/unidade11/processados/ensemble_ponderado_dinizia.csv"
)

# ------------------------------------------------------------
# 11. Resumo estatístico do ensemble ponderado
# ------------------------------------------------------------

resumo_ensemble <- tibble(
  produto = "Ensemble ponderado por desempenho",
  criterio_ponderacao = "Peso proporcional a AUC - 0.5",
  modelos_incluidos = paste(metricas$modelo, collapse = ", "),
  n_modelos = nrow(metricas),
  n_pixels = nrow(df),
  minimo = min(df$adequabilidade, na.rm = TRUE),
  primeiro_quartil = quantile(df$adequabilidade, 0.25, na.rm = TRUE),
  mediana = median(df$adequabilidade, na.rm = TRUE),
  media = mean(df$adequabilidade, na.rm = TRUE),
  terceiro_quartil = quantile(df$adequabilidade, 0.75, na.rm = TRUE),
  maximo = max(df$adequabilidade, na.rm = TRUE),
  desvio_padrao = sd(df$adequabilidade, na.rm = TRUE)
)

write_csv(
  resumo_ensemble,
  "tabelas/unidade11/resumo_ensemble_ponderado.csv"
)

# ------------------------------------------------------------
# 12. Preparar bioma, área M e ocorrências para mapa
# ------------------------------------------------------------

bioma_proj <- bioma_raw |>
  sf::st_transform(5880) |>
  sf::st_make_valid() |>
  sf::st_union() |>
  sf::st_as_sf() |>
  sf::st_make_valid()

bioma <- bioma_proj |>
  sf::st_transform(terra::crs(ensemble_ponderado)) |>
  sf::st_make_valid()

bioma_plot <- sf::st_transform(
  bioma,
  4326
)

area_m_plot <- NULL

if (!is.null(area_m)) {
  area_m_plot <- area_m |>
    sf::st_make_valid() |>
    sf::st_transform(4326)
}

if (!all(c("lon", "lat") %in% names(oc))) {
  stop("O arquivo de ocorrências precisa conter as colunas 'lon' e 'lat'.")
}

oc_sf <- sf::st_as_sf(
  oc,
  coords = c("lon", "lat"),
  crs = 4326,
  remove = FALSE
)

bbox_bioma <- sf::st_bbox(bioma_plot)

# ------------------------------------------------------------
# 13. Mapa do ensemble ponderado
# ------------------------------------------------------------

g <- ggplot() +
  geom_sf(
    data = bioma_plot,
    fill = "grey96",
    color = "grey35",
    linewidth = 0.35
  ) +
  geom_raster(
    data = df,
    aes(
      x = lon,
      y = lat,
      fill = adequabilidade
    )
  ) +
  {
    if (!is.null(area_m_plot)) {
      geom_sf(
        data = area_m_plot,
        fill = NA,
        color = "grey20",
        linewidth = 0.30,
        linetype = "dashed"
      )
    }
  } +
  geom_sf(
    data = oc_sf,
    color = "black",
    fill = "red",
    shape = 21,
    size = 1.05,
    stroke = 0.20,
    alpha = 0.80
  ) +
  scale_fill_viridis_c(
    name = "Adequabilidade",
    option = "viridis",
    limits = c(0, 1),
    na.value = NA
  ) +
  coord_sf(
    xlim = c(bbox_bioma["xmin"], bbox_bioma["xmax"]),
    ylim = c(bbox_bioma["ymin"], bbox_bioma["ymax"]),
    expand = FALSE
  ) +
  ggspatial::annotation_scale(
    location = "bl",
    width_hint = 0.28,
    text_cex = 0.65
  ) +
  ggspatial::annotation_north_arrow(
    location = "tr",
    which_north = "true",
    style = ggspatial::north_arrow_fancy_orienteering
  ) +
  labs(
    title = expression("Ensemble ponderado para " * italic("Dinizia excelsa")),
    subtitle = "Pesos derivados do desempenho preditivo dos modelos no conjunto de teste",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_bw(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 9),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    legend.position = "right",
    panel.grid.major = element_line(color = "grey88", linewidth = 0.25),
    panel.grid.minor = element_blank()
  )

ggsave(
  "figuras/unidade11/unidade11_ensemble_ponderado.png",
  plot = g,
  width = 9,
  height = 7,
  dpi = 600
)

# ------------------------------------------------------------
# 14. Gráfico dos pesos
# ------------------------------------------------------------

g_pesos <- ggplot(
  metricas,
  aes(
    x = reorder(modelo, peso),
    y = peso
  )
) +
  geom_col(
    fill = "grey55",
    color = "grey25",
    linewidth = 0.15
  ) +
  coord_flip() +
  labs(
    title = "Pesos dos modelos no ensemble ponderado",
    subtitle = "Pesos calculados a partir do AUC no conjunto de teste",
    x = NULL,
    y = "Peso normalizado"
  ) +
  theme_bw(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )

ggsave(
  "figuras/unidade11/unidade11_pesos_ensemble_ponderado.png",
  plot = g_pesos,
  width = 7,
  height = 5,
  dpi = 600
)

# ------------------------------------------------------------
# 15. Mensagem final
# ------------------------------------------------------------

message("Ensemble ponderado por desempenho gerado com sucesso.")
message("Modelos incluídos: ", paste(metricas$modelo, collapse = ", "))
message("Número de modelos: ", nrow(metricas))
message("Número de pixels preditos: ", nrow(df))
message("Raster salvo em: resultados/unidade11/ensemble_ponderado_dinizia.tif")
message("Mapa salvo em: figuras/unidade11/unidade11_ensemble_ponderado.png")

print(metricas)
