source("scripts/_bootstrap.R")

# ============================================================
# Unidade 7 - GAM em SDM
# Script 07: Comparar GLM e GAM
# Autor: Robson Borges de Lima
# ============================================================

# ------------------------------------------------------------
# 1. Pacotes
# ------------------------------------------------------------

pacotes <- c(
  "dplyr", "readr", "terra", "sf",
  "ggplot2", "ggspatial", "viridis",
  "patchwork", "tibble"
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

dir.create("figuras/unidade07", recursive = TRUE, showWarnings = FALSE)
dir.create("resultados/unidade07", recursive = TRUE, showWarnings = FALSE)
dir.create("dados/unidade07/processados", recursive = TRUE, showWarnings = FALSE)
dir.create("tabelas/unidade07", recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# 4. Arquivos de entrada
# ------------------------------------------------------------

arquivo_glm <- "resultados/unidade06/adequabilidade_glm_dinizia.tif"
arquivo_gam <- "resultados/unidade07/adequabilidade_gam_dinizia.tif"

arquivo_bioma <- "dados/unidade01/brutos/amazon_biome_border.shp"
arquivo_m <- "dados/unidade05/processados/area_m_dinizia.gpkg"
arquivo_oc <- "dados/unidade05/processados/presencas_area_m_dinizia.csv"

if (!file.exists(arquivo_glm)) {
  stop("Mapa GLM não encontrado. Execute o Script 05 da Unidade 6.")
}

if (!file.exists(arquivo_gam)) {
  stop("Mapa GAM não encontrado. Execute o Script 05 da Unidade 7.")
}

if (!file.exists(arquivo_bioma)) {
  stop("Limite do bioma Amazônia não encontrado.")
}

if (!file.exists(arquivo_oc)) {
  stop("Arquivo de ocorrências não encontrado.")
}

if (!file.exists(arquivo_m)) {
  warning("Área M não encontrada. A comparação será gerada sem contorno da área M.")
}

# ------------------------------------------------------------
# 5. Leitura dos rasters
# ------------------------------------------------------------

r_glm <- terra::rast(arquivo_glm)
r_gam <- terra::rast(arquivo_gam)

if (!terra::compareGeom(r_glm, r_gam, stopOnError = FALSE)) {
  r_gam <- terra::resample(
    r_gam,
    r_glm,
    method = "bilinear"
  )
}

names(r_glm) <- "adequabilidade_glm"
names(r_gam) <- "adequabilidade_gam"

# ------------------------------------------------------------
# 6. Diferença espacial GAM - GLM
# ------------------------------------------------------------

r_diff <- r_gam - r_glm
names(r_diff) <- "diferenca_gam_menos_glm"

terra::writeRaster(
  r_diff,
  "resultados/unidade07/diferenca_gam_menos_glm.tif",
  overwrite = TRUE
)

# ------------------------------------------------------------
# 7. Estatísticas pixel a pixel
# ------------------------------------------------------------

df_comp <- as.data.frame(
  c(r_glm, r_gam, r_diff),
  xy = TRUE,
  na.rm = TRUE
) |>
  rename(
    lon = x,
    lat = y
  )

cor_pearson <- stats::cor(
  df_comp$adequabilidade_glm,
  df_comp$adequabilidade_gam,
  method = "pearson",
  use = "complete.obs"
)

cor_spearman <- stats::cor(
  df_comp$adequabilidade_glm,
  df_comp$adequabilidade_gam,
  method = "spearman",
  use = "complete.obs"
)

rmse <- sqrt(
  mean(
    (df_comp$adequabilidade_gam - df_comp$adequabilidade_glm)^2,
    na.rm = TRUE
  )
)

mae <- mean(
  abs(df_comp$adequabilidade_gam - df_comp$adequabilidade_glm),
  na.rm = TRUE
)

read_csv_comp <- df_comp |>
  select(
    lon,
    lat,
    adequabilidade_glm,
    adequabilidade_gam,
    diferenca_gam_menos_glm
  )

write_csv(
  read_csv_comp,
  "dados/unidade07/processados/comparacao_pixel_glm_gam.csv"
)

# ------------------------------------------------------------
# 8. Leitura e correção do bioma
# ------------------------------------------------------------

bioma_raw <- sf::st_read(
  arquivo_bioma,
  quiet = TRUE
)

bioma_proj <- bioma_raw |>
  sf::st_transform(5880) |>
  sf::st_make_valid() |>
  sf::st_union() |>
  sf::st_as_sf() |>
  sf::st_make_valid()

bioma <- bioma_proj |>
  sf::st_transform(terra::crs(r_glm)) |>
  sf::st_make_valid()

bioma_plot <- sf::st_transform(bioma, 4326)
bbox_bioma <- sf::st_bbox(bioma_plot)

# ------------------------------------------------------------
# 9. Área M e ocorrências
# ------------------------------------------------------------

area_m_plot <- NULL

if (file.exists(arquivo_m)) {
  area_m <- sf::st_read(
    arquivo_m,
    quiet = TRUE
  )
  
  area_m_plot <- area_m |>
    sf::st_make_valid() |>
    sf::st_transform(4326)
}

oc <- read_csv(
  arquivo_oc,
  show_col_types = FALSE
)

if (!all(c("lon", "lat") %in% names(oc))) {
  stop("O arquivo de ocorrências precisa conter as colunas 'lon' e 'lat'.")
}

oc_plot <- sf::st_as_sf(
  oc,
  coords = c("lon", "lat"),
  crs = 4326,
  remove = FALSE
)

# ------------------------------------------------------------
# 10. Converter rasters para data.frame
# ------------------------------------------------------------

df_glm <- as.data.frame(
  r_glm,
  xy = TRUE,
  na.rm = TRUE
) |>
  rename(
    lon = x,
    lat = y,
    valor = adequabilidade_glm
  )

df_gam <- as.data.frame(
  r_gam,
  xy = TRUE,
  na.rm = TRUE
) |>
  rename(
    lon = x,
    lat = y,
    valor = adequabilidade_gam
  )

df_diff <- as.data.frame(
  r_diff,
  xy = TRUE,
  na.rm = TRUE
) |>
  rename(
    lon = x,
    lat = y,
    valor = diferenca_gam_menos_glm
  )

# ------------------------------------------------------------
# 11. Função para mapas de adequabilidade
# ------------------------------------------------------------

mapa_adequabilidade <- function(df, titulo) {
  
  ggplot() +
    geom_sf(
      data = bioma_plot,
      fill = "grey96",
      color = "grey35",
      linewidth = 0.30
    ) +
    geom_raster(
      data = df,
      aes(x = lon, y = lat, fill = valor)
    ) +
    {
      if (!is.null(area_m_plot)) {
        geom_sf(
          data = area_m_plot,
          fill = NA,
          color = "grey20",
          linewidth = 0.25,
          linetype = "dashed"
        )
      }
    } +
    geom_sf(
      data = oc_plot,
      color = "black",
      fill = "red",
      shape = 21,
      size = 0.75,
      stroke = 0.18,
      alpha = 0.75
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
    labs(
      title = titulo,
      x = "Longitude",
      y = "Latitude"
    ) +
    theme_bw(base_size = 10) +
    theme(
      plot.title = element_text(face = "bold", size = 11),
      legend.position = "right",
      panel.grid.major = element_line(color = "grey88", linewidth = 0.20),
      panel.grid.minor = element_blank()
    )
}

# ------------------------------------------------------------
# 12. Mapa da diferença espacial
# ------------------------------------------------------------

lim_diff <- max(abs(df_diff$valor), na.rm = TRUE)

p_diff <- ggplot() +
  geom_sf(
    data = bioma_plot,
    fill = "grey96",
    color = "grey35",
    linewidth = 0.30
  ) +
  geom_raster(
    data = df_diff,
    aes(x = lon, y = lat, fill = valor)
  ) +
  {
    if (!is.null(area_m_plot)) {
      geom_sf(
        data = area_m_plot,
        fill = NA,
        color = "grey20",
        linewidth = 0.25,
        linetype = "dashed"
      )
    }
  } +
  scale_fill_gradient2(
    name = "GAM - GLM",
    low = "#2166AC",
    mid = "white",
    high = "#B2182B",
    midpoint = 0,
    limits = c(-lim_diff, lim_diff),
    na.value = NA
  ) +
  coord_sf(
    xlim = c(bbox_bioma["xmin"], bbox_bioma["xmax"]),
    ylim = c(bbox_bioma["ymin"], bbox_bioma["ymax"]),
    expand = FALSE
  ) +
  labs(
    title = "Diferença espacial entre modelos",
    subtitle = "Valores positivos indicam maior adequabilidade estimada pelo GAM",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_bw(base_size = 10) +
  theme(
    plot.title = element_text(face = "bold", size = 11),
    plot.subtitle = element_text(size = 9),
    legend.position = "right",
    panel.grid.major = element_line(color = "grey88", linewidth = 0.20),
    panel.grid.minor = element_blank()
  )

# ------------------------------------------------------------
# 13. Mapas individuais
# ------------------------------------------------------------

p_glm <- mapa_adequabilidade(df_glm, "GLM")
p_gam <- mapa_adequabilidade(df_gam, "GAM")

# ------------------------------------------------------------
# 14. Figura composta com patchwork
# ------------------------------------------------------------

linha_superior <- p_glm + p_gam +
  patchwork::plot_layout(
    ncol = 2,
    guides = "collect"
  ) &
  theme(legend.position = "right")

linha_inferior <- p_diff + patchwork::plot_spacer() +
  patchwork::plot_layout(widths = c(1.15, 0.05))

fig <- linha_superior / linha_inferior +
  patchwork::plot_layout(
    heights = c(1.15, 1.25)
  ) +
  patchwork::plot_annotation(
    title = expression("Comparação espacial entre GLM e GAM para " * italic("Dinizia excelsa")),
    subtitle = "Modelos calibrados com variáveis selecionadas por VIF e projetados para o bioma Amazônia"
  ) &
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 11)
  )

# ------------------------------------------------------------
# 15. Salvar figura
# ------------------------------------------------------------

ggsave(
  "figuras/unidade07/unidade07_comparacao_glm_gam.png",
  plot = fig,
  width = 13,
  height = 10,
  dpi = 600
)

# ------------------------------------------------------------
# 16. Salvar tabela resumo da diferença
# ------------------------------------------------------------

resumo_diff <- tibble(
  estatistica = c(
    "mínimo",
    "primeiro_quartil",
    "mediana",
    "média",
    "terceiro_quartil",
    "máximo",
    "desvio_padrao",
    "correlacao_pearson",
    "correlacao_spearman",
    "rmse_gam_glm",
    "mae_gam_glm",
    "n_pixels_comparados"
  ),
  valor = c(
    min(df_diff$valor, na.rm = TRUE),
    stats::quantile(df_diff$valor, 0.25, na.rm = TRUE),
    stats::median(df_diff$valor, na.rm = TRUE),
    mean(df_diff$valor, na.rm = TRUE),
    stats::quantile(df_diff$valor, 0.75, na.rm = TRUE),
    max(df_diff$valor, na.rm = TRUE),
    stats::sd(df_diff$valor, na.rm = TRUE),
    cor_pearson,
    cor_spearman,
    rmse,
    mae,
    nrow(df_comp)
  )
)

write_csv(
  resumo_diff,
  "tabelas/unidade07/resumo_diferenca_gam_glm.csv"
)

# ------------------------------------------------------------
# 17. Mensagem final
# ------------------------------------------------------------

message("Comparação GLM-GAM concluída.")
message("Correlação de Pearson GLM-GAM: ", round(cor_pearson, 3))
message("RMSE GAM-GLM: ", round(rmse, 4))
message("Figura salva em: figuras/unidade07/unidade07_comparacao_glm_gam.png")

print(resumo_diff)
