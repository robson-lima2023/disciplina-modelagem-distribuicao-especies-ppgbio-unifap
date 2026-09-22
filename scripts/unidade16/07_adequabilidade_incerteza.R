source("scripts/_bootstrap.R")

# ============================================================
# Unidade 16 - Incertezas em SDM
# Script 07: Adequabilidade versus incerteza
# ============================================================

pacotes <- c(
  "dplyr", "readr", "terra", "sf", "ggplot2",
  "ggspatial", "tibble", "tidyr", "patchwork"
)

require_packages(pacotes)
invisible(lapply(pacotes, library, character.only = TRUE))

sf::sf_use_s2(FALSE)
dir.create("dados/unidade16/processados", recursive = TRUE, showWarnings = FALSE)
dir.create("figuras/unidade16", recursive = TRUE, showWarnings = FALSE)
dir.create("tabelas/unidade16", recursive = TRUE, showWarnings = FALSE)
dir.create("resultados/unidade16", recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# 1. Arquivos
# ------------------------------------------------------------

arquivo_adeq <- "resultados/unidade11/ensemble_media_simples_dinizia.tif"
arquivo_inc <- "resultados/unidade16/incerteza_integrada_dinizia.tif"
arquivo_metricas <- "tabelas/unidade12/metricas_integradas_modelos.csv"

arquivo_bioma <- "dados/unidade01/brutos/amazon_biome_border.shp"
arquivo_m <- "dados/unidade05/processados/area_m_dinizia.gpkg"
arquivo_oc <- "dados/unidade05/processados/presencas_area_m_dinizia.csv"

if (!file.exists(arquivo_adeq)) stop("Ensemble atual não encontrado. Execute a Unidade 11.")
if (!file.exists(arquivo_inc)) stop("Incerteza integrada não encontrada. Execute o Script 06 da Unidade 16.")
if (!file.exists(arquivo_metricas)) stop("Métricas da Unidade 12 não encontradas.")
if (!file.exists(arquivo_bioma)) stop("Limite do bioma Amazônia não encontrado.")
if (!file.exists(arquivo_oc)) stop("Arquivo de ocorrências não encontrado.")

# ------------------------------------------------------------
# 2. Ler rasters e limiares
# ------------------------------------------------------------

adeq <- terra::rast(arquivo_adeq)
inc <- terra::rast(arquivo_inc)

names(adeq) <- "adequabilidade"
names(inc) <- "incerteza_integrada"

if (!terra::compareGeom(adeq, inc, stopOnError = FALSE)) {
  inc <- terra::resample(inc, adeq, method = "bilinear")
}

metricas <- readr::read_csv(
  arquivo_metricas,
  show_col_types = FALSE
)

limiar_adeq <- metricas |>
  dplyr::filter(
    modelo %in% c("Ensemble média", "Ensemble_media", "Ensemble_Media")
  ) |>
  dplyr::pull(limiar_TSS)

if (length(limiar_adeq) == 0 || is.na(limiar_adeq[1]) || !is.finite(limiar_adeq[1])) {
  warning("Limiar TSS do ensemble não encontrado. Usando 0.5.")
  limiar_adeq <- 0.5
}

limiar_adeq <- as.numeric(limiar_adeq[1])

limiar_inc <- as.numeric(
  terra::global(
    inc,
    fun = function(x, ...) stats::median(x, na.rm = TRUE),
    na.rm = TRUE
  )[1, 1]
)

if (is.na(limiar_inc) || !is.finite(limiar_inc)) {
  stop("Não foi possível calcular o limiar de incerteza.")
}

# ------------------------------------------------------------
# 3. Classificação
# ------------------------------------------------------------

adeq_bin <- terra::ifel(
  adeq >= limiar_adeq,
  1,
  0
)

inc_bin <- terra::ifel(
  inc >= limiar_inc,
  1,
  0
)

classe <- adeq_bin + inc_bin * 2

names(classe) <- "classe_adequabilidade_incerteza"

terra::writeRaster(
  classe,
  "resultados/unidade16/adequabilidade_incerteza_classes.tif",
  overwrite = TRUE
)

stack_diag <- c(adeq, inc, adeq_bin, inc_bin, classe)

names(stack_diag) <- c(
  "adequabilidade",
  "incerteza_integrada",
  "adequabilidade_binaria",
  "incerteza_binaria",
  "classe"
)

terra::writeRaster(
  stack_diag,
  "resultados/unidade16/stack_adequabilidade_incerteza.tif",
  overwrite = TRUE
)

df <- as.data.frame(
  stack_diag,
  xy = TRUE,
  na.rm = TRUE
) |>
  dplyr::rename(
    lon = x,
    lat = y
  ) |>
  dplyr::mutate(
    classe_nome = dplyr::case_when(
      classe == 0 ~ "Baixa adequabilidade / baixa incerteza",
      classe == 1 ~ "Alta adequabilidade / baixa incerteza",
      classe == 2 ~ "Baixa adequabilidade / alta incerteza",
      classe == 3 ~ "Alta adequabilidade / alta incerteza",
      TRUE ~ NA_character_
    ),
    classe_nome = factor(
      classe_nome,
      levels = c(
        "Baixa adequabilidade / baixa incerteza",
        "Baixa adequabilidade / alta incerteza",
        "Alta adequabilidade / alta incerteza",
        "Alta adequabilidade / baixa incerteza"
      )
    )
  )

readr::write_csv(
  df,
  "dados/unidade16/processados/adequabilidade_incerteza_classes.csv"
)

# ------------------------------------------------------------
# 4. Síntese por área
# ------------------------------------------------------------

area_cell <- terra::cellSize(
  adeq,
  unit = "km"
)

area_stack <- c(area_cell, classe)

names(area_stack) <- c("area_km2", "classe")

area_df <- as.data.frame(
  area_stack,
  xy = TRUE,
  na.rm = TRUE
) |>
  dplyr::rename(
    lon = x,
    lat = y
  ) |>
  dplyr::mutate(
    classe_nome = dplyr::case_when(
      classe == 0 ~ "Baixa adequabilidade / baixa incerteza",
      classe == 1 ~ "Alta adequabilidade / baixa incerteza",
      classe == 2 ~ "Baixa adequabilidade / alta incerteza",
      classe == 3 ~ "Alta adequabilidade / alta incerteza",
      TRUE ~ NA_character_
    )
  )

resumo <- area_df |>
  dplyr::group_by(classe_nome) |>
  dplyr::summarise(
    area_km2 = sum(area_km2, na.rm = TRUE),
    n_pixels = dplyr::n(),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    prop_area = area_km2 / sum(area_km2, na.rm = TRUE),
    limiar_adequabilidade = limiar_adeq,
    limiar_incerteza = limiar_inc
  )

readr::write_csv(
  resumo,
  "tabelas/unidade16/resumo_adequabilidade_incerteza.csv"
)

# ------------------------------------------------------------
# 5. Elementos cartográficos
# ------------------------------------------------------------

bioma_raw <- sf::st_read(arquivo_bioma, quiet = TRUE)

bioma_plot <- bioma_raw |>
  sf::st_transform(5880) |>
  sf::st_make_valid() |>
  sf::st_union() |>
  sf::st_as_sf() |>
  sf::st_make_valid() |>
  sf::st_transform(4326)

bbox_bioma <- sf::st_bbox(bioma_plot)

area_m_plot <- NULL

if (file.exists(arquivo_m)) {
  area_m_plot <- sf::st_read(arquivo_m, quiet = TRUE) |>
    sf::st_make_valid() |>
    sf::st_transform(4326)
}

oc <- readr::read_csv(arquivo_oc, show_col_types = FALSE)

oc_sf <- sf::st_as_sf(
  oc,
  coords = c("lon", "lat"),
  crs = 4326,
  remove = FALSE
)

# ------------------------------------------------------------
# 6. Mapa de classes
# ------------------------------------------------------------

cores_classes <- c(
  "Baixa adequabilidade / baixa incerteza" = "grey85",
  "Baixa adequabilidade / alta incerteza" = "#D9A441",
  "Alta adequabilidade / alta incerteza" = "#B2182B",
  "Alta adequabilidade / baixa incerteza" = "#1A9850"
)

g_mapa <- ggplot() +
  geom_sf(
    data = bioma_plot,
    fill = "grey96",
    color = "grey35",
    linewidth = 0.30
  ) +
  geom_raster(
    data = df,
    aes(
      x = lon,
      y = lat,
      fill = classe_nome
    )
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
    data = oc_sf,
    color = "black",
    fill = "red",
    shape = 21,
    size = 0.50,
    stroke = 0.13,
    alpha = 0.65
  ) +
  scale_fill_manual(
    values = cores_classes,
    name = "Classe",
    drop = FALSE
  ) +
  coord_sf(
    xlim = c(bbox_bioma["xmin"], bbox_bioma["xmax"]),
    ylim = c(bbox_bioma["ymin"], bbox_bioma["ymax"]),
    expand = FALSE
  ) +
  ggspatial::annotation_scale(
    location = "bl",
    width_hint = 0.25,
    text_cex = 0.60
  ) +
  ggspatial::annotation_north_arrow(
    location = "tr",
    which_north = "true",
    style = ggspatial::north_arrow_fancy_orienteering
  ) +
  labs(
    title = expression("Adequabilidade versus incerteza para " * italic("Dinizia excelsa")),
    subtitle = "Áreas verdes indicam alta adequabilidade com baixa incerteza",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_bw(base_size = 10) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10),
    legend.position = "bottom",
    panel.grid.major = element_line(color = "grey88", linewidth = 0.20),
    panel.grid.minor = element_blank()
  )

ggsave(
  "figuras/unidade16/unidade16_adequabilidade_incerteza.png",
  plot = g_mapa,
  width = 11,
  height = 8,
  dpi = 600
)

# ------------------------------------------------------------
# 7. Gráfico de proporção das classes
# ------------------------------------------------------------

g_bar <- ggplot(
  resumo,
  aes(
    x = reorder(classe_nome, prop_area),
    y = prop_area,
    fill = classe_nome
  )
) +
  geom_col(
    color = "grey25",
    linewidth = 0.15,
    show.legend = FALSE
  ) +
  coord_flip() +
  scale_fill_manual(values = cores_classes, drop = FALSE) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1)
  ) +
  labs(
    title = "Proporção de área por classe de adequabilidade-incerteza",
    subtitle = paste0(
      "Limiar adequabilidade = ",
      round(limiar_adeq, 3),
      " | Limiar incerteza = ",
      round(limiar_inc, 3)
    ),
    x = NULL,
    y = "Proporção da área"
  ) +
  theme_bw(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave(
  "figuras/unidade16/unidade16_proporcao_adequabilidade_incerteza.png",
  plot = g_bar,
  width = 9,
  height = 6,
  dpi = 600
)

fig_composta <- g_mapa / g_bar +
  patchwork::plot_layout(
    heights = c(1.25, 0.75)
  ) +
  patchwork::plot_annotation(
    title = expression("Síntese espacial de robustez ecológica para " * italic("Dinizia excelsa")),
    subtitle = "Integração entre adequabilidade ambiental e incerteza integrada"
  ) &
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 10)
  )

ggsave(
  "figuras/unidade16/unidade16_adequabilidade_incerteza_patchwork.png",
  plot = fig_composta,
  width = 11,
  height = 13,
  dpi = 600
)

message("Classificação adequabilidade-incerteza concluída com sucesso.")
print(resumo)
