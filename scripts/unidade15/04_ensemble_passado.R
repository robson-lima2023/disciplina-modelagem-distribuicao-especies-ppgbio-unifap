source("scripts/_bootstrap.R")

# ============================================================
# Unidade 15 - Paleoclima e nicho climático passado
# Script 04: Ensemble paleoclimático
# ============================================================

pacotes <- c(
  "dplyr", "readr", "terra", "sf", "ggplot2",
  "ggspatial", "viridis", "tibble", "tidyr",
  "patchwork"
)

require_packages(pacotes)
invisible(lapply(pacotes, library, character.only = TRUE))

sf::sf_use_s2(FALSE)
dir.create("dados/unidade15/processados", recursive = TRUE, showWarnings = FALSE)
dir.create("figuras/unidade15", recursive = TRUE, showWarnings = FALSE)
dir.create("tabelas/unidade15", recursive = TRUE, showWarnings = FALSE)
dir.create("resultados/unidade15/ensemble", recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# 1. Configurações
# ------------------------------------------------------------

periodos <- tibble::tibble(
  periodo = c("lig", "lgm", "holoceno_medio"),
  periodo_legenda = c(
    "Último Interglacial",
    "Último Máximo Glacial",
    "Holoceno Médio"
  )
)

arquivo_bioma <- "dados/unidade01/brutos/amazon_biome_border.shp"
arquivo_m <- "dados/unidade05/processados/area_m_dinizia.gpkg"
arquivo_oc <- "dados/unidade05/processados/presencas_area_m_dinizia.csv"

if (!file.exists(arquivo_bioma)) {
  stop("Limite do bioma Amazônia não encontrado.")
}

if (!file.exists(arquivo_oc)) {
  stop("Arquivo de ocorrências não encontrado.")
}

# ------------------------------------------------------------
# 2. Calcular ensemble por período
# ------------------------------------------------------------

dados_all <- list()
resumos <- list()
rasters_ensemble <- list()
rasters_incerteza <- list()

for (i in seq_len(nrow(periodos))) {
  
  periodo <- periodos$periodo[i]
  periodo_legenda <- periodos$periodo_legenda[i]
  
  message("Gerando ensemble paleoclimático para: ", periodo_legenda)
  
  arquivo <- paste0(
    "resultados/unidade15/modelos_individuais/predicoes_modelos_passado_",
    periodo,
    ".tif"
  )
  
  if (!file.exists(arquivo)) {
    stop("Predições individuais não encontradas para ", periodo, ". Execute o Script 03 da Unidade 15.")
  }
  
  stack_modelos <- terra::rast(arquivo)
  
  ensemble <- terra::app(
    stack_modelos,
    fun = mean,
    na.rm = TRUE
  )
  
  incerteza <- terra::app(
    stack_modelos,
    fun = sd,
    na.rm = TRUE
  )
  
  names(ensemble) <- paste0("ensemble_", periodo)
  names(incerteza) <- paste0("incerteza_", periodo)
  
  terra::writeRaster(
    ensemble,
    paste0("resultados/unidade15/ensemble/ensemble_passado_", periodo, ".tif"),
    overwrite = TRUE
  )
  
  terra::writeRaster(
    incerteza,
    paste0("resultados/unidade15/ensemble/incerteza_ensemble_passado_", periodo, ".tif"),
    overwrite = TRUE
  )
  
  df_ens <- as.data.frame(
    ensemble,
    xy = TRUE,
    na.rm = TRUE
  )
  
  names(df_ens) <- c("lon", "lat", "adequabilidade")
  
  df_inc <- as.data.frame(
    incerteza,
    xy = TRUE,
    na.rm = TRUE
  )
  
  names(df_inc) <- c("lon", "lat", "incerteza")
  
  df <- df_ens |>
    left_join(
      df_inc,
      by = c("lon", "lat")
    ) |>
    mutate(
      periodo = periodo,
      periodo_legenda = periodo_legenda
    )
  
  readr::write_csv(
    df,
    paste0("dados/unidade15/processados/ensemble_passado_", periodo, ".csv")
  )
  
  resumo_periodo <- df |>
    summarise(
      periodo = periodo,
      periodo_legenda = periodo_legenda,
      adequabilidade_media = mean(adequabilidade, na.rm = TRUE),
      adequabilidade_mediana = median(adequabilidade, na.rm = TRUE),
      adequabilidade_min = min(adequabilidade, na.rm = TRUE),
      adequabilidade_max = max(adequabilidade, na.rm = TRUE),
      adequabilidade_sd = sd(adequabilidade, na.rm = TRUE),
      incerteza_media = mean(incerteza, na.rm = TRUE),
      incerteza_mediana = median(incerteza, na.rm = TRUE),
      incerteza_max = max(incerteza, na.rm = TRUE),
      n_pixels = n()
    )
  
  dados_all[[periodo]] <- df
  resumos[[periodo]] <- resumo_periodo
  rasters_ensemble[[periodo]] <- ensemble
  rasters_incerteza[[periodo]] <- incerteza
}

dados_all <- bind_rows(dados_all)
resumos <- bind_rows(resumos)

readr::write_csv(
  dados_all,
  "dados/unidade15/processados/ensemble_passado_todos_periodos.csv"
)

readr::write_csv(
  resumos,
  "tabelas/unidade15/resumo_ensemble_passado.csv"
)

# ------------------------------------------------------------
# 3. Preparar elementos cartográficos
# ------------------------------------------------------------

bioma_raw <- sf::st_read(
  arquivo_bioma,
  quiet = TRUE
)

ref <- rasters_ensemble[[1]]

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
  area_m_plot <- sf::st_read(
    arquivo_m,
    quiet = TRUE
  ) |>
    sf::st_make_valid() |>
    sf::st_transform(4326)
}

oc <- readr::read_csv(
  arquivo_oc,
  show_col_types = FALSE
)

oc_sf <- sf::st_as_sf(
  oc,
  coords = c("lon", "lat"),
  crs = 4326,
  remove = FALSE
)

# ------------------------------------------------------------
# 4. Mapa do ensemble paleoclimático
# ------------------------------------------------------------

g_ensemble <- ggplot() +
  geom_sf(
    data = bioma_plot,
    fill = "grey96",
    color = "grey35",
    linewidth = 0.30
  ) +
  geom_raster(
    data = dados_all,
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
  facet_wrap(
    ~ periodo_legenda,
    ncol = 3
  ) +
  ggspatial::annotation_scale(
    location = "bl",
    width_hint = 0.22,
    text_cex = 0.55
  ) +
  labs(
    title = expression("Ensemble paleoclimático para " * italic("Dinizia excelsa")),
    subtitle = "Média das projeções GLM, GAM, Random Forest, BRT e MaxEnt",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_bw(base_size = 10) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10),
    strip.text = element_text(face = "bold"),
    panel.grid.major = element_line(color = "grey88", linewidth = 0.20),
    panel.grid.minor = element_blank()
  )

ggsave(
  "figuras/unidade15/unidade15_ensemble_passado_periodos.png",
  plot = g_ensemble,
  width = 13,
  height = 6,
  dpi = 600
)

# ------------------------------------------------------------
# 5. Mapa de incerteza entre modelos
# ------------------------------------------------------------

g_incerteza <- ggplot() +
  geom_sf(
    data = bioma_plot,
    fill = "grey96",
    color = "grey35",
    linewidth = 0.30
  ) +
  geom_raster(
    data = dados_all,
    aes(
      x = lon,
      y = lat,
      fill = incerteza
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
  scale_fill_viridis_c(
    name = "DP entre modelos",
    option = "magma",
    na.value = NA
  ) +
  coord_sf(
    xlim = c(bbox_bioma["xmin"], bbox_bioma["xmax"]),
    ylim = c(bbox_bioma["ymin"], bbox_bioma["ymax"]),
    expand = FALSE
  ) +
  facet_wrap(
    ~ periodo_legenda,
    ncol = 3
  ) +
  ggspatial::annotation_scale(
    location = "bl",
    width_hint = 0.22,
    text_cex = 0.55
  ) +
  labs(
    title = expression("Incerteza do ensemble paleoclimático para " * italic("Dinizia excelsa")),
    subtitle = "Desvio-padrão das predições entre algoritmos",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_bw(base_size = 10) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10),
    strip.text = element_text(face = "bold"),
    panel.grid.major = element_line(color = "grey88", linewidth = 0.20),
    panel.grid.minor = element_blank()
  )

ggsave(
  "figuras/unidade15/unidade15_incerteza_ensemble_passado_periodos.png",
  plot = g_incerteza,
  width = 13,
  height = 6,
  dpi = 600
)

# ------------------------------------------------------------
# 6. Figura composta
# ------------------------------------------------------------

fig_composta <- g_ensemble / g_incerteza +
  patchwork::plot_annotation(
    title = expression("Síntese paleoclimática de adequabilidade para " * italic("Dinizia excelsa")),
    subtitle = "Consenso médio e incerteza entre modelos para períodos climáticos do passado"
  ) &
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 10)
  )

ggsave(
  "figuras/unidade15/unidade15_ensemble_incerteza_passado_patchwork.png",
  plot = fig_composta,
  width = 13,
  height = 12,
  dpi = 600
)

message("Ensemble paleoclimático gerado com sucesso.")
print(resumos)
