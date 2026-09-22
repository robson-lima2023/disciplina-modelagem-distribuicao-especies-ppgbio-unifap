source("scripts/_bootstrap.R")

# ============================================================
# Unidade 16 - Incertezas em SDM
# Script 04: Incerteza por extrapolação ambiental
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
dir.create("resultados/unidade16/componentes", recursive = TRUE, showWarnings = FALSE)

cenarios <- c("ssp126", "ssp245", "ssp370", "ssp585")
periodo <- "2061-2080"
modelo_gcm <- "MIROC6"

arquivo_bioma <- "dados/unidade01/brutos/amazon_biome_border.shp"
arquivo_m <- "dados/unidade05/processados/area_m_dinizia.gpkg"
arquivo_oc <- "dados/unidade05/processados/presencas_area_m_dinizia.csv"

if (!file.exists(arquivo_bioma)) stop("Limite do bioma Amazônia não encontrado.")
if (!file.exists(arquivo_oc)) stop("Arquivo de ocorrências não encontrado.")

dados <- list()
rasters <- list()

for (ssp in cenarios) {
  
  arquivo_novo <- paste0(
    "resultados/unidade14/extrapolacao/extrapolacao_combinada_",
    ssp, "_", periodo, "_", modelo_gcm, ".tif"
  )
  
  arquivo_antigo <- paste0(
    "resultados/unidade14/extrapolacao/extrapolacao_combinada_",
    ssp, ".tif"
  )
  
  arquivo <- ifelse(file.exists(arquivo_novo), arquivo_novo, arquivo_antigo)
  
  if (!file.exists(arquivo)) {
    stop("Mapa de extrapolação não encontrado para ", ssp, ". Execute a Unidade 14, Script 05.")
  }
  
  r <- terra::rast(arquivo)
  names(r) <- ssp
  
  rasters[[ssp]] <- r
  
  df <- as.data.frame(
    r,
    xy = TRUE,
    na.rm = TRUE
  )
  
  names(df) <- c("lon", "lat", "extrapolacao")
  
  dados[[ssp]] <- df |>
    mutate(
      cenario = ssp,
      classe = case_when(
        extrapolacao == 0 ~ "Sem extrapolação",
        extrapolacao == 1 ~ "Com extrapolação",
        TRUE ~ NA_character_
      )
    )
}

ref <- rasters[[1]]

rasters <- lapply(rasters, function(r) {
  if (!terra::compareGeom(ref, r, stopOnError = FALSE)) {
    terra::resample(r, ref, method = "near")
  } else {
    r
  }
})

stack_ext <- terra::rast(rasters)
names(stack_ext) <- cenarios

terra::writeRaster(
  stack_ext,
  "resultados/unidade16/componentes/stack_extrapolacao_cenarios.tif",
  overwrite = TRUE
)

inc_ext <- terra::app(
  stack_ext,
  fun = mean,
  na.rm = TRUE
)

freq_ext <- terra::app(
  stack_ext,
  fun = sum,
  na.rm = TRUE
)

names(inc_ext) <- "proporcao_extrapolacao"
names(freq_ext) <- "frequencia_extrapolacao"

terra::writeRaster(
  inc_ext,
  "resultados/unidade16/componentes/incerteza_extrapolacao.tif",
  overwrite = TRUE
)

terra::writeRaster(
  freq_ext,
  "resultados/unidade16/componentes/frequencia_extrapolacao.tif",
  overwrite = TRUE
)

dados <- dplyr::bind_rows(dados)

readr::write_csv(
  dados,
  "dados/unidade16/processados/extrapolacao_cenarios_long.csv"
)

df_inc <- as.data.frame(
  c(inc_ext, freq_ext),
  xy = TRUE,
  na.rm = TRUE
)

names(df_inc) <- c(
  "lon",
  "lat",
  "proporcao_extrapolacao",
  "frequencia_extrapolacao"
)

readr::write_csv(
  df_inc,
  "dados/unidade16/processados/incerteza_extrapolacao.csv"
)

resumo <- dados |>
  group_by(cenario, classe) |>
  summarise(
    n_pixels = n(),
    .groups = "drop"
  ) |>
  group_by(cenario) |>
  mutate(
    proporcao = n_pixels / sum(n_pixels)
  ) |>
  ungroup()

readr::write_csv(
  resumo,
  "tabelas/unidade16/resumo_extrapolacao_cenarios.csv"
)

resumo_integrado <- tibble(
  componente = c("proporcao_extrapolacao", "frequencia_extrapolacao"),
  minimo = c(
    min(df_inc$proporcao_extrapolacao, na.rm = TRUE),
    min(df_inc$frequencia_extrapolacao, na.rm = TRUE)
  ),
  media = c(
    mean(df_inc$proporcao_extrapolacao, na.rm = TRUE),
    mean(df_inc$frequencia_extrapolacao, na.rm = TRUE)
  ),
  mediana = c(
    median(df_inc$proporcao_extrapolacao, na.rm = TRUE),
    median(df_inc$frequencia_extrapolacao, na.rm = TRUE)
  ),
  maximo = c(
    max(df_inc$proporcao_extrapolacao, na.rm = TRUE),
    max(df_inc$frequencia_extrapolacao, na.rm = TRUE)
  )
)

readr::write_csv(
  resumo_integrado,
  "tabelas/unidade16/resumo_incerteza_extrapolacao.csv"
)

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

g_classes <- ggplot() +
  geom_sf(
    data = bioma_plot,
    fill = "grey96",
    color = "grey35",
    linewidth = 0.30
  ) +
  geom_raster(
    data = dados,
    aes(x = lon, y = lat, fill = classe)
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
    values = c(
      "Sem extrapolação" = "#1B7837",
      "Com extrapolação" = "#B2182B"
    ),
    name = "Classe",
    drop = FALSE
  ) +
  coord_sf(
    xlim = c(bbox_bioma["xmin"], bbox_bioma["xmax"]),
    ylim = c(bbox_bioma["ymin"], bbox_bioma["ymax"]),
    expand = FALSE
  ) +
  facet_wrap(~ cenario, ncol = 2) +
  ggspatial::annotation_scale(
    location = "bl",
    width_hint = 0.25,
    text_cex = 0.60
  ) +
  labs(
    title = expression("Extrapolação ambiental futura para " * italic("Dinizia excelsa")),
    subtitle = "Diagnóstico combinado por MESS e MOP em cada cenário",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_bw(base_size = 10) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom",
    panel.grid.major = element_line(color = "grey88", linewidth = 0.20),
    panel.grid.minor = element_blank()
  )

ggsave(
  "figuras/unidade16/unidade16_extrapolacao_cenarios.png",
  plot = g_classes,
  width = 12,
  height = 9,
  dpi = 600
)

g_prop <- ggplot() +
  geom_sf(
    data = bioma_plot,
    fill = "grey96",
    color = "grey35",
    linewidth = 0.30
  ) +
  geom_raster(
    data = df_inc,
    aes(
      x = lon,
      y = lat,
      fill = proporcao_extrapolacao
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
    name = "Proporção",
    option = "magma",
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
    width_hint = 0.25,
    text_cex = 0.60
  ) +
  ggspatial::annotation_north_arrow(
    location = "tr",
    which_north = "true",
    style = ggspatial::north_arrow_fancy_orienteering
  ) +
  labs(
    title = expression("Incerteza por extrapolação ambiental para " * italic("Dinizia excelsa")),
    subtitle = "Proporção de cenários futuros em extrapolação ambiental",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_bw(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10),
    panel.grid.major = element_line(color = "grey88", linewidth = 0.20),
    panel.grid.minor = element_blank()
  )

ggsave(
  "figuras/unidade16/unidade16_incerteza_extrapolacao.png",
  plot = g_prop,
  width = 9,
  height = 7,
  dpi = 600
)

fig_composta <- g_classes / g_prop +
  patchwork::plot_annotation(
    title = expression("Componente de incerteza por extrapolação ambiental para " * italic("Dinizia excelsa")),
    subtitle = "Áreas extrapoladas por cenário e proporção integrada entre cenários SSP"
  ) &
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 10)
  )

ggsave(
  "figuras/unidade16/unidade16_extrapolacao_patchwork.png",
  plot = fig_composta,
  width = 12,
  height = 16,
  dpi = 600
)

message("Incerteza por extrapolação ambiental calculada com sucesso.")
print(resumo_integrado)
