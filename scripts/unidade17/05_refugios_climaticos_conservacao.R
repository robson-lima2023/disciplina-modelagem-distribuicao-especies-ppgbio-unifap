source("scripts/_bootstrap.R")

# ============================================================
# Unidade 17 - Aplicações à conservação
# Script 05: Refúgios climáticos potenciais
# ============================================================

pacotes <- c(
  "dplyr", "readr", "terra", "sf", "ggplot2",
  "ggspatial", "viridis", "tibble"
)

require_packages(pacotes)
invisible(lapply(pacotes, library, character.only = TRUE))

sf::sf_use_s2(FALSE)
dir.create("dados/unidade17/processados", recursive = TRUE, showWarnings = FALSE)
dir.create("figuras/unidade17", recursive = TRUE, showWarnings = FALSE)
dir.create("tabelas/unidade17", recursive = TRUE, showWarnings = FALSE)
dir.create("resultados/unidade17/componentes", recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# 1. Arquivos de entrada
# ------------------------------------------------------------

arquivos <- c(
  atual = "resultados/unidade11/ensemble_media_simples_dinizia.tif",
  holoceno_medio = "resultados/unidade15/ensemble/ensemble_passado_holoceno_medio.tif",
  lgm = "resultados/unidade15/ensemble/ensemble_passado_lgm.tif",
  lig = "resultados/unidade15/ensemble/ensemble_passado_lig.tif"
)

arquivo_bioma <- "dados/unidade01/brutos/amazon_biome_border.shp"
arquivo_area_m <- "dados/unidade05/processados/area_m_dinizia.gpkg"
arquivo_oc <- "dados/unidade05/processados/presencas_area_m_dinizia.csv"
arquivo_uc <- "dados/unidade17/processados/areas_protegidas_area_m.gpkg"

faltantes <- arquivos[!file.exists(arquivos)]

if (length(faltantes) > 0) {
  stop(
    "Arquivos de adequabilidade atual/paleoclimática faltantes: ",
    paste(names(faltantes), collapse = ", "),
    ". Execute as Unidades 11 e 15."
  )
}

if (!file.exists(arquivo_bioma)) stop("Limite do bioma Amazônia não encontrado.")
if (!file.exists(arquivo_area_m)) stop("Área M não encontrada.")
if (!file.exists(arquivo_oc)) stop("Arquivo de ocorrências não encontrado.")

# ------------------------------------------------------------
# 2. Ler, alinhar e empilhar rasters
# ------------------------------------------------------------

rasters <- lapply(
  arquivos,
  terra::rast
)

ref <- rasters[[1]]

rasters <- lapply(rasters, function(r) {
  if (!terra::compareGeom(ref, r, stopOnError = FALSE)) {
    terra::resample(
      r,
      ref,
      method = "bilinear"
    )
  } else {
    r
  }
})

stack_ref <- terra::rast(rasters)

names(stack_ref) <- names(arquivos)

terra::writeRaster(
  stack_ref,
  "resultados/unidade17/componentes/stack_refugios_climaticos.tif",
  overwrite = TRUE
)

# ------------------------------------------------------------
# 3. Índice de estabilidade histórica
# ------------------------------------------------------------
# Interpretação:
# valores altos indicam áreas com adequabilidade consistente
# no presente e nos períodos paleoclimáticos.
# ------------------------------------------------------------

estabilidade <- terra::app(
  stack_ref,
  fun = mean,
  na.rm = TRUE
)

incerteza_hist <- terra::app(
  stack_ref,
  fun = sd,
  na.rm = TRUE
)

estabilidade_minima <- terra::app(
  stack_ref,
  fun = min,
  na.rm = TRUE
)

names(estabilidade) <- "estabilidade_historica"
names(incerteza_hist) <- "incerteza_historica"
names(estabilidade_minima) <- "estabilidade_minima"

terra::writeRaster(
  estabilidade,
  "resultados/unidade17/componentes/estabilidade_historica_refugios.tif",
  overwrite = TRUE
)

terra::writeRaster(
  incerteza_hist,
  "resultados/unidade17/componentes/incerteza_historica_refugios.tif",
  overwrite = TRUE
)

terra::writeRaster(
  estabilidade_minima,
  "resultados/unidade17/componentes/estabilidade_minima_refugios.tif",
  overwrite = TRUE
)

# ------------------------------------------------------------
# 4. Delimitar refúgios climáticos
# ------------------------------------------------------------
# Critério principal:
# refúgios = 10% maiores valores do índice de estabilidade histórica.
# ------------------------------------------------------------

limiar_refugio <- as.numeric(
  terra::global(
    estabilidade,
    fun = function(x, ...) {
      stats::quantile(x, probs = 0.90, na.rm = TRUE)
    }
  )[1, 1]
)

refugio_bin <- terra::ifel(
  estabilidade >= limiar_refugio,
  1,
  NA
)

names(refugio_bin) <- "refugio_top10"

terra::writeRaster(
  refugio_bin,
  "resultados/unidade17/componentes/refugios_climaticos_top10.tif",
  overwrite = TRUE
)

# Converter refúgios para polígonos para contorno
refugio_poly <- terra::as.polygons(
  refugio_bin,
  dissolve = TRUE,
  na.rm = TRUE
)

refugio_sf <- sf::st_as_sf(refugio_poly) |>
  sf::st_make_valid()

sf::st_write(
  refugio_sf,
  "resultados/unidade17/componentes/refugios_climaticos_top10.gpkg",
  delete_dsn = TRUE,
  quiet = TRUE
)

# ------------------------------------------------------------
# 5. Exportar tabela espacial
# ------------------------------------------------------------

df <- as.data.frame(
  c(
    stack_ref,
    estabilidade,
    incerteza_hist,
    estabilidade_minima,
    refugio_bin
  ),
  xy = TRUE,
  na.rm = TRUE
) |>
  dplyr::rename(
    lon = x,
    lat = y
  )

readr::write_csv(
  df,
  "dados/unidade17/processados/estabilidade_historica_refugios.csv"
)

# ------------------------------------------------------------
# 6. Síntese de área
# ------------------------------------------------------------

area_cell <- terra::cellSize(
  estabilidade,
  unit = "km"
)

area_stack <- c(
  area_cell,
  estabilidade,
  incerteza_hist,
  refugio_bin
)

names(area_stack) <- c(
  "area_km2",
  "estabilidade",
  "incerteza",
  "refugio"
)

area_df <- as.data.frame(
  area_stack,
  xy = FALSE,
  na.rm = TRUE
)

sintese <- area_df |>
  dplyr::mutate(
    classe = ifelse(refugio == 1, "Refúgio climático top 10%", "Demais áreas")
  ) |>
  dplyr::group_by(classe) |>
  dplyr::summarise(
    area_km2 = sum(area_km2, na.rm = TRUE),
    estabilidade_media = mean(estabilidade, na.rm = TRUE),
    incerteza_media = mean(incerteza, na.rm = TRUE),
    n_pixels = dplyr::n(),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    prop_area = area_km2 / sum(area_km2, na.rm = TRUE),
    limiar_refugio = limiar_refugio
  )

readr::write_csv(
  sintese,
  "tabelas/unidade17/sintese_refugios_climaticos.csv"
)

# ------------------------------------------------------------
# 7. Elementos cartográficos
# ------------------------------------------------------------

bioma_raw <- sf::st_read(
  arquivo_bioma,
  quiet = TRUE
)

bioma_plot <- bioma_raw |>
  sf::st_transform(5880) |>
  sf::st_make_valid() |>
  sf::st_union() |>
  sf::st_as_sf() |>
  sf::st_make_valid() |>
  sf::st_transform(4326)

bbox_bioma <- sf::st_bbox(bioma_plot)

area_m_plot <- sf::st_read(
  arquivo_area_m,
  quiet = TRUE
) |>
  sf::st_make_valid() |>
  sf::st_transform(4326)

refugio_plot <- refugio_sf |>
  sf::st_transform(4326)

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

uc_plot <- NULL

if (file.exists(arquivo_uc)) {
  uc_plot <- sf::st_read(
    arquivo_uc,
    quiet = TRUE
  ) |>
    sf::st_make_valid() |>
    sf::st_transform(4326)
}

# ------------------------------------------------------------
# 8. Mapa principal: índice com contorno dos refúgios
# ------------------------------------------------------------

g_refugios <- ggplot() +
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
      fill = estabilidade_historica
    )
  ) +
  geom_sf(
    data = area_m_plot,
    fill = NA,
    color = "grey25",
    linewidth = 0.25,
    linetype = "dashed"
  ) +
  geom_sf(
    data = refugio_plot,
    fill = NA,
    color = "black",
    linewidth = 0.55
  ) +
  {
    if (!is.null(uc_plot) && nrow(uc_plot) > 0) {
      geom_sf(
        data = uc_plot,
        fill = NA,
        color = "#1A9850",
        linewidth = 0.30
      )
    }
  } +
  geom_sf(
    data = oc_sf,
    color = "black",
    fill = "red",
    shape = 21,
    size = 0.55,
    stroke = 0.12,
    alpha = 0.65
  ) +
  scale_fill_viridis_c(
    name = "Índice",
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
    width_hint = 0.25,
    text_cex = 0.60
  ) +
  ggspatial::annotation_north_arrow(
    location = "tr",
    which_north = "true",
    style = ggspatial::north_arrow_fancy_orienteering
  ) +
  labs(
    title = expression("Refúgios climáticos potenciais para " * italic("Dinizia excelsa")),
    subtitle = "Contorno preto indica os 10% maiores valores de estabilidade histórica",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_bw(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10),
    panel.grid.major = element_line(color = "grey88", linewidth = 0.20),
    panel.grid.minor = element_blank()
  )

ggsave(
  "figuras/unidade17/unidade17_refugios_climaticos.png",
  plot = g_refugios,
  width = 9.5,
  height = 7,
  dpi = 600
)

# ------------------------------------------------------------
# 9. Mapa categórico dos refúgios
# ------------------------------------------------------------

df_refugio <- df |>
  dplyr::mutate(
    classe_refugio = dplyr::case_when(
      refugio_top10 == 1 ~ "Refúgio climático top 10%",
      TRUE ~ "Demais áreas"
    ),
    classe_refugio = factor(
      classe_refugio,
      levels = c("Demais áreas", "Refúgio climático top 10%")
    )
  )

g_cat <- ggplot() +
  geom_sf(
    data = bioma_plot,
    fill = "grey96",
    color = "grey35",
    linewidth = 0.30
  ) +
  geom_raster(
    data = df_refugio,
    aes(
      x = lon,
      y = lat,
      fill = classe_refugio
    )
  ) +
  geom_sf(
    data = area_m_plot,
    fill = NA,
    color = "grey25",
    linewidth = 0.25,
    linetype = "dashed"
  ) +
  geom_sf(
    data = refugio_plot,
    fill = NA,
    color = "black",
    linewidth = 0.55
  ) +
  geom_sf(
    data = oc_sf,
    color = "black",
    fill = "red",
    shape = 21,
    size = 0.55,
    stroke = 0.12,
    alpha = 0.65
  ) +
  scale_fill_manual(
    values = c(
      "Demais áreas" = "grey85",
      "Refúgio climático top 10%" = "#1A9850"
    ),
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
  labs(
    title = expression("Delimitação dos refúgios climáticos para " * italic("Dinizia excelsa")),
    subtitle = paste0(
      "Áreas com estabilidade histórica >= quantil 90% (limiar = ",
      round(limiar_refugio, 3),
      ")"
    ),
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_bw(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

ggsave(
  "figuras/unidade17/unidade17_refugios_climaticos_categorico.png",
  plot = g_cat,
  width = 9.5,
  height = 7,
  dpi = 600
)

# ------------------------------------------------------------
# 10. Mensagem final
# ------------------------------------------------------------

message("Refúgios climáticos potenciais calculados com sucesso.")
message("Limiar top 10%: ", round(limiar_refugio, 4))

print(sintese)

