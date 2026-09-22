source("scripts/_bootstrap.R")

# ============================================================
# Unidade 16 - Incertezas em SDM
# Script 05: Incerteza paleoclimática
# ============================================================

pacotes <- c(
  "dplyr", "readr", "terra", "sf", "ggplot2",
  "ggspatial", "viridis", "tibble", "patchwork"
)

require_packages(pacotes)
invisible(lapply(pacotes, library, character.only = TRUE))

sf::sf_use_s2(FALSE)
dir.create("dados/unidade16/processados", recursive = TRUE, showWarnings = FALSE)
dir.create("figuras/unidade16", recursive = TRUE, showWarnings = FALSE)
dir.create("tabelas/unidade16", recursive = TRUE, showWarnings = FALSE)
dir.create("resultados/unidade16/componentes", recursive = TRUE, showWarnings = FALSE)

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

if (!file.exists(arquivo_bioma)) stop("Limite do bioma Amazônia não encontrado.")
if (!file.exists(arquivo_oc)) stop("Arquivo de ocorrências não encontrado.")

arquivos <- paste0(
  "resultados/unidade15/ensemble/ensemble_passado_",
  periodos$periodo,
  ".tif"
)

faltantes <- arquivos[!file.exists(arquivos)]

if (length(faltantes) > 0) {
  stop(
    "Ensembles paleoclimáticos faltantes. Execute a Unidade 15. Arquivos ausentes: ",
    paste(faltantes, collapse = ", ")
  )
}

rasters <- lapply(arquivos, terra::rast)

ref <- rasters[[1]]

rasters <- lapply(rasters, function(r) {
  if (!terra::compareGeom(ref, r, stopOnError = FALSE)) {
    terra::resample(r, ref, method = "bilinear")
  } else {
    r
  }
})

stack_paleo <- terra::rast(rasters)
names(stack_paleo) <- periodos$periodo

terra::writeRaster(
  stack_paleo,
  "resultados/unidade16/componentes/stack_ensembles_paleoclimaticos.tif",
  overwrite = TRUE
)

media_paleo <- terra::app(
  stack_paleo,
  fun = mean,
  na.rm = TRUE
)

inc_paleo <- terra::app(
  stack_paleo,
  fun = sd,
  na.rm = TRUE
)

amplitude_paleo <- terra::app(
  stack_paleo,
  fun = function(x) max(x, na.rm = TRUE) - min(x, na.rm = TRUE)
)

cv_paleo <- inc_paleo / media_paleo

names(media_paleo) <- "media_paleoclimatica"
names(inc_paleo) <- "incerteza_paleoclimatica"
names(amplitude_paleo) <- "amplitude_paleoclimatica"
names(cv_paleo) <- "cv_paleoclimatico"

terra::writeRaster(
  inc_paleo,
  "resultados/unidade16/componentes/incerteza_paleoclimatica.tif",
  overwrite = TRUE
)

terra::writeRaster(
  amplitude_paleo,
  "resultados/unidade16/componentes/amplitude_paleoclimatica.tif",
  overwrite = TRUE
)

terra::writeRaster(
  cv_paleo,
  "resultados/unidade16/componentes/cv_paleoclimatico.tif",
  overwrite = TRUE
)

stack_inc <- c(
  media_paleo,
  inc_paleo,
  amplitude_paleo,
  cv_paleo
)

terra::writeRaster(
  stack_inc,
  "resultados/unidade16/componentes/stack_incerteza_paleoclimatica.tif",
  overwrite = TRUE
)

df <- as.data.frame(
  stack_inc,
  xy = TRUE,
  na.rm = TRUE
)

names(df) <- c(
  "lon",
  "lat",
  "media",
  "incerteza",
  "amplitude",
  "cv"
)

readr::write_csv(
  df,
  "dados/unidade16/processados/incerteza_paleoclimatica.csv"
)

resumo <- tibble(
  componente = c(
    "media_paleoclimatica",
    "incerteza_paleoclimatica",
    "amplitude_paleoclimatica",
    "cv_paleoclimatico"
  ),
  minimo = c(
    min(df$media, na.rm = TRUE),
    min(df$incerteza, na.rm = TRUE),
    min(df$amplitude, na.rm = TRUE),
    min(df$cv, na.rm = TRUE)
  ),
  media = c(
    mean(df$media, na.rm = TRUE),
    mean(df$incerteza, na.rm = TRUE),
    mean(df$amplitude, na.rm = TRUE),
    mean(df$cv, na.rm = TRUE)
  ),
  mediana = c(
    median(df$media, na.rm = TRUE),
    median(df$incerteza, na.rm = TRUE),
    median(df$amplitude, na.rm = TRUE),
    median(df$cv, na.rm = TRUE)
  ),
  maximo = c(
    max(df$media, na.rm = TRUE),
    max(df$incerteza, na.rm = TRUE),
    max(df$amplitude, na.rm = TRUE),
    max(df$cv, na.rm = TRUE)
  ),
  desvio_padrao = c(
    sd(df$media, na.rm = TRUE),
    sd(df$incerteza, na.rm = TRUE),
    sd(df$amplitude, na.rm = TRUE),
    sd(df$cv, na.rm = TRUE)
  )
)

readr::write_csv(
  resumo,
  "tabelas/unidade16/resumo_incerteza_paleoclimatica.csv"
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

g_sd <- ggplot() +
  geom_sf(
    data = bioma_plot,
    fill = "grey96",
    color = "grey35",
    linewidth = 0.30
  ) +
  geom_raster(
    data = df,
    aes(x = lon, y = lat, fill = incerteza)
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
    name = "Desvio-padrão",
    option = "magma",
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
    title = expression("Incerteza paleoclimática para " * italic("Dinizia excelsa")),
    subtitle = "Desvio-padrão entre Último Interglacial, Último Máximo Glacial e Holoceno Médio",
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
  "figuras/unidade16/unidade16_incerteza_paleoclimatica.png",
  plot = g_sd,
  width = 9,
  height = 7,
  dpi = 600
)

g_amp <- ggplot() +
  geom_sf(
    data = bioma_plot,
    fill = "grey96",
    color = "grey35",
    linewidth = 0.30
  ) +
  geom_raster(
    data = df,
    aes(x = lon, y = lat, fill = amplitude)
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
    name = "Amplitude",
    option = "inferno",
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
  labs(
    title = expression("Amplitude paleoclimática de adequabilidade para " * italic("Dinizia excelsa")),
    subtitle = "Diferença entre maior e menor adequabilidade estimada entre períodos passados",
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
  "figuras/unidade16/unidade16_amplitude_paleoclimatica.png",
  plot = g_amp,
  width = 9,
  height = 7,
  dpi = 600
)

fig_composta <- g_sd / g_amp +
  patchwork::plot_annotation(
    title = expression("Componente paleoclimático de incerteza para " * italic("Dinizia excelsa")),
    subtitle = "Divergência espacial das projeções entre períodos climáticos do passado"
  ) &
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 10)
  )

ggsave(
  "figuras/unidade16/unidade16_incerteza_paleoclimatica_patchwork.png",
  plot = fig_composta,
  width = 9,
  height = 13,
  dpi = 600
)

message("Incerteza paleoclimática calculada com sucesso.")
print(resumo)
