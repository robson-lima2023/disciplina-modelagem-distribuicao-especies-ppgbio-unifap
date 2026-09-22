source("scripts/_bootstrap.R")

# ============================================================
# Unidade 16 - Incertezas em SDM
# Script 03: Incerteza entre cenários futuros
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
  "patchwork"
)

require_packages(pacotes)
invisible(lapply(pacotes, library, character.only = TRUE))

sf::sf_use_s2(FALSE)

# ------------------------------------------------------------
# 2. Diretório raiz
# ------------------------------------------------------------
# ------------------------------------------------------------
# 3. Diretórios
# ------------------------------------------------------------

dir.create("dados/unidade16/processados", recursive = TRUE, showWarnings = FALSE)
dir.create("figuras/unidade16", recursive = TRUE, showWarnings = FALSE)
dir.create("tabelas/unidade16", recursive = TRUE, showWarnings = FALSE)
dir.create("resultados/unidade16/componentes", recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# 4. Configurações
# ------------------------------------------------------------

cenarios <- c("ssp126", "ssp245", "ssp370", "ssp585")

periodo <- "2061-2080"

modelo_gcm <- "MIROC6"

arquivo_bioma <- "dados/unidade01/brutos/amazon_biome_border.shp"
arquivo_m <- "dados/unidade05/processados/area_m_dinizia.gpkg"
arquivo_oc <- "dados/unidade05/processados/presencas_area_m_dinizia.csv"

if (!file.exists(arquivo_bioma)) {
  stop("Limite do bioma Amazônia não encontrado.")
}

if (!file.exists(arquivo_oc)) {
  stop("Arquivo de ocorrências não encontrado.")
}

if (!file.exists(arquivo_m)) {
  warning("Área M não encontrada. O mapa será gerado sem contorno da área M.")
}

# ------------------------------------------------------------
# 5. Localizar ensembles futuros
# ------------------------------------------------------------

arquivos <- paste0(
  "resultados/unidade13/ensemble/ensemble_futuro_",
  cenarios,
  "_",
  periodo,
  "_",
  modelo_gcm,
  ".tif"
)

# Compatibilidade com nomes antigos
arquivos_antigos <- paste0(
  "resultados/unidade13/ensemble/ensemble_futuro_",
  cenarios,
  ".tif"
)

arquivos_finais <- ifelse(
  file.exists(arquivos),
  arquivos,
  arquivos_antigos
)

faltantes <- arquivos_finais[!file.exists(arquivos_finais)]

if (length(faltantes) > 0) {
  stop(
    "Ensembles futuros faltantes. Execute a Unidade 13. Arquivos ausentes: ",
    paste(faltantes, collapse = ", ")
  )
}

# ------------------------------------------------------------
# 6. Ler, alinhar e empilhar cenários
# ------------------------------------------------------------

rasters <- lapply(
  arquivos_finais,
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

stack_cenarios <- terra::rast(rasters)
names(stack_cenarios) <- cenarios

terra::writeRaster(
  stack_cenarios,
  "resultados/unidade16/componentes/stack_ensembles_futuros_cenarios.tif",
  overwrite = TRUE
)

# ------------------------------------------------------------
# 7. Componentes de incerteza entre cenários
# ------------------------------------------------------------

media_cenarios <- terra::app(
  stack_cenarios,
  fun = mean,
  na.rm = TRUE
)

inc_cenarios <- terra::app(
  stack_cenarios,
  fun = sd,
  na.rm = TRUE
)

amplitude_cenarios <- terra::app(
  stack_cenarios,
  fun = function(x) {
    max(x, na.rm = TRUE) - min(x, na.rm = TRUE)
  }
)

cv_cenarios <- inc_cenarios / media_cenarios

names(media_cenarios) <- "media_cenarios"
names(inc_cenarios) <- "incerteza_cenarios"
names(amplitude_cenarios) <- "amplitude_cenarios"
names(cv_cenarios) <- "cv_cenarios"

terra::writeRaster(
  inc_cenarios,
  "resultados/unidade16/componentes/incerteza_cenarios_futuros.tif",
  overwrite = TRUE
)

terra::writeRaster(
  amplitude_cenarios,
  "resultados/unidade16/componentes/amplitude_cenarios_futuros.tif",
  overwrite = TRUE
)

terra::writeRaster(
  cv_cenarios,
  "resultados/unidade16/componentes/cv_cenarios_futuros.tif",
  overwrite = TRUE
)

stack_inc <- c(
  media_cenarios,
  inc_cenarios,
  amplitude_cenarios,
  cv_cenarios
)

terra::writeRaster(
  stack_inc,
  "resultados/unidade16/componentes/stack_incerteza_cenarios_futuros.tif",
  overwrite = TRUE
)

# ------------------------------------------------------------
# 8. Exportar tabela
# ------------------------------------------------------------

df <- as.data.frame(
  stack_inc,
  xy = TRUE,
  na.rm = TRUE
) |>
  dplyr::rename(
    lon = x,
    lat = y,
    media = media_cenarios,
    incerteza = incerteza_cenarios,
    amplitude = amplitude_cenarios,
    cv = cv_cenarios
  )

readr::write_csv(
  df,
  "dados/unidade16/processados/incerteza_cenarios_futuros.csv"
)

# ------------------------------------------------------------
# 9. Resumo estatístico
# ------------------------------------------------------------

resumo <- tibble::tibble(
  componente = c(
    "media_cenarios",
    "incerteza_cenarios",
    "amplitude_cenarios",
    "cv_cenarios"
  ),
  minimo = c(
    min(df$media, na.rm = TRUE),
    min(df$incerteza, na.rm = TRUE),
    min(df$amplitude, na.rm = TRUE),
    min(df$cv, na.rm = TRUE)
  ),
  q25 = c(
    quantile(df$media, 0.25, na.rm = TRUE),
    quantile(df$incerteza, 0.25, na.rm = TRUE),
    quantile(df$amplitude, 0.25, na.rm = TRUE),
    quantile(df$cv, 0.25, na.rm = TRUE)
  ),
  mediana = c(
    median(df$media, na.rm = TRUE),
    median(df$incerteza, na.rm = TRUE),
    median(df$amplitude, na.rm = TRUE),
    median(df$cv, na.rm = TRUE)
  ),
  media = c(
    mean(df$media, na.rm = TRUE),
    mean(df$incerteza, na.rm = TRUE),
    mean(df$amplitude, na.rm = TRUE),
    mean(df$cv, na.rm = TRUE)
  ),
  q75 = c(
    quantile(df$media, 0.75, na.rm = TRUE),
    quantile(df$incerteza, 0.75, na.rm = TRUE),
    quantile(df$amplitude, 0.75, na.rm = TRUE),
    quantile(df$cv, 0.75, na.rm = TRUE)
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
  "tabelas/unidade16/resumo_incerteza_cenarios_futuros.csv"
)

# ------------------------------------------------------------
# 10. Elementos cartográficos
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
# 11. Mapa de desvio-padrão entre cenários
# ------------------------------------------------------------

g_sd <- ggplot() +
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
    title = expression("Incerteza entre cenários futuros para " * italic("Dinizia excelsa")),
    subtitle = paste0(
      "Desvio-padrão entre SSP126, SSP245, SSP370 e SSP585 | ",
      periodo,
      " | ",
      modelo_gcm
    ),
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
  "figuras/unidade16/unidade16_incerteza_cenarios_futuros.png",
  plot = g_sd,
  width = 9,
  height = 7,
  dpi = 600
)

# ------------------------------------------------------------
# 12. Mapa de amplitude entre cenários
# ------------------------------------------------------------

g_amp <- ggplot() +
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
      fill = amplitude
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
    title = expression("Amplitude de adequabilidade entre cenários para " * italic("Dinizia excelsa")),
    subtitle = "Diferença entre o maior e o menor valor projetado entre cenários SSP",
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
  "figuras/unidade16/unidade16_amplitude_cenarios_futuros.png",
  plot = g_amp,
  width = 9,
  height = 7,
  dpi = 600
)

# ------------------------------------------------------------
# 13. Figura composta
# ------------------------------------------------------------

fig_composta <- g_sd / g_amp +
  patchwork::plot_annotation(
    title = expression("Incerteza climática entre cenários futuros para " * italic("Dinizia excelsa")),
    subtitle = "Divergência das projeções associada aos caminhos SSP"
  ) &
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 10)
  )

ggsave(
  "figuras/unidade16/unidade16_incerteza_cenarios_futuros_patchwork.png",
  plot = fig_composta,
  width = 9,
  height = 13,
  dpi = 600
)

message("Incerteza entre cenários futuros calculada com sucesso.")
print(resumo)
