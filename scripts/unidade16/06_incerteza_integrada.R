source("scripts/_bootstrap.R")

# ============================================================
# Unidade 16 - Incertezas em SDM
# Script 06: Incerteza integrada
# ============================================================

pacotes <- c(
  "dplyr", "readr", "terra", "sf", "ggplot2",
  "ggspatial", "viridis", "tibble", "tidyr", "patchwork"
)

require_packages(pacotes)
invisible(lapply(pacotes, library, character.only = TRUE))

sf::sf_use_s2(FALSE)
dir.create("dados/unidade16/processados", recursive = TRUE, showWarnings = FALSE)
dir.create("figuras/unidade16", recursive = TRUE, showWarnings = FALSE)
dir.create("tabelas/unidade16", recursive = TRUE, showWarnings = FALSE)
dir.create("resultados/unidade16", recursive = TRUE, showWarnings = FALSE)
dir.create("resultados/unidade16/componentes", recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# 1. Arquivos de entrada
# ------------------------------------------------------------

arquivos <- c(
  algoritmica = "resultados/unidade16/componentes/incerteza_algoritmica_atual.tif",
  cenarios = "resultados/unidade16/componentes/incerteza_cenarios_futuros.tif",
  extrapolacao = "resultados/unidade16/componentes/incerteza_extrapolacao.tif",
  paleoclimatica = "resultados/unidade16/componentes/incerteza_paleoclimatica.tif"
)

arquivo_bioma <- "dados/unidade01/brutos/amazon_biome_border.shp"
arquivo_m <- "dados/unidade05/processados/area_m_dinizia.gpkg"
arquivo_oc <- "dados/unidade05/processados/presencas_area_m_dinizia.csv"

faltantes <- arquivos[!file.exists(arquivos)]

if (length(faltantes) > 0) {
  stop(
    "Componentes de incerteza faltantes. Execute os Scripts 02 a 05 da Unidade 16: ",
    paste(names(faltantes), collapse = ", ")
  )
}

if (!file.exists(arquivo_bioma)) stop("Limite do bioma Amazônia não encontrado.")
if (!file.exists(arquivo_oc)) stop("Arquivo de ocorrências não encontrado.")

# ------------------------------------------------------------
# 2. Ler, alinhar e normalizar componentes
# ------------------------------------------------------------

rasters <- lapply(arquivos, terra::rast)

ref <- rasters[[1]]

rasters <- lapply(rasters, function(r) {
  if (!terra::compareGeom(ref, r, stopOnError = FALSE)) {
    terra::resample(r, ref, method = "bilinear")
  } else {
    r
  }
})

normalizar_raster <- function(r) {
  
  mn <- as.numeric(terra::global(r, "min", na.rm = TRUE)[1, 1])
  mx <- as.numeric(terra::global(r, "max", na.rm = TRUE)[1, 1])
  
  if (is.na(mn) || is.na(mx) || !is.finite(mn) || !is.finite(mx) || mx == mn) {
    out <- r * 0
    return(out)
  }
  
  (r - mn) / (mx - mn)
}

rasters_n <- lapply(rasters, normalizar_raster)

stack_componentes <- terra::rast(rasters_n)
names(stack_componentes) <- names(arquivos)

terra::writeRaster(
  stack_componentes,
  "resultados/unidade16/componentes/stack_componentes_incerteza_normalizados.tif",
  overwrite = TRUE
)

# ------------------------------------------------------------
# 3. Incerteza integrada
# ------------------------------------------------------------

inc_integrada <- terra::app(
  stack_componentes,
  fun = mean,
  na.rm = TRUE
)

names(inc_integrada) <- "incerteza_integrada"

terra::writeRaster(
  inc_integrada,
  "resultados/unidade16/incerteza_integrada_dinizia.tif",
  overwrite = TRUE
)

stack_saida <- c(stack_componentes, inc_integrada)

terra::writeRaster(
  stack_saida,
  "resultados/unidade16/stack_incerteza_integrada_componentes.tif",
  overwrite = TRUE
)

# ------------------------------------------------------------
# 4. Exportar tabela
# ------------------------------------------------------------

df <- as.data.frame(
  stack_saida,
  xy = TRUE,
  na.rm = TRUE
) |>
  dplyr::rename(
    lon = x,
    lat = y
  )

readr::write_csv(
  df,
  "dados/unidade16/processados/incerteza_integrada_componentes.csv"
)

readr::write_csv(
  df |>
    dplyr::select(lon, lat, incerteza_integrada),
  "dados/unidade16/processados/incerteza_integrada.csv"
)

# ------------------------------------------------------------
# 5. Resumo estatístico
# ------------------------------------------------------------

resumo <- df |>
  tidyr::pivot_longer(
    cols = -c(lon, lat),
    names_to = "componente",
    values_to = "valor"
  ) |>
  dplyr::group_by(componente) |>
  dplyr::summarise(
    minimo = min(valor, na.rm = TRUE),
    q25 = quantile(valor, 0.25, na.rm = TRUE),
    mediana = median(valor, na.rm = TRUE),
    media = mean(valor, na.rm = TRUE),
    q75 = quantile(valor, 0.75, na.rm = TRUE),
    maximo = max(valor, na.rm = TRUE),
    desvio_padrao = sd(valor, na.rm = TRUE),
    .groups = "drop"
  )

readr::write_csv(
  resumo,
  "tabelas/unidade16/resumo_incerteza_integrada.csv"
)

# ------------------------------------------------------------
# 6. Elementos cartográficos
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
# 7. Mapa da incerteza integrada
# ------------------------------------------------------------

g_integrada <- ggplot() +
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
      fill = incerteza_integrada
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
    name = "Incerteza integrada",
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
    title = expression("Incerteza integrada para " * italic("Dinizia excelsa")),
    subtitle = "Síntese normalizada de incerteza algorítmica, cenários, extrapolação e paleoclima",
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
  "figuras/unidade16/unidade16_incerteza_integrada.png",
  plot = g_integrada,
  width = 9,
  height = 7,
  dpi = 600
)

# ------------------------------------------------------------
# 8. Mapa comparativo dos componentes
# ------------------------------------------------------------

df_comp <- df |>
  dplyr::select(
    lon,
    lat,
    algoritmica,
    cenarios,
    extrapolacao,
    paleoclimatica
  ) |>
  tidyr::pivot_longer(
    cols = -c(lon, lat),
    names_to = "componente",
    values_to = "valor"
  ) |>
  dplyr::mutate(
    componente = dplyr::case_when(
      componente == "algoritmica" ~ "Algorítmica",
      componente == "cenarios" ~ "Cenários futuros",
      componente == "extrapolacao" ~ "Extrapolação",
      componente == "paleoclimatica" ~ "Paleoclimática",
      TRUE ~ componente
    )
  )

g_componentes <- ggplot() +
  geom_sf(
    data = bioma_plot,
    fill = "grey96",
    color = "grey35",
    linewidth = 0.25
  ) +
  geom_raster(
    data = df_comp,
    aes(
      x = lon,
      y = lat,
      fill = valor
    )
  ) +
  {
    if (!is.null(area_m_plot)) {
      geom_sf(
        data = area_m_plot,
        fill = NA,
        color = "grey20",
        linewidth = 0.20,
        linetype = "dashed"
      )
    }
  } +
  scale_fill_viridis_c(
    name = "Valor normalizado",
    option = "magma",
    limits = c(0, 1),
    na.value = NA
  ) +
  coord_sf(
    xlim = c(bbox_bioma["xmin"], bbox_bioma["xmax"]),
    ylim = c(bbox_bioma["ymin"], bbox_bioma["ymax"]),
    expand = FALSE
  ) +
  facet_wrap(
    ~ componente,
    ncol = 2
  ) +
  labs(
    title = expression("Componentes normalizados de incerteza para " * italic("Dinizia excelsa")),
    subtitle = "Comparação espacial dos componentes usados na incerteza integrada",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_bw(base_size = 10) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10),
    strip.text = element_text(face = "bold"),
    panel.grid.major = element_line(color = "grey88", linewidth = 0.15),
    panel.grid.minor = element_blank()
  )

ggsave(
  "figuras/unidade16/unidade16_componentes_incerteza_normalizados.png",
  plot = g_componentes,
  width = 12,
  height = 9,
  dpi = 600
)

fig_composta <- g_componentes / g_integrada +
  patchwork::plot_layout(
    heights = c(1.25, 1)
  ) +
  patchwork::plot_annotation(
    title = expression("Síntese integrada das incertezas em SDM para " * italic("Dinizia excelsa")),
    subtitle = "Componentes normalizados e mapa final de incerteza integrada"
  ) &
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 10)
  )

ggsave(
  "figuras/unidade16/unidade16_incerteza_integrada_patchwork.png",
  plot = fig_composta,
  width = 12,
  height = 16,
  dpi = 600
)

message("Incerteza integrada calculada com sucesso.")
print(resumo)
