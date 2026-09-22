source("scripts/_bootstrap.R")

# ============================================================
# Unidade 17 - Aplicações à conservação
# Script 04: Estabilidade futura para conservação
# ============================================================

pacotes <- c(
  "dplyr", "readr", "terra", "sf", "ggplot2",
  "ggspatial", "viridis", "tidyr", "tibble", "patchwork"
)

require_packages(pacotes)
invisible(lapply(pacotes, library, character.only = TRUE))

sf::sf_use_s2(FALSE)
dir.create("dados/unidade17/processados", recursive = TRUE, showWarnings = FALSE)
dir.create("figuras/unidade17", recursive = TRUE, showWarnings = FALSE)
dir.create("tabelas/unidade17", recursive = TRUE, showWarnings = FALSE)
dir.create("resultados/unidade17/componentes", recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# 1. Configurações
# ------------------------------------------------------------

cenarios <- c("ssp126", "ssp245", "ssp370", "ssp585")

periodo <- "2061-2080"

modelo_gcm <- "MIROC6"

arquivo_bioma <- "dados/unidade01/brutos/amazon_biome_border.shp"
arquivo_area_m <- "dados/unidade05/processados/area_m_dinizia.gpkg"
arquivo_uc <- "dados/unidade17/processados/areas_protegidas_area_m.gpkg"
arquivo_oc <- "dados/unidade05/processados/presencas_area_m_dinizia.csv"

if (!file.exists(arquivo_bioma)) stop("Limite do bioma Amazônia não encontrado.")
if (!file.exists(arquivo_area_m)) stop("Área M não encontrada.")
if (!file.exists(arquivo_oc)) stop("Arquivo de ocorrências não encontrado.")

# ------------------------------------------------------------
# 2. Localizar ensembles futuros
# ------------------------------------------------------------

arquivos_novos <- paste0(
  "resultados/unidade13/ensemble/ensemble_futuro_",
  cenarios,
  "_",
  periodo,
  "_",
  modelo_gcm,
  ".tif"
)

arquivos_antigos <- paste0(
  "resultados/unidade13/ensemble/ensemble_futuro_",
  cenarios,
  ".tif"
)

arquivos <- ifelse(
  file.exists(arquivos_novos),
  arquivos_novos,
  arquivos_antigos
)

faltantes <- arquivos[!file.exists(arquivos)]

if (length(faltantes) > 0) {
  stop(
    "Ensembles futuros faltantes. Execute a Unidade 13. Arquivos ausentes: ",
    paste(faltantes, collapse = ", ")
  )
}

# ------------------------------------------------------------
# 3. Ler e alinhar rasters
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

stack_fut <- terra::rast(rasters)
names(stack_fut) <- cenarios

terra::writeRaster(
  stack_fut,
  "resultados/unidade17/componentes/stack_ensembles_futuros_conservacao.tif",
  overwrite = TRUE
)

# ------------------------------------------------------------
# 4. Estabilidade futura
# ------------------------------------------------------------
# Média entre cenários: maior valor indica adequabilidade mantida
# em diferentes cenários SSP.
# ------------------------------------------------------------

estabilidade <- terra::app(
  stack_fut,
  fun = mean,
  na.rm = TRUE
)

inc_fut <- terra::app(
  stack_fut,
  fun = sd,
  na.rm = TRUE
)

amplitude_fut <- terra::app(
  stack_fut,
  fun = function(x) {
    max(x, na.rm = TRUE) - min(x, na.rm = TRUE)
  }
)

names(estabilidade) <- "estabilidade_futura_media"
names(inc_fut) <- "incerteza_futura"
names(amplitude_fut) <- "amplitude_futura"

terra::writeRaster(
  estabilidade,
  "resultados/unidade17/componentes/estabilidade_futura_media.tif",
  overwrite = TRUE
)

terra::writeRaster(
  inc_fut,
  "resultados/unidade17/componentes/incerteza_futura_conservacao.tif",
  overwrite = TRUE
)

terra::writeRaster(
  amplitude_fut,
  "resultados/unidade17/componentes/amplitude_futura_conservacao.tif",
  overwrite = TRUE
)

stack_saida <- c(
  stack_fut,
  estabilidade,
  inc_fut,
  amplitude_fut
)

terra::writeRaster(
  stack_saida,
  "resultados/unidade17/componentes/stack_estabilidade_futura_conservacao.tif",
  overwrite = TRUE
)

# ------------------------------------------------------------
# 5. Exportar tabela espacial
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
  "dados/unidade17/processados/estabilidade_futura_conservacao.csv"
)

df_long <- df |>
  dplyr::select(
    lon,
    lat,
    dplyr::all_of(cenarios)
  ) |>
  tidyr::pivot_longer(
    cols = dplyr::all_of(cenarios),
    names_to = "cenario",
    values_to = "adequabilidade"
  )

readr::write_csv(
  df_long,
  "dados/unidade17/processados/ensembles_futuros_conservacao_long.csv"
)

# ------------------------------------------------------------
# 6. Resumo estatístico
# ------------------------------------------------------------

resumo <- df |>
  tidyr::pivot_longer(
    cols = -c(lon, lat),
    names_to = "camada",
    values_to = "valor"
  ) |>
  dplyr::group_by(camada) |>
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
  "tabelas/unidade17/resumo_estabilidade_futura_conservacao.csv"
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

uc_plot <- NULL

if (file.exists(arquivo_uc)) {
  uc_plot <- sf::st_read(
    arquivo_uc,
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
# 8. Mapa dos cenários futuros
# ------------------------------------------------------------

g_cenarios <- ggplot() +
  geom_sf(data = bioma_plot, fill = "grey96", color = "grey35", linewidth = 0.25) +
  geom_raster(data = df_long, aes(x = lon, y = lat, fill = adequabilidade)) +
  geom_sf(data = area_m_plot, fill = NA, color = "grey20", linewidth = 0.20, linetype = "dashed") +
  {
    if (!is.null(uc_plot) && nrow(uc_plot) > 0) {
      geom_sf(data = uc_plot, fill = NA, color = "#1A9850", linewidth = 0.30)
    }
  } +
  scale_fill_viridis_c(name = "Adequabilidade", option = "viridis", limits = c(0, 1), na.value = NA) +
  coord_sf(xlim = c(bbox_bioma["xmin"], bbox_bioma["xmax"]),
           ylim = c(bbox_bioma["ymin"], bbox_bioma["ymax"]),
           expand = FALSE) +
  facet_wrap(~ cenario, ncol = 2) +
  ggspatial::annotation_scale(location = "bl", width_hint = 0.22, text_cex = 0.55) +
  labs(
    title = expression("Ensembles futuros para conservação de " * italic("Dinizia excelsa")),
    subtitle = paste0("Cenários SSP | ", periodo, " | ", modelo_gcm),
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_bw(base_size = 10) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10),
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave(
  "figuras/unidade17/unidade17_estabilidade_futura_conservacao.png",
  plot = g_cenarios,
  width = 12,
  height = 9,
  dpi = 600
)

# ------------------------------------------------------------
# 9. Mapa de estabilidade média e incerteza futura
# ------------------------------------------------------------

g_est <- ggplot() +
  geom_sf(data = bioma_plot, fill = "grey96", color = "grey35", linewidth = 0.30) +
  geom_raster(data = df, aes(x = lon, y = lat, fill = estabilidade_futura_media)) +
  geom_sf(data = area_m_plot, fill = NA, color = "grey20", linewidth = 0.25, linetype = "dashed") +
  {
    if (!is.null(uc_plot) && nrow(uc_plot) > 0) {
      geom_sf(data = uc_plot, fill = NA, color = "#1A9850", linewidth = 0.35)
    }
  } +
  geom_sf(data = oc_sf, color = "black", fill = "red", shape = 21, size = 0.50, stroke = 0.13, alpha = 0.65) +
  scale_fill_viridis_c(name = "Estabilidade", option = "viridis", limits = c(0, 1), na.value = NA) +
  coord_sf(xlim = c(bbox_bioma["xmin"], bbox_bioma["xmax"]),
           ylim = c(bbox_bioma["ymin"], bbox_bioma["ymax"]),
           expand = FALSE) +
  ggspatial::annotation_scale(location = "bl", width_hint = 0.25, text_cex = 0.60) +
  labs(
    title = expression("Estabilidade futura média para " * italic("Dinizia excelsa")),
    subtitle = "Média dos ensembles futuros entre cenários SSP",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_bw(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10),
    panel.grid.minor = element_blank()
  )

g_inc <- ggplot() +
  geom_sf(data = bioma_plot, fill = "grey96", color = "grey35", linewidth = 0.30) +
  geom_raster(data = df, aes(x = lon, y = lat, fill = incerteza_futura)) +
  geom_sf(data = area_m_plot, fill = NA, color = "grey20", linewidth = 0.25, linetype = "dashed") +
  {
    if (!is.null(uc_plot) && nrow(uc_plot) > 0) {
      geom_sf(data = uc_plot, fill = NA, color = "#1A9850", linewidth = 0.35)
    }
  } +
  geom_sf(data = oc_sf, color = "black", fill = "red", shape = 21, size = 0.50, stroke = 0.13, alpha = 0.65) +
  scale_fill_viridis_c(name = "Incerteza", option = "magma", na.value = NA) +
  coord_sf(xlim = c(bbox_bioma["xmin"], bbox_bioma["xmax"]),
           ylim = c(bbox_bioma["ymin"], bbox_bioma["ymax"]),
           expand = FALSE) +
  ggspatial::annotation_scale(location = "bl", width_hint = 0.25, text_cex = 0.60) +
  labs(
    title = expression("Incerteza futura entre cenários para " * italic("Dinizia excelsa")),
    subtitle = "Desvio-padrão entre cenários SSP",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_bw(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10),
    panel.grid.minor = element_blank()
  )

fig <- g_est + g_inc +
  patchwork::plot_annotation(
    title = expression("Estabilidade futura e incerteza climática para conservação de " * italic("Dinizia excelsa")),
    subtitle = "Síntese espacial dos ensembles futuros sob cenários SSP"
  ) &
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 10)
  )

ggsave(
  "figuras/unidade17/unidade17_estabilidade_incerteza_futura_patchwork.png",
  plot = fig,
  width = 14,
  height = 7,
  dpi = 600
)

message("Estabilidade futura para conservação calculada com sucesso.")
print(resumo)
