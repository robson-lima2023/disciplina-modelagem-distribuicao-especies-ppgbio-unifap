source("scripts/_bootstrap.R")

# ============================================================
# Unidade 16 - Incertezas em SDM
# Script 02: Incerteza algorítmica atual
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
# 4. Arquivos de entrada
# ------------------------------------------------------------

arquivo_stack <- "dados/unidade11/processados/stack_predicoes_modelos.tif"
arquivo_bioma <- "dados/unidade01/brutos/amazon_biome_border.shp"
arquivo_m <- "dados/unidade05/processados/area_m_dinizia.gpkg"
arquivo_oc <- "dados/unidade05/processados/presencas_area_m_dinizia.csv"

if (!file.exists(arquivo_stack)) {
  stop("Stack de predições dos modelos não encontrado. Execute a Unidade 11, Script 02.")
}

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
# 5. Ler rasters e calcular estatísticas
# ------------------------------------------------------------

stack_modelos <- terra::rast(arquivo_stack)

inc_alg <- terra::app(
  stack_modelos,
  fun = sd,
  na.rm = TRUE
)

media_alg <- terra::app(
  stack_modelos,
  fun = mean,
  na.rm = TRUE
)

cv_alg <- inc_alg / media_alg

names(inc_alg) <- "incerteza_algoritmica"
names(media_alg) <- "media_algoritmica"
names(cv_alg) <- "cv_algoritmico"

terra::writeRaster(
  inc_alg,
  "resultados/unidade16/componentes/incerteza_algoritmica_atual.tif",
  overwrite = TRUE
)

terra::writeRaster(
  media_alg,
  "resultados/unidade16/componentes/media_algoritmica_atual.tif",
  overwrite = TRUE
)

terra::writeRaster(
  cv_alg,
  "resultados/unidade16/componentes/cv_algoritmico_atual.tif",
  overwrite = TRUE
)

stack_inc <- c(media_alg, inc_alg, cv_alg)

terra::writeRaster(
  stack_inc,
  "resultados/unidade16/componentes/stack_incerteza_algoritmica_atual.tif",
  overwrite = TRUE
)

# ------------------------------------------------------------
# 6. Exportar tabela espacial
# ------------------------------------------------------------

df <- as.data.frame(
  stack_inc,
  xy = TRUE,
  na.rm = TRUE
) |>
  dplyr::rename(
    lon = x,
    lat = y,
    media = media_algoritmica,
    incerteza = incerteza_algoritmica,
    cv = cv_algoritmico
  )

readr::write_csv(
  df,
  "dados/unidade16/processados/incerteza_algoritmica_atual.csv"
)

# ------------------------------------------------------------
# 7. Resumo estatístico
# ------------------------------------------------------------

resumo <- tibble::tibble(
  componente = c(
    "media_algoritmica",
    "incerteza_algoritmica",
    "cv_algoritmico"
  ),
  minimo = c(
    min(df$media, na.rm = TRUE),
    min(df$incerteza, na.rm = TRUE),
    min(df$cv, na.rm = TRUE)
  ),
  q25 = c(
    quantile(df$media, 0.25, na.rm = TRUE),
    quantile(df$incerteza, 0.25, na.rm = TRUE),
    quantile(df$cv, 0.25, na.rm = TRUE)
  ),
  mediana = c(
    median(df$media, na.rm = TRUE),
    median(df$incerteza, na.rm = TRUE),
    median(df$cv, na.rm = TRUE)
  ),
  media = c(
    mean(df$media, na.rm = TRUE),
    mean(df$incerteza, na.rm = TRUE),
    mean(df$cv, na.rm = TRUE)
  ),
  q75 = c(
    quantile(df$media, 0.75, na.rm = TRUE),
    quantile(df$incerteza, 0.75, na.rm = TRUE),
    quantile(df$cv, 0.75, na.rm = TRUE)
  ),
  maximo = c(
    max(df$media, na.rm = TRUE),
    max(df$incerteza, na.rm = TRUE),
    max(df$cv, na.rm = TRUE)
  ),
  desvio_padrao = c(
    sd(df$media, na.rm = TRUE),
    sd(df$incerteza, na.rm = TRUE),
    sd(df$cv, na.rm = TRUE)
  )
)

readr::write_csv(
  resumo,
  "tabelas/unidade16/resumo_incerteza_algoritmica_atual.csv"
)

# ------------------------------------------------------------
# 8. Elementos cartográficos
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
# 9. Mapa de incerteza algorítmica
# ------------------------------------------------------------

g_inc <- ggplot() +
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
    title = expression("Incerteza algorítmica atual para " * italic("Dinizia excelsa")),
    subtitle = "Desvio-padrão entre GLM, GAM, Random Forest, BRT e MaxEnt",
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
  "figuras/unidade16/unidade16_incerteza_algoritmica_atual.png",
  plot = g_inc,
  width = 9,
  height = 7,
  dpi = 600
)

# ------------------------------------------------------------
# 10. Mapa do coeficiente de variação
# ------------------------------------------------------------

g_cv <- ggplot() +
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
      fill = cv
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
    name = "CV",
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
    title = expression("Incerteza relativa atual para " * italic("Dinizia excelsa")),
    subtitle = "Coeficiente de variação entre algoritmos",
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
  "figuras/unidade16/unidade16_cv_algoritmico_atual.png",
  plot = g_cv,
  width = 9,
  height = 7,
  dpi = 600
)

# ------------------------------------------------------------
# 11. Figura composta
# ------------------------------------------------------------

fig_composta <- g_inc / g_cv +
  patchwork::plot_annotation(
    title = expression("Componentes atuais de incerteza algorítmica para " * italic("Dinizia excelsa")),
    subtitle = "Incerteza absoluta e relativa derivada da divergência entre algoritmos"
  ) &
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 10)
  )

ggsave(
  "figuras/unidade16/unidade16_incerteza_algoritmica_atual_patchwork.png",
  plot = fig_composta,
  width = 9,
  height = 13,
  dpi = 600
)

message("Incerteza algorítmica atual calculada com sucesso.")
print(resumo)
