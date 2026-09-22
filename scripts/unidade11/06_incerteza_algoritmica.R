source("scripts/_bootstrap.R")

# ============================================================
# Unidade 11 - Modelagem Ensemble em SDM
# Script 06: Incerteza algorítmica
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
# 2. Diretório raiz do projeto
# ------------------------------------------------------------
# ------------------------------------------------------------
# 3. Diretórios de saída
# ------------------------------------------------------------

dir.create("dados/unidade11/processados", recursive = TRUE, showWarnings = FALSE)
dir.create("figuras/unidade11", recursive = TRUE, showWarnings = FALSE)
dir.create("resultados/unidade11", recursive = TRUE, showWarnings = FALSE)
dir.create("tabelas/unidade11", recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# 4. Arquivos de entrada
# ------------------------------------------------------------

arquivo_stack <- "dados/unidade11/processados/stack_predicoes_modelos.tif"

arquivo_bioma <- "dados/unidade01/brutos/amazon_biome_border.shp"

arquivo_m <- "dados/unidade05/processados/area_m_dinizia.gpkg"

arquivo_oc <- "dados/unidade05/processados/presencas_area_m_dinizia.csv"

if (!file.exists(arquivo_stack)) {
  stop("Stack de predições não encontrado. Execute o Script 02 da Unidade 11.")
}

if (!file.exists(arquivo_bioma)) {
  stop("Limite do bioma Amazônia não encontrado: ", arquivo_bioma)
}

if (!file.exists(arquivo_oc)) {
  stop("Arquivo de ocorrências não encontrado: ", arquivo_oc)
}

if (!file.exists(arquivo_m)) {
  warning("Área M não encontrada. O mapa será gerado sem contorno da área M.")
}

# ------------------------------------------------------------
# 5. Leitura dos dados
# ------------------------------------------------------------

stack_modelos <- terra::rast(
  arquivo_stack
)

bioma_raw <- sf::st_read(
  arquivo_bioma,
  quiet = TRUE
)

oc <- read_csv(
  arquivo_oc,
  show_col_types = FALSE
)

area_m <- NULL

if (file.exists(arquivo_m)) {
  area_m <- sf::st_read(
    arquivo_m,
    quiet = TRUE
  )
}

if (terra::nlyr(stack_modelos) < 2) {
  stop("O stack deve conter pelo menos dois modelos para calcular incerteza.")
}

nomes_modelos <- names(stack_modelos)

# ------------------------------------------------------------
# 6. Calcular métricas de incerteza
# ------------------------------------------------------------

incerteza_sd <- terra::app(
  stack_modelos,
  fun = sd,
  na.rm = TRUE
)

names(incerteza_sd) <- "incerteza_sd"

amplitude <- terra::app(
  stack_modelos,
  fun = function(x) {
    max(x, na.rm = TRUE) - min(x, na.rm = TRUE)
  }
)

names(amplitude) <- "amplitude_modelos"

media_modelos <- terra::app(
  stack_modelos,
  fun = mean,
  na.rm = TRUE
)

names(media_modelos) <- "media_modelos"

cv_modelos <- incerteza_sd / media_modelos
cv_modelos <- terra::clamp(
  cv_modelos,
  lower = 0,
  upper = 10,
  values = TRUE
)

names(cv_modelos) <- "coeficiente_variacao"

# ------------------------------------------------------------
# 7. Exportar rasters
# ------------------------------------------------------------

terra::writeRaster(
  incerteza_sd,
  "resultados/unidade11/incerteza_algoritmica_dinizia.tif",
  overwrite = TRUE
)

terra::writeRaster(
  amplitude,
  "resultados/unidade11/amplitude_algoritmica_dinizia.tif",
  overwrite = TRUE
)

terra::writeRaster(
  cv_modelos,
  "resultados/unidade11/cv_algoritmico_dinizia.tif",
  overwrite = TRUE
)

# ------------------------------------------------------------
# 8. Converter para tabelas
# ------------------------------------------------------------

df_sd <- as.data.frame(
  incerteza_sd,
  xy = TRUE,
  na.rm = TRUE
) |>
  rename(
    lon = x,
    lat = y,
    incerteza = incerteza_sd
  )

df_amp <- as.data.frame(
  amplitude,
  xy = TRUE,
  na.rm = TRUE
) |>
  rename(
    lon = x,
    lat = y,
    amplitude = amplitude_modelos
  )

df_cv <- as.data.frame(
  cv_modelos,
  xy = TRUE,
  na.rm = TRUE
) |>
  rename(
    lon = x,
    lat = y,
    cv = coeficiente_variacao
  )

write_csv(
  df_sd,
  "dados/unidade11/processados/incerteza_algoritmica_dinizia.csv"
)

write_csv(
  df_amp,
  "dados/unidade11/processados/amplitude_algoritmica_dinizia.csv"
)

write_csv(
  df_cv,
  "dados/unidade11/processados/cv_algoritmico_dinizia.csv"
)

# ------------------------------------------------------------
# 9. Resumo estatístico da incerteza
# ------------------------------------------------------------

resumo_incerteza <- tibble(
  produto = c(
    "Desvio-padrão entre modelos",
    "Amplitude entre modelos",
    "Coeficiente de variação entre modelos"
  ),
  n_modelos = terra::nlyr(stack_modelos),
  modelos_incluidos = paste(nomes_modelos, collapse = ", "),
  n_pixels = c(
    nrow(df_sd),
    nrow(df_amp),
    nrow(df_cv)
  ),
  minimo = c(
    min(df_sd$incerteza, na.rm = TRUE),
    min(df_amp$amplitude, na.rm = TRUE),
    min(df_cv$cv, na.rm = TRUE)
  ),
  primeiro_quartil = c(
    quantile(df_sd$incerteza, 0.25, na.rm = TRUE),
    quantile(df_amp$amplitude, 0.25, na.rm = TRUE),
    quantile(df_cv$cv, 0.25, na.rm = TRUE)
  ),
  mediana = c(
    median(df_sd$incerteza, na.rm = TRUE),
    median(df_amp$amplitude, na.rm = TRUE),
    median(df_cv$cv, na.rm = TRUE)
  ),
  media = c(
    mean(df_sd$incerteza, na.rm = TRUE),
    mean(df_amp$amplitude, na.rm = TRUE),
    mean(df_cv$cv, na.rm = TRUE)
  ),
  terceiro_quartil = c(
    quantile(df_sd$incerteza, 0.75, na.rm = TRUE),
    quantile(df_amp$amplitude, 0.75, na.rm = TRUE),
    quantile(df_cv$cv, 0.75, na.rm = TRUE)
  ),
  maximo = c(
    max(df_sd$incerteza, na.rm = TRUE),
    max(df_amp$amplitude, na.rm = TRUE),
    max(df_cv$cv, na.rm = TRUE)
  ),
  desvio_padrao = c(
    sd(df_sd$incerteza, na.rm = TRUE),
    sd(df_amp$amplitude, na.rm = TRUE),
    sd(df_cv$cv, na.rm = TRUE)
  )
)

write_csv(
  resumo_incerteza,
  "tabelas/unidade11/resumo_incerteza_algoritmica.csv"
)

# ------------------------------------------------------------
# 10. Preparar bioma, área M e ocorrências para mapa
# ------------------------------------------------------------

bioma_proj <- bioma_raw |>
  sf::st_transform(5880) |>
  sf::st_make_valid() |>
  sf::st_union() |>
  sf::st_as_sf() |>
  sf::st_make_valid()

bioma <- bioma_proj |>
  sf::st_transform(terra::crs(incerteza_sd)) |>
  sf::st_make_valid()

bioma_plot <- sf::st_transform(
  bioma,
  4326
)

area_m_plot <- NULL

if (!is.null(area_m)) {
  area_m_plot <- area_m |>
    sf::st_make_valid() |>
    sf::st_transform(4326)
}

if (!all(c("lon", "lat") %in% names(oc))) {
  stop("O arquivo de ocorrências precisa conter as colunas 'lon' e 'lat'.")
}

oc_sf <- sf::st_as_sf(
  oc,
  coords = c("lon", "lat"),
  crs = 4326,
  remove = FALSE
)

bbox_bioma <- sf::st_bbox(bioma_plot)

# ------------------------------------------------------------
# 11. Função auxiliar para mapas
# ------------------------------------------------------------

mapa_incerteza <- function(df, coluna, titulo, subtitulo, legenda) {
  
  ggplot() +
    geom_sf(
      data = bioma_plot,
      fill = "grey96",
      color = "grey35",
      linewidth = 0.35
    ) +
    geom_raster(
      data = df,
      aes(
        x = lon,
        y = lat,
        fill = .data[[coluna]]
      )
    ) +
    {
      if (!is.null(area_m_plot)) {
        geom_sf(
          data = area_m_plot,
          fill = NA,
          color = "grey20",
          linewidth = 0.30,
          linetype = "dashed"
        )
      }
    } +
    geom_sf(
      data = oc_sf,
      color = "black",
      fill = "red",
      shape = 21,
      size = 1.05,
      stroke = 0.20,
      alpha = 0.80
    ) +
    scale_fill_viridis_c(
      name = legenda,
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
      width_hint = 0.28,
      text_cex = 0.65
    ) +
    ggspatial::annotation_north_arrow(
      location = "tr",
      which_north = "true",
      style = ggspatial::north_arrow_fancy_orienteering
    ) +
    labs(
      title = titulo,
      subtitle = subtitulo,
      x = "Longitude",
      y = "Latitude"
    ) +
    theme_bw(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold", size = 13),
      plot.subtitle = element_text(size = 10),
      axis.title = element_text(size = 10),
      axis.text = element_text(size = 9),
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 9),
      legend.position = "right",
      panel.grid.major = element_line(color = "grey88", linewidth = 0.25),
      panel.grid.minor = element_blank()
    )
}

# ------------------------------------------------------------
# 12. Mapas
# ------------------------------------------------------------

g_sd <- mapa_incerteza(
  df = df_sd,
  coluna = "incerteza",
  titulo = expression("Incerteza algorítmica para " * italic("Dinizia excelsa")),
  subtitulo = "Desvio-padrão das predições entre modelos",
  legenda = "DP"
)

g_amp <- mapa_incerteza(
  df = df_amp,
  coluna = "amplitude",
  titulo = "Amplitude entre modelos",
  subtitulo = "Diferença entre a maior e a menor adequabilidade predita",
  legenda = "Amplitude"
)

g_cv <- mapa_incerteza(
  df = df_cv,
  coluna = "cv",
  titulo = "Coeficiente de variação entre modelos",
  subtitulo = "Incerteza relativa à média das predições",
  legenda = "CV"
)

ggsave(
  "figuras/unidade11/unidade11_incerteza_algoritmica.png",
  plot = g_sd,
  width = 9,
  height = 7,
  dpi = 600
)

ggsave(
  "figuras/unidade11/unidade11_amplitude_algoritmica.png",
  plot = g_amp,
  width = 9,
  height = 7,
  dpi = 600
)

ggsave(
  "figuras/unidade11/unidade11_cv_algoritmico.png",
  plot = g_cv,
  width = 9,
  height = 7,
  dpi = 600
)

g_patch <- g_sd + g_amp +
  patchwork::plot_annotation(
    title = expression("Incerteza algorítmica em ensemble SDM para " * italic("Dinizia excelsa")),
    subtitle = "Comparação entre desvio-padrão e amplitude das predições dos modelos"
  ) &
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10)
  )

ggsave(
  "figuras/unidade11/unidade11_incerteza_algoritmica_patchwork.png",
  plot = g_patch,
  width = 14,
  height = 7,
  dpi = 600
)

# ------------------------------------------------------------
# 13. Mensagem final
# ------------------------------------------------------------

message("Incerteza algorítmica gerada com sucesso.")
message("Modelos incluídos: ", paste(nomes_modelos, collapse = ", "))
message("Raster principal salvo em: resultados/unidade11/incerteza_algoritmica_dinizia.tif")
message("Mapa principal salvo em: figuras/unidade11/unidade11_incerteza_algoritmica.png")

print(resumo_incerteza)
