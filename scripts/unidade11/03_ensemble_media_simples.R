source("scripts/_bootstrap.R")

# ============================================================
# Unidade 11 - Modelagem Ensemble em SDM
# Script 03: Ensemble por média simples
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
  "tibble"
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

# ------------------------------------------------------------
# 6. Checagem do stack
# ------------------------------------------------------------

if (terra::nlyr(stack_modelos) < 2) {
  stop("O stack deve conter pelo menos dois modelos para gerar ensemble.")
}

if (is.na(terra::crs(stack_modelos)) || terra::crs(stack_modelos) == "") {
  stop("O stack de predições não possui CRS definido.")
}

nomes_modelos <- names(stack_modelos)

# ------------------------------------------------------------
# 7. Ensemble por média simples
# ------------------------------------------------------------

ensemble_media <- terra::app(
  stack_modelos,
  fun = mean,
  na.rm = TRUE
)

ensemble_media <- terra::clamp(
  ensemble_media,
  lower = 0,
  upper = 1,
  values = TRUE
)

names(ensemble_media) <- "ensemble_media"

terra::writeRaster(
  ensemble_media,
  "resultados/unidade11/ensemble_media_simples_dinizia.tif",
  overwrite = TRUE
)

# ------------------------------------------------------------
# 8. Converter ensemble para tabela
# ------------------------------------------------------------

df <- as.data.frame(
  ensemble_media,
  xy = TRUE,
  na.rm = TRUE
) |>
  rename(
    lon = x,
    lat = y,
    adequabilidade = ensemble_media
  )

write_csv(
  df,
  "dados/unidade11/processados/ensemble_media_simples_dinizia.csv"
)

# ------------------------------------------------------------
# 9. Resumo estatístico do ensemble
# ------------------------------------------------------------

resumo_ensemble <- tibble(
  produto = "Ensemble por média simples",
  modelos_incluidos = paste(nomes_modelos, collapse = ", "),
  n_modelos = terra::nlyr(stack_modelos),
  n_pixels = nrow(df),
  minimo = min(df$adequabilidade, na.rm = TRUE),
  primeiro_quartil = quantile(df$adequabilidade, 0.25, na.rm = TRUE),
  mediana = median(df$adequabilidade, na.rm = TRUE),
  media = mean(df$adequabilidade, na.rm = TRUE),
  terceiro_quartil = quantile(df$adequabilidade, 0.75, na.rm = TRUE),
  maximo = max(df$adequabilidade, na.rm = TRUE),
  desvio_padrao = sd(df$adequabilidade, na.rm = TRUE)
)

write_csv(
  resumo_ensemble,
  "tabelas/unidade11/resumo_ensemble_media_simples.csv"
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
  sf::st_transform(terra::crs(ensemble_media)) |>
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
# 11. Mapa do ensemble por média simples
# ------------------------------------------------------------

g <- ggplot() +
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
      fill = adequabilidade
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
    title = expression("Ensemble por média simples para " * italic("Dinizia excelsa")),
    subtitle = paste0(
      "Média das predições dos modelos: ",
      paste(nomes_modelos, collapse = ", ")
    ),
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

ggsave(
  "figuras/unidade11/unidade11_ensemble_media_simples.png",
  plot = g,
  width = 9,
  height = 7,
  dpi = 600
)

# ------------------------------------------------------------
# 12. Mensagem final
# ------------------------------------------------------------

message("Ensemble por média simples gerado com sucesso.")
message("Modelos incluídos: ", paste(nomes_modelos, collapse = ", "))
message("Número de modelos: ", terra::nlyr(stack_modelos))
message("Número de pixels preditos: ", nrow(df))
message("Raster salvo em: resultados/unidade11/ensemble_media_simples_dinizia.tif")
message("Mapa salvo em: figuras/unidade11/unidade11_ensemble_media_simples.png")

print(resumo_ensemble)
