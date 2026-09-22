source("scripts/_bootstrap.R")

# ============================================================
# Unidade 9 - BRT em SDM
# Script 05: Predição espacial do BRT
# ============================================================

pacotes <- c(
  "dplyr", "readr", "terra", "sf",
  "ggplot2", "ggspatial", "gbm",
  "viridis", "tibble"
)

require_packages(pacotes)
invisible(lapply(pacotes, library, character.only = TRUE))

sf::sf_use_s2(FALSE)
dir.create("dados/unidade09/processados", recursive = TRUE, showWarnings = FALSE)
dir.create("resultados/unidade09", recursive = TRUE, showWarnings = FALSE)
dir.create("figuras/unidade09", recursive = TRUE, showWarnings = FALSE)
dir.create("tabelas/unidade09", recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# 1. Arquivos de entrada
# ------------------------------------------------------------

arquivo_modelo <- "resultados/unidade09/modelo_brt_dinizia.rds"
arquivo_parametros <- "tabelas/unidade09/parametros_brt.csv"
arquivo_vars <- "resultados/unidade09/variaveis_brt.csv"

arquivo_amb <- "dados/unidade04/processados/variaveis_ambientais_selecionadas_bioma.tif"
arquivo_bioma <- "dados/unidade01/brutos/amazon_biome_border.shp"
arquivo_m <- "dados/unidade05/processados/area_m_dinizia.gpkg"
arquivo_oc <- "dados/unidade05/processados/presencas_area_m_dinizia.csv"

arquivos_obrigatorios <- c(
  arquivo_modelo,
  arquivo_parametros,
  arquivo_vars,
  arquivo_amb,
  arquivo_bioma,
  arquivo_oc
)

if (any(!file.exists(arquivos_obrigatorios))) {
  stop(
    "Arquivos ausentes:\n",
    paste(arquivos_obrigatorios[!file.exists(arquivos_obrigatorios)], collapse = "\n")
  )
}

if (!file.exists(arquivo_m)) {
  warning("Área M não encontrada. O mapa será gerado sem o contorno da área M.")
}

# ------------------------------------------------------------
# 2. Leitura dos dados
# ------------------------------------------------------------

modelo <- readRDS(arquivo_modelo)

parametros <- read_csv(
  arquivo_parametros,
  show_col_types = FALSE
)

melhor_iter <- as.numeric(
  parametros$valor[parametros$parametro == "best.trees"]
)

if (length(melhor_iter) == 0 || is.na(melhor_iter)) {
  stop("Número ótimo de árvores não encontrado em parametros_brt.csv.")
}

vars <- read_csv(
  arquivo_vars,
  show_col_types = FALSE
)$variavel |>
  unique()

amb_bioma <- terra::rast(arquivo_amb)

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
# 3. Verificar variáveis ambientais
# ------------------------------------------------------------

vars_ausentes <- setdiff(vars, names(amb_bioma))

if (length(vars_ausentes) > 0) {
  stop(
    "As seguintes variáveis usadas no BRT não estão no raster ambiental selecionado: ",
    paste(vars_ausentes, collapse = ", ")
  )
}

amb_bioma <- amb_bioma[[vars]]

if (terra::nlyr(amb_bioma) != length(vars)) {
  stop("Número de camadas do raster diferente do número de variáveis usadas no BRT.")
}

# ------------------------------------------------------------
# 4. Corrigir e padronizar limite do bioma
# ------------------------------------------------------------

bioma_proj <- bioma_raw |>
  sf::st_transform(5880) |>
  sf::st_make_valid() |>
  sf::st_union() |>
  sf::st_as_sf() |>
  sf::st_make_valid()

bioma <- bioma_proj |>
  sf::st_transform(terra::crs(amb_bioma)) |>
  sf::st_make_valid()

# ------------------------------------------------------------
# 5. Máscara ambiental pelo bioma
# ------------------------------------------------------------

bioma_vect <- terra::vect(bioma)

amb_bioma <- amb_bioma |>
  terra::crop(bioma_vect) |>
  terra::mask(bioma_vect)

# ------------------------------------------------------------
# 6. Área M e ocorrências
# ------------------------------------------------------------

if (!is.null(area_m)) {
  area_m <- area_m |>
    sf::st_make_valid() |>
    sf::st_transform(terra::crs(amb_bioma))
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

oc_sf <- sf::st_transform(
  oc_sf,
  terra::crs(amb_bioma)
)

# ------------------------------------------------------------
# 7. Converter raster ambiental para data.frame
# ------------------------------------------------------------

pred_df <- as.data.frame(
  amb_bioma,
  xy = TRUE,
  na.rm = TRUE
) |>
  rename(
    lon = x,
    lat = y
  )

if (nrow(pred_df) == 0) {
  stop("Nenhum pixel válido disponível para predição espacial.")
}

# ------------------------------------------------------------
# 8. Predição espacial do BRT
# ------------------------------------------------------------

pred_df$adequabilidade_brt <- as.numeric(
  predict(
    modelo,
    newdata = pred_df,
    n.trees = melhor_iter,
    type = "response"
  )
)

pred_df <- pred_df |>
  filter(
    !is.na(adequabilidade_brt),
    is.finite(adequabilidade_brt)
  )

# ------------------------------------------------------------
# 9. Criar raster de adequabilidade
# ------------------------------------------------------------

r_pred <- amb_bioma[[1]]
terra::values(r_pred) <- NA_real_

celulas <- terra::cellFromXY(
  r_pred,
  as.matrix(pred_df[, c("lon", "lat")])
)

r_pred[celulas] <- pred_df$adequabilidade_brt

names(r_pred) <- "adequabilidade_brt"

terra::writeRaster(
  r_pred,
  "resultados/unidade09/adequabilidade_brt_dinizia.tif",
  overwrite = TRUE
)

write_csv(
  pred_df,
  "dados/unidade09/processados/predicao_espacial_brt_dinizia.csv"
)

# ------------------------------------------------------------
# 10. Resumo da predição espacial
# ------------------------------------------------------------

resumo_predicao <- tibble(
  produto = c(
    "Número de variáveis ambientais usadas",
    "Número ótimo de árvores",
    "Número de pixels preditos",
    "Adequabilidade mínima",
    "Adequabilidade média",
    "Adequabilidade mediana",
    "Adequabilidade máxima"
  ),
  valor = c(
    length(vars),
    melhor_iter,
    nrow(pred_df),
    min(pred_df$adequabilidade_brt, na.rm = TRUE),
    mean(pred_df$adequabilidade_brt, na.rm = TRUE),
    median(pred_df$adequabilidade_brt, na.rm = TRUE),
    max(pred_df$adequabilidade_brt, na.rm = TRUE)
  )
)

write_csv(
  resumo_predicao,
  "tabelas/unidade09/resumo_predicao_espacial_brt.csv"
)

# ------------------------------------------------------------
# 11. Preparar dados para mapa
# ------------------------------------------------------------

map_df <- as.data.frame(
  r_pred,
  xy = TRUE,
  na.rm = TRUE
) |>
  rename(
    lon = x,
    lat = y,
    adequabilidade_brt = adequabilidade_brt
  )

bioma_plot <- sf::st_transform(bioma, 4326)

area_m_plot <- NULL

if (!is.null(area_m)) {
  area_m_plot <- sf::st_transform(area_m, 4326)
}

oc_plot <- sf::st_transform(oc_sf, 4326)

bbox_bioma <- sf::st_bbox(bioma_plot)

# ------------------------------------------------------------
# 12. Mapa refinado do BRT
# ------------------------------------------------------------

mapa_brt <- ggplot() +
  geom_sf(
    data = bioma_plot,
    fill = "grey96",
    color = "grey35",
    linewidth = 0.35
  ) +
  geom_raster(
    data = map_df,
    aes(
      x = lon,
      y = lat,
      fill = adequabilidade_brt
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
    data = oc_plot,
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
    title = expression(
      "Adequabilidade ambiental de " *
        italic("Dinizia excelsa") *
        " estimada por BRT"
    ),
    subtitle = "Modelo calibrado com variáveis selecionadas por VIF e projetado para o bioma Amazônia",
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
  "figuras/unidade09/unidade09_adequabilidade_brt.png",
  plot = mapa_brt,
  width = 9,
  height = 7,
  dpi = 600
)

# ------------------------------------------------------------
# 13. Mensagem final
# ------------------------------------------------------------

message("Predição espacial do BRT concluída para todo o bioma Amazônia.")
message("Número de variáveis ambientais utilizadas: ", length(vars))
message("Número ótimo de árvores: ", melhor_iter)
message("Número de pixels preditos: ", nrow(pred_df))
message("Raster salvo em: resultados/unidade09/adequabilidade_brt_dinizia.tif")
message("Mapa salvo em: figuras/unidade09/unidade09_adequabilidade_brt.png")

print(resumo_predicao)
