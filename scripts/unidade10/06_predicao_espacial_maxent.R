source("scripts/_bootstrap.R")

# ============================================================
# Unidade 10 - Maxent em SDM
# Script 06: Predição espacial do MaxEnt
# ============================================================

pacotes <- c(
  "dplyr", "readr", "terra", "sf",
  "ggplot2", "ggspatial", "maxnet",
  "viridis", "tibble"
)

require_packages(pacotes)
invisible(lapply(pacotes, library, character.only = TRUE))

sf::sf_use_s2(FALSE)
dir.create("dados/unidade10/processados", recursive = TRUE, showWarnings = FALSE)
dir.create("resultados/unidade10", recursive = TRUE, showWarnings = FALSE)
dir.create("figuras/unidade10", recursive = TRUE, showWarnings = FALSE)
dir.create("tabelas/unidade10", recursive = TRUE, showWarnings = FALSE)

arquivo_modelo <- "resultados/unidade10/modelo_maxent_dinizia.rds"
arquivo_amb <- "dados/unidade04/processados/variaveis_ambientais_selecionadas_bioma.tif"
arquivo_bioma <- "dados/unidade01/brutos/amazon_biome_border.shp"
arquivo_m <- "dados/unidade05/processados/area_m_dinizia.gpkg"
arquivo_oc <- "dados/unidade05/processados/presencas_area_m_dinizia.csv"
arquivo_vars <- "resultados/unidade10/variaveis_maxent.csv"

arquivos_obrigatorios <- c(
  arquivo_modelo,
  arquivo_amb,
  arquivo_bioma,
  arquivo_oc,
  arquivo_vars
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

modelo <- readRDS(arquivo_modelo)

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

vars_ausentes <- setdiff(vars, names(amb_bioma))

if (length(vars_ausentes) > 0) {
  stop(
    "As seguintes variáveis usadas no MaxEnt não estão no raster ambiental selecionado: ",
    paste(vars_ausentes, collapse = ", ")
  )
}

amb_bioma <- amb_bioma[[vars]]

if (terra::nlyr(amb_bioma) != length(vars)) {
  stop("Número de camadas do raster diferente do número de variáveis usadas no MaxEnt.")
}

bioma_proj <- bioma_raw |>
  sf::st_transform(5880) |>
  sf::st_make_valid() |>
  sf::st_union() |>
  sf::st_as_sf() |>
  sf::st_make_valid()

bioma <- bioma_proj |>
  sf::st_transform(terra::crs(amb_bioma)) |>
  sf::st_make_valid()

bioma_vect <- terra::vect(bioma)

amb_bioma <- amb_bioma |>
  terra::crop(bioma_vect) |>
  terra::mask(bioma_vect)

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

x_pred <- pred_df |>
  select(all_of(vars)) |>
  as.data.frame()

for (v in vars) {
  x_pred[[v]] <- as.numeric(x_pred[[v]])
}

pred_df$adequabilidade_maxent <- as.numeric(
  predict(
    modelo,
    newdata = x_pred,
    type = "cloglog",
    clamp = FALSE
  )
)

pred_df <- pred_df |>
  filter(
    !is.na(adequabilidade_maxent),
    is.finite(adequabilidade_maxent)
  )

r_pred <- amb_bioma[[1]]
terra::values(r_pred) <- NA_real_

celulas <- terra::cellFromXY(
  r_pred,
  as.matrix(pred_df[, c("lon", "lat")])
)

r_pred[celulas] <- pred_df$adequabilidade_maxent
names(r_pred) <- "adequabilidade_maxent"

terra::writeRaster(
  r_pred,
  "resultados/unidade10/adequabilidade_maxent_dinizia.tif",
  overwrite = TRUE
)

write_csv(
  pred_df,
  "dados/unidade10/processados/predicao_espacial_maxent_dinizia.csv"
)

resumo_predicao <- tibble(
  produto = c(
    "Número de variáveis ambientais usadas",
    "Número de pixels preditos",
    "Adequabilidade mínima",
    "Adequabilidade média",
    "Adequabilidade mediana",
    "Adequabilidade máxima"
  ),
  valor = c(
    length(vars),
    nrow(pred_df),
    min(pred_df$adequabilidade_maxent, na.rm = TRUE),
    mean(pred_df$adequabilidade_maxent, na.rm = TRUE),
    median(pred_df$adequabilidade_maxent, na.rm = TRUE),
    max(pred_df$adequabilidade_maxent, na.rm = TRUE)
  )
)

write_csv(
  resumo_predicao,
  "tabelas/unidade10/resumo_predicao_espacial_maxent.csv"
)

map_df <- as.data.frame(
  r_pred,
  xy = TRUE,
  na.rm = TRUE
) |>
  rename(
    lon = x,
    lat = y,
    adequabilidade_maxent = adequabilidade_maxent
  )

bioma_plot <- sf::st_transform(bioma, 4326)

area_m_plot <- NULL

if (!is.null(area_m)) {
  area_m_plot <- sf::st_transform(area_m, 4326)
}

oc_plot <- sf::st_transform(oc_sf, 4326)

bbox_bioma <- sf::st_bbox(bioma_plot)

mapa_maxent <- ggplot() +
  geom_sf(
    data = bioma_plot,
    fill = "grey96",
    color = "grey35",
    linewidth = 0.35
  ) +
  geom_raster(
    data = map_df,
    aes(x = lon, y = lat, fill = adequabilidade_maxent)
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
        " estimada por MaxEnt"
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
  "figuras/unidade10/unidade10_adequabilidade_maxent.png",
  plot = mapa_maxent,
  width = 9,
  height = 7,
  dpi = 600
)

message("Predição espacial do MaxEnt concluída para todo o bioma Amazônia.")
message("Número de variáveis ambientais utilizadas: ", length(vars))
message("Número de pixels preditos: ", nrow(pred_df))
message("Raster salvo em: resultados/unidade10/adequabilidade_maxent_dinizia.tif")
message("Mapa salvo em: figuras/unidade10/unidade10_adequabilidade_maxent.png")

print(resumo_predicao)

