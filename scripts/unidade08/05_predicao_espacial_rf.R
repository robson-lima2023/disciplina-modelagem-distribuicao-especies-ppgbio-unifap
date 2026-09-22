source("scripts/_bootstrap.R")

# ============================================================
# Unidade 8 - Random Forest em SDM
# Script 05: Predição espacial do Random Forest
# Autor: Robson Borges de Lima
# ============================================================

# ------------------------------------------------------------
# 1. Pacotes
# ------------------------------------------------------------

pacotes <- c(
  "dplyr", "readr", "terra", "sf", "ggplot2",
  "ggspatial", "ranger", "viridis", "tibble"
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

dir.create("dados/unidade08/processados", recursive = TRUE, showWarnings = FALSE)
dir.create("resultados/unidade08", recursive = TRUE, showWarnings = FALSE)
dir.create("figuras/unidade08", recursive = TRUE, showWarnings = FALSE)
dir.create("tabelas/unidade08", recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# 4. Arquivos de entrada
# ------------------------------------------------------------

arquivo_modelo <- "resultados/unidade08/modelo_rf_dinizia.rds"
arquivo_vars   <- "resultados/unidade08/variaveis_rf.csv"

arquivo_amb <- "dados/unidade04/processados/variaveis_ambientais_selecionadas_bioma.tif"

arquivo_m <- "dados/unidade05/processados/area_m_dinizia.gpkg"
arquivo_oc <- "dados/unidade05/processados/presencas_area_m_dinizia.csv"
arquivo_bioma <- "dados/unidade01/brutos/amazon_biome_border.shp"

# ------------------------------------------------------------
# 5. Checagem dos arquivos
# ------------------------------------------------------------

if (!file.exists(arquivo_modelo)) {
  stop("Modelo RF não encontrado. Execute o Script 03 da Unidade 8.")
}

if (!file.exists(arquivo_vars)) {
  stop("Arquivo de variáveis RF não encontrado. Execute o Script 02 da Unidade 8.")
}

if (!file.exists(arquivo_amb)) {
  stop(
    "Raster ambiental selecionado não encontrado: ",
    arquivo_amb,
    "\nExecute antes o script da Unidade 4: 07_gerar_raster_ambiental.R"
  )
}

if (!file.exists(arquivo_oc)) {
  stop("Arquivo de ocorrências não encontrado: ", arquivo_oc)
}

if (!file.exists(arquivo_bioma)) {
  stop("Limite do bioma Amazônia não encontrado: ", arquivo_bioma)
}

if (!file.exists(arquivo_m)) {
  warning("Área M não encontrada. O mapa será gerado sem contorno da área M.")
}

# ------------------------------------------------------------
# 6. Leitura dos dados
# ------------------------------------------------------------

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

# ------------------------------------------------------------
# 7. Verificar variáveis usadas no RF
# ------------------------------------------------------------

vars_ausentes <- setdiff(vars, names(amb_bioma))

if (length(vars_ausentes) > 0) {
  stop(
    "As seguintes variáveis usadas no RF não estão no raster ambiental selecionado: ",
    paste(vars_ausentes, collapse = ", ")
  )
}

amb_bioma <- amb_bioma[[vars]]

if (terra::nlyr(amb_bioma) != length(vars)) {
  stop("Número de camadas do raster diferente do número de variáveis usadas no RF.")
}

# ------------------------------------------------------------
# 8. Corrigir e padronizar limite do bioma
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
# 9. Máscara ambiental pelo bioma
# ------------------------------------------------------------

bioma_vect <- terra::vect(bioma)

amb_bioma <- amb_bioma |>
  terra::crop(bioma_vect) |>
  terra::mask(bioma_vect)

# ------------------------------------------------------------
# 10. Área M e ocorrências
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
# 11. Converter raster ambiental para data.frame
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
# 12. Predição espacial RF
# ------------------------------------------------------------

pred_rf <- predict(
  modelo,
  data = pred_df
)$predictions[, "presenca"]

pred_df$adequabilidade_rf <- as.numeric(pred_rf)

pred_df <- pred_df |>
  filter(
    !is.na(adequabilidade_rf),
    is.finite(adequabilidade_rf)
  )

# ------------------------------------------------------------
# 13. Criar raster de adequabilidade
# ------------------------------------------------------------

r_pred <- amb_bioma[[1]]
terra::values(r_pred) <- NA_real_

celulas <- terra::cellFromXY(
  r_pred,
  as.matrix(pred_df[, c("lon", "lat")])
)

r_pred[celulas] <- pred_df$adequabilidade_rf
names(r_pred) <- "adequabilidade_rf"

terra::writeRaster(
  r_pred,
  "resultados/unidade08/adequabilidade_rf_dinizia.tif",
  overwrite = TRUE
)

write_csv(
  pred_df,
  "dados/unidade08/processados/predicao_espacial_rf_dinizia.csv"
)

# ------------------------------------------------------------
# 14. Resumo da predição espacial
# ------------------------------------------------------------

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
    min(pred_df$adequabilidade_rf, na.rm = TRUE),
    mean(pred_df$adequabilidade_rf, na.rm = TRUE),
    median(pred_df$adequabilidade_rf, na.rm = TRUE),
    max(pred_df$adequabilidade_rf, na.rm = TRUE)
  )
)

write_csv(
  resumo_predicao,
  "tabelas/unidade08/resumo_predicao_espacial_rf.csv"
)

# ------------------------------------------------------------
# 15. Dados para mapa
# ------------------------------------------------------------

map_df <- as.data.frame(
  r_pred,
  xy = TRUE,
  na.rm = TRUE
) |>
  rename(
    lon = x,
    lat = y,
    adequabilidade_rf = adequabilidade_rf
  )

bioma_plot <- sf::st_transform(bioma, 4326)

area_m_plot <- NULL

if (!is.null(area_m)) {
  area_m_plot <- sf::st_transform(area_m, 4326)
}

oc_plot <- sf::st_transform(oc_sf, 4326)

bbox_bioma <- sf::st_bbox(bioma_plot)

# ------------------------------------------------------------
# 16. Mapa refinado RF
# ------------------------------------------------------------

mapa_rf <- ggplot() +
  geom_sf(
    data = bioma_plot,
    fill = "grey96",
    color = "grey35",
    linewidth = 0.35
  ) +
  geom_raster(
    data = map_df,
    aes(x = lon, y = lat, fill = adequabilidade_rf)
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
        " estimada por Random Forest"
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
  filename = "figuras/unidade08/unidade08_adequabilidade_rf.png",
  plot = mapa_rf,
  width = 9,
  height = 7,
  dpi = 600
)

# ------------------------------------------------------------
# 17. Mensagem final
# ------------------------------------------------------------

message("Predição espacial do Random Forest concluída para todo o bioma Amazônia.")
message("Número de variáveis ambientais utilizadas: ", length(vars))
message("Número de pixels preditos: ", nrow(pred_df))
message("Mapa salvo em: figuras/unidade08/unidade08_adequabilidade_rf.png")
message("Raster salvo em: resultados/unidade08/adequabilidade_rf_dinizia.tif")

print(resumo_predicao)
