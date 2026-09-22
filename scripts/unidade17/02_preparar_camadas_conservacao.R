source("scripts/_bootstrap.R")

# ============================================================
# Unidade 17 - Aplicações à conservação
# Script 02: Preparar camadas de conservação
# ============================================================

pacotes <- c(
  "dplyr", "readr", "terra", "sf", "ggplot2",
  "ggspatial", "viridis", "tibble", "patchwork"
)

require_packages(pacotes)
invisible(lapply(pacotes, library, character.only = TRUE))

sf::sf_use_s2(FALSE)
dir.create("dados/unidade17/brutos/areas_protegidas", recursive = TRUE, showWarnings = FALSE)
dir.create("dados/unidade17/processados", recursive = TRUE, showWarnings = FALSE)
dir.create("figuras/unidade17", recursive = TRUE, showWarnings = FALSE)
dir.create("tabelas/unidade17", recursive = TRUE, showWarnings = FALSE)
dir.create("resultados/unidade17/componentes", recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# 1. Arquivos de entrada
# ------------------------------------------------------------

arquivo_area_m <- "dados/unidade05/processados/area_m_dinizia.gpkg"
arquivo_adeq <- "resultados/unidade11/ensemble_media_simples_dinizia.tif"
arquivo_inc <- "resultados/unidade16/incerteza_integrada_dinizia.tif"
arquivo_bioma <- "dados/unidade01/brutos/amazon_biome_border.shp"
arquivo_oc <- "dados/unidade05/processados/presencas_area_m_dinizia.csv"

if (!file.exists(arquivo_area_m)) stop("Área M não encontrada. Execute a Unidade 5.")
if (!file.exists(arquivo_adeq)) stop("Ensemble atual não encontrado. Execute a Unidade 11.")
if (!file.exists(arquivo_inc)) stop("Incerteza integrada não encontrada. Execute a Unidade 16.")
if (!file.exists(arquivo_bioma)) stop("Limite do bioma Amazônia não encontrado.")
if (!file.exists(arquivo_oc)) stop("Arquivo de ocorrências não encontrado.")

# ------------------------------------------------------------
# 2. Leitura das camadas principais
# ------------------------------------------------------------

area_m <- sf::st_read(
  arquivo_area_m,
  quiet = TRUE
) |>
  sf::st_make_valid()

bioma_raw <- sf::st_read(
  arquivo_bioma,
  quiet = TRUE
)

oc <- readr::read_csv(
  arquivo_oc,
  show_col_types = FALSE
)

if (!all(c("lon", "lat") %in% names(oc))) {
  stop("O arquivo de ocorrências precisa conter as colunas 'lon' e 'lat'.")
}

adeq <- terra::rast(arquivo_adeq)
inc <- terra::rast(arquivo_inc)

names(adeq) <- "adequabilidade_atual"
names(inc) <- "incerteza_integrada"

if (!terra::compareGeom(adeq, inc, stopOnError = FALSE)) {
  inc <- terra::resample(
    inc,
    adeq,
    method = "bilinear"
  )
}

area_m_raster <- sf::st_transform(
  area_m,
  terra::crs(adeq)
)

area_m_vect <- terra::vect(area_m_raster)

adeq_m <- terra::crop(
  adeq,
  area_m_vect
)

adeq_m <- terra::mask(
  adeq_m,
  area_m_vect
)

inc_m <- terra::crop(
  inc,
  area_m_vect
)

inc_m <- terra::mask(
  inc_m,
  area_m_vect
)

terra::writeRaster(
  adeq_m,
  "dados/unidade17/processados/adequabilidade_atual_conservacao.tif",
  overwrite = TRUE
)

terra::writeRaster(
  inc_m,
  "dados/unidade17/processados/incerteza_integrada_conservacao.tif",
  overwrite = TRUE
)

# ------------------------------------------------------------
# 3. Áreas protegidas
# ------------------------------------------------------------
# Coloque shapefile ou geopackage de unidades de conservação em:
# dados/unidade17/brutos/areas_protegidas/
# Caso não exista, o script cria uma camada vazia reprodutível.
# ------------------------------------------------------------

pasta_uc <- "dados/unidade17/brutos/areas_protegidas"

arquivos_uc <- if (dir.exists(pasta_uc)) {
  list.files(
    pasta_uc,
    pattern = "\\.(shp|gpkg)$",
    full.names = TRUE,
    recursive = TRUE,
    ignore.case = TRUE
  )
} else {
  character(0)
}

if (length(arquivos_uc) > 0) {
  
  uc <- sf::st_read(
    arquivos_uc[1],
    quiet = TRUE
  ) |>
    sf::st_make_valid() |>
    sf::st_transform(sf::st_crs(area_m))
  
  uc <- suppressWarnings(
    sf::st_intersection(
      uc,
      area_m
    )
  )
  
  origem_uc <- "arquivo_local"
  arquivo_uc_origem <- arquivos_uc[1]
  
} else {
  
  message("Nenhuma camada local de áreas protegidas encontrada. Criando camada didática vazia.")
  
  uc <- area_m |>
    dplyr::slice(0)
  
  origem_uc <- "sem_arquivo_local"
  arquivo_uc_origem <- NA_character_
}

sf::st_write(
  uc,
  "dados/unidade17/processados/areas_protegidas_area_m.gpkg",
  delete_dsn = TRUE,
  quiet = TRUE
)

# ------------------------------------------------------------
# 4. Tabela espacial das camadas raster
# ------------------------------------------------------------

stack_cons <- c(
  adeq_m,
  inc_m
)

df_cons <- as.data.frame(
  stack_cons,
  xy = TRUE,
  na.rm = TRUE
) |>
  dplyr::rename(
    lon = x,
    lat = y
  )

readr::write_csv(
  df_cons,
  "dados/unidade17/processados/camadas_conservacao_raster.csv"
)

# ------------------------------------------------------------
# 5. Metadados
# ------------------------------------------------------------

metadados <- tibble::tibble(
  camada = c(
    "adequabilidade_atual",
    "incerteza_integrada",
    "areas_protegidas"
  ),
  arquivo = c(
    "dados/unidade17/processados/adequabilidade_atual_conservacao.tif",
    "dados/unidade17/processados/incerteza_integrada_conservacao.tif",
    "dados/unidade17/processados/areas_protegidas_area_m.gpkg"
  ),
  origem = c(
    "Unidade 11 - ensemble atual",
    "Unidade 16 - incerteza integrada",
    origem_uc
  ),
  arquivo_origem = c(
    arquivo_adeq,
    arquivo_inc,
    arquivo_uc_origem
  )
)

readr::write_csv(
  metadados,
  "tabelas/unidade17/metadados_camadas_conservacao.csv"
)

# ------------------------------------------------------------
# 6. Elementos cartográficos
# ------------------------------------------------------------

bioma_plot <- bioma_raw |>
  sf::st_transform(5880) |>
  sf::st_make_valid() |>
  sf::st_union() |>
  sf::st_as_sf() |>
  sf::st_make_valid() |>
  sf::st_transform(4326)

bbox_bioma <- sf::st_bbox(bioma_plot)

area_m_plot <- area_m |>
  sf::st_make_valid() |>
  sf::st_transform(4326)

uc_plot <- uc |>
  sf::st_make_valid() |>
  sf::st_transform(4326)

oc_sf <- sf::st_as_sf(
  oc,
  coords = c("lon", "lat"),
  crs = 4326,
  remove = FALSE
)

# ------------------------------------------------------------
# 7. Mapa diagnóstico das camadas de conservação
# ------------------------------------------------------------

g_adeq <- ggplot() +
  geom_sf(data = bioma_plot, fill = "grey96", color = "grey35", linewidth = 0.30) +
  geom_raster(data = df_cons, aes(x = lon, y = lat, fill = adequabilidade_atual)) +
  geom_sf(data = area_m_plot, fill = NA, color = "grey20", linewidth = 0.25, linetype = "dashed") +
  {
    if (nrow(uc_plot) > 0) {
      geom_sf(data = uc_plot, fill = NA, color = "#1A9850", linewidth = 0.35)
    }
  } +
  geom_sf(data = oc_sf, color = "black", fill = "red", shape = 21, size = 0.50, stroke = 0.13, alpha = 0.65) +
  scale_fill_viridis_c(name = "Adequabilidade", option = "viridis", limits = c(0, 1), na.value = NA) +
  coord_sf(xlim = c(bbox_bioma["xmin"], bbox_bioma["xmax"]),
           ylim = c(bbox_bioma["ymin"], bbox_bioma["ymax"]),
           expand = FALSE) +
  ggspatial::annotation_scale(location = "bl", width_hint = 0.25, text_cex = 0.60) +
  labs(
    title = "Adequabilidade atual",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_bw(base_size = 10) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

g_inc <- ggplot() +
  geom_sf(data = bioma_plot, fill = "grey96", color = "grey35", linewidth = 0.30) +
  geom_raster(data = df_cons, aes(x = lon, y = lat, fill = incerteza_integrada)) +
  geom_sf(data = area_m_plot, fill = NA, color = "grey20", linewidth = 0.25, linetype = "dashed") +
  {
    if (nrow(uc_plot) > 0) {
      geom_sf(data = uc_plot, fill = NA, color = "#1A9850", linewidth = 0.35)
    }
  } +
  geom_sf(data = oc_sf, color = "black", fill = "red", shape = 21, size = 0.50, stroke = 0.13, alpha = 0.65) +
  scale_fill_viridis_c(name = "Incerteza", option = "magma", limits = c(0, 1), na.value = NA) +
  coord_sf(xlim = c(bbox_bioma["xmin"], bbox_bioma["xmax"]),
           ylim = c(bbox_bioma["ymin"], bbox_bioma["ymax"]),
           expand = FALSE) +
  ggspatial::annotation_scale(location = "bl", width_hint = 0.25, text_cex = 0.60) +
  labs(
    title = "Incerteza integrada",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_bw(base_size = 10) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

fig <- g_adeq + g_inc +
  patchwork::plot_annotation(
    title = expression("Camadas de conservação para " * italic("Dinizia excelsa")),
    subtitle = "Adequabilidade atual, incerteza integrada, área M, ocorrências e áreas protegidas disponíveis"
  ) &
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 10)
  )

ggsave(
  "figuras/unidade17/unidade17_camadas_conservacao.png",
  plot = fig,
  width = 14,
  height = 7,
  dpi = 600
)

message("Camadas de conservação preparadas com sucesso.")
print(metadados)
