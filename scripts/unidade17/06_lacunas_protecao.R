source("scripts/_bootstrap.R")

# ============================================================
# Unidade 17 - Aplicações à conservação
# Script 06: Lacunas de proteção
# ============================================================

pacotes <- c(
  "dplyr", "readr", "terra", "sf", "ggplot2",
  "ggspatial", "tibble", "patchwork"
)

require_packages(pacotes)
invisible(lapply(pacotes, library, character.only = TRUE))

sf::sf_use_s2(FALSE)
dir.create("dados/unidade17/processados", recursive = TRUE, showWarnings = FALSE)
dir.create("figuras/unidade17", recursive = TRUE, showWarnings = FALSE)
dir.create("tabelas/unidade17", recursive = TRUE, showWarnings = FALSE)
dir.create("resultados/unidade17/componentes", recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# 1. Arquivos de entrada
# ------------------------------------------------------------

arquivo_prioridade <- "resultados/unidade17/componentes/prioridade_climatica_atual.tif"
arquivo_uc <- "dados/unidade17/processados/areas_protegidas_area_m.gpkg"
arquivo_bioma <- "dados/unidade01/brutos/amazon_biome_border.shp"
arquivo_area_m <- "dados/unidade05/processados/area_m_dinizia.gpkg"
arquivo_oc <- "dados/unidade05/processados/presencas_area_m_dinizia.csv"

if (!file.exists(arquivo_prioridade)) stop("Prioridade climática atual não encontrada. Execute o Script 03.")
if (!file.exists(arquivo_uc)) stop("Áreas protegidas não encontradas. Execute o Script 02.")
if (!file.exists(arquivo_bioma)) stop("Limite do bioma Amazônia não encontrado.")
if (!file.exists(arquivo_area_m)) stop("Área M não encontrada.")
if (!file.exists(arquivo_oc)) stop("Ocorrências não encontradas.")

# ------------------------------------------------------------
# 2. Ler camadas
# ------------------------------------------------------------

prioridade <- terra::rast(arquivo_prioridade)
names(prioridade) <- "prioridade_climatica"

uc <- sf::st_read(
  arquivo_uc,
  quiet = TRUE
) |>
  sf::st_make_valid()

# ------------------------------------------------------------
# 3. Definir alta prioridade
# ------------------------------------------------------------

limiar_prioridade <- as.numeric(
  terra::global(
    prioridade,
    fun = function(x, ...) stats::quantile(x, probs = 0.75, na.rm = TRUE)
  )[1, 1]
)

prioridade_alta <- terra::ifel(
  prioridade >= limiar_prioridade,
  1,
  0
)

names(prioridade_alta) <- "alta_prioridade"

terra::writeRaster(
  prioridade_alta,
  "resultados/unidade17/componentes/alta_prioridade_climatica_atual.tif",
  overwrite = TRUE
)

# ------------------------------------------------------------
# 4. Rasterizar áreas protegidas
# ------------------------------------------------------------

if (nrow(uc) > 0) {
  
  uc_proj <- sf::st_transform(
    uc,
    terra::crs(prioridade)
  )
  
  protegida <- terra::rasterize(
    terra::vect(uc_proj),
    prioridade,
    field = 1,
    background = 0
  )
  
} else {
  
  protegida <- prioridade
  terra::values(protegida) <- 0
}

names(protegida) <- "protegida"

terra::writeRaster(
  protegida,
  "resultados/unidade17/componentes/areas_protegidas_raster.tif",
  overwrite = TRUE
)

# ------------------------------------------------------------
# 5. Lacunas de proteção
# ------------------------------------------------------------
# Lacuna = alta prioridade climática fora de área protegida.
# ------------------------------------------------------------

lacuna <- terra::ifel(
  prioridade_alta == 1 & protegida == 0,
  1,
  0
)

names(lacuna) <- "lacuna_protecao"

terra::writeRaster(
  lacuna,
  "resultados/unidade17/componentes/lacunas_protecao.tif",
  overwrite = TRUE
)

stack_lacuna <- c(
  prioridade,
  prioridade_alta,
  protegida,
  lacuna
)

terra::writeRaster(
  stack_lacuna,
  "resultados/unidade17/componentes/stack_lacunas_protecao.tif",
  overwrite = TRUE
)

# ------------------------------------------------------------
# 6. Exportar tabela espacial
# ------------------------------------------------------------

df <- as.data.frame(
  stack_lacuna,
  xy = TRUE,
  na.rm = TRUE
) |>
  dplyr::rename(
    lon = x,
    lat = y
  ) |>
  dplyr::mutate(
    classe = dplyr::case_when(
      alta_prioridade == 1 & protegida == 1 ~ "Alta prioridade protegida",
      alta_prioridade == 1 & protegida == 0 ~ "Lacuna de proteção",
      alta_prioridade == 0 & protegida == 1 ~ "Protegida não prioritária",
      alta_prioridade == 0 & protegida == 0 ~ "Demais áreas",
      TRUE ~ NA_character_
    ),
    classe = factor(
      classe,
      levels = c(
        "Demais áreas",
        "Protegida não prioritária",
        "Alta prioridade protegida",
        "Lacuna de proteção"
      )
    )
  )

readr::write_csv(
  df,
  "dados/unidade17/processados/lacunas_protecao.csv"
)

# ------------------------------------------------------------
# 7. Síntese de área
# ------------------------------------------------------------

area_cell <- terra::cellSize(
  prioridade,
  unit = "km"
)

area_df <- as.data.frame(
  c(area_cell, stack_lacuna),
  xy = FALSE,
  na.rm = TRUE
)

names(area_df) <- c(
  "area_km2",
  "prioridade",
  "alta_prioridade",
  "protegida",
  "lacuna"
)

sintese <- area_df |>
  dplyr::mutate(
    classe = dplyr::case_when(
      alta_prioridade == 1 & protegida == 1 ~ "Alta prioridade protegida",
      alta_prioridade == 1 & protegida == 0 ~ "Lacuna de proteção",
      alta_prioridade == 0 & protegida == 1 ~ "Protegida não prioritária",
      alta_prioridade == 0 & protegida == 0 ~ "Demais áreas",
      TRUE ~ NA_character_
    )
  ) |>
  dplyr::group_by(classe) |>
  dplyr::summarise(
    area_km2 = sum(area_km2, na.rm = TRUE),
    prioridade_media = mean(prioridade, na.rm = TRUE),
    n_pixels = dplyr::n(),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    prop_area = area_km2 / sum(area_km2, na.rm = TRUE),
    limiar_prioridade = limiar_prioridade
  ) |>
  dplyr::arrange(dplyr::desc(area_km2))

readr::write_csv(
  sintese,
  "tabelas/unidade17/sintese_lacunas_protecao.csv"
)

# ------------------------------------------------------------
# 8. Elementos cartográficos
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

area_m_plot <- sf::st_read(
  arquivo_area_m,
  quiet = TRUE
) |>
  sf::st_make_valid() |>
  sf::st_transform(4326)

uc_plot <- uc |>
  sf::st_make_valid() |>
  sf::st_transform(4326)

oc <- readr::read_csv(arquivo_oc, show_col_types = FALSE)

oc_sf <- sf::st_as_sf(
  oc,
  coords = c("lon", "lat"),
  crs = 4326,
  remove = FALSE
)

# ------------------------------------------------------------
# 9. Mapa de lacunas
# ------------------------------------------------------------

cores_lacunas <- c(
  "Demais áreas" = "grey88",
  "Protegida não prioritária" = "#A6CEE3",
  "Alta prioridade protegida" = "#1A9850",
  "Lacuna de proteção" = "#B2182B"
)

g_lacuna <- ggplot() +
  geom_sf(
    data = bioma_plot,
    fill = "grey96",
    color = "grey35",
    linewidth = 0.30
  ) +
  geom_raster(
    data = df,
    aes(x = lon, y = lat, fill = classe)
  ) +
  geom_sf(
    data = area_m_plot,
    fill = NA,
    color = "grey25",
    linewidth = 0.25,
    linetype = "dashed"
  ) +
  {
    if (nrow(uc_plot) > 0) {
      geom_sf(
        data = uc_plot,
        fill = NA,
        color = "#08306B",
        linewidth = 0.35
      )
    }
  } +
  geom_sf(
    data = oc_sf,
    color = "black",
    fill = "red",
    shape = 21,
    size = 0.50,
    stroke = 0.12,
    alpha = 0.65
  ) +
  scale_fill_manual(
    values = cores_lacunas,
    name = "Classe",
    drop = FALSE
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
    title = expression("Lacunas de proteção para " * italic("Dinizia excelsa")),
    subtitle = "Áreas de alta prioridade climática fora de áreas protegidas",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_bw(base_size = 10) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

ggsave(
  "figuras/unidade17/unidade17_lacunas_protecao.png",
  plot = g_lacuna,
  width = 11,
  height = 8,
  dpi = 600
)

# ------------------------------------------------------------
# 10. Gráfico de área por classe
# ------------------------------------------------------------

g_bar <- ggplot(
  sintese,
  aes(
    x = reorder(classe, area_km2),
    y = area_km2,
    fill = classe
  )
) +
  geom_col(
    color = "grey25",
    linewidth = 0.15,
    show.legend = FALSE
  ) +
  coord_flip() +
  scale_fill_manual(values = cores_lacunas) +
  scale_y_continuous(
    labels = scales::label_number(
      big.mark = ".",
      decimal.mark = ","
    )
  ) +
  labs(
    title = "Área por classe de proteção e prioridade",
    subtitle = paste0("Alta prioridade definida por P75 = ", round(limiar_prioridade, 3)),
    x = NULL,
    y = expression("Área (km"^2*")")
  ) +
  theme_bw(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave(
  "figuras/unidade17/unidade17_area_lacunas_protecao.png",
  plot = g_bar,
  width = 9,
  height = 6,
  dpi = 600
)

fig <- g_lacuna / g_bar +
  patchwork::plot_layout(
    heights = c(1.25, 0.75)
  ) +
  patchwork::plot_annotation(
    title = expression("Síntese das lacunas de proteção para " * italic("Dinizia excelsa")),
    subtitle = "Integração entre prioridade climática atual e cobertura por áreas protegidas"
  ) &
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 10)
  )

ggsave(
  "figuras/unidade17/unidade17_lacunas_protecao_patchwork.png",
  plot = fig,
  width = 11,
  height = 13,
  dpi = 600
)

message("Lacunas de proteção calculadas com sucesso.")
print(sintese)
