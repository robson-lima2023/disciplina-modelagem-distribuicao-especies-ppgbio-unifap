source("scripts/_bootstrap.R")

# ============================================================
# Unidade 17 - Aplicações à conservação
# Script 03: Prioridade climática atual
# ============================================================

pacotes <- c(
  "dplyr", "readr", "terra", "sf", "ggplot2",
  "ggspatial", "viridis", "tibble", "patchwork"
)

require_packages(pacotes)
invisible(lapply(pacotes, library, character.only = TRUE))

sf::sf_use_s2(FALSE)
dir.create("dados/unidade17/processados", recursive = TRUE, showWarnings = FALSE)
dir.create("figuras/unidade17", recursive = TRUE, showWarnings = FALSE)
dir.create("tabelas/unidade17", recursive = TRUE, showWarnings = FALSE)
dir.create("resultados/unidade17/componentes", recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# 1. Arquivos
# ------------------------------------------------------------

arquivo_adeq <- "dados/unidade17/processados/adequabilidade_atual_conservacao.tif"
arquivo_inc <- "dados/unidade17/processados/incerteza_integrada_conservacao.tif"
arquivo_uc <- "dados/unidade17/processados/areas_protegidas_area_m.gpkg"

arquivo_bioma <- "dados/unidade01/brutos/amazon_biome_border.shp"
arquivo_area_m <- "dados/unidade05/processados/area_m_dinizia.gpkg"
arquivo_oc <- "dados/unidade05/processados/presencas_area_m_dinizia.csv"

if (!file.exists(arquivo_adeq)) stop("Adequabilidade atual não encontrada. Execute o Script 02 da Unidade 17.")
if (!file.exists(arquivo_inc)) stop("Incerteza integrada não encontrada. Execute o Script 02 da Unidade 17.")
if (!file.exists(arquivo_bioma)) stop("Limite do bioma Amazônia não encontrado.")
if (!file.exists(arquivo_area_m)) stop("Área M não encontrada.")
if (!file.exists(arquivo_oc)) stop("Arquivo de ocorrências não encontrado.")

# ------------------------------------------------------------
# 2. Ler rasters
# ------------------------------------------------------------

adeq <- terra::rast(arquivo_adeq)
inc <- terra::rast(arquivo_inc)

names(adeq) <- "adequabilidade"
names(inc) <- "incerteza"

if (!terra::compareGeom(adeq, inc, stopOnError = FALSE)) {
  inc <- terra::resample(
    inc,
    adeq,
    method = "bilinear"
  )
}

# ------------------------------------------------------------
# 3. Função de normalização
# ------------------------------------------------------------

normalizar <- function(r) {
  
  mn <- as.numeric(terra::global(r, "min", na.rm = TRUE)[1, 1])
  mx <- as.numeric(terra::global(r, "max", na.rm = TRUE)[1, 1])
  
  if (is.na(mn) || is.na(mx) || !is.finite(mn) || !is.finite(mx) || mx == mn) {
    return(r * 0)
  }
  
  (r - mn) / (mx - mn)
}

adeq_n <- normalizar(adeq)
inc_n <- normalizar(inc)

names(adeq_n) <- "adequabilidade_normalizada"
names(inc_n) <- "incerteza_normalizada"

# ------------------------------------------------------------
# 4. Prioridade climática atual
# ------------------------------------------------------------
# Critério:
# prioridade = adequabilidade normalizada * (1 - incerteza normalizada)
# ------------------------------------------------------------

prioridade_atual <- adeq_n * (1 - inc_n)

names(prioridade_atual) <- "prioridade_climatica_atual"

terra::writeRaster(
  prioridade_atual,
  "resultados/unidade17/componentes/prioridade_climatica_atual.tif",
  overwrite = TRUE
)

terra::writeRaster(
  c(adeq_n, inc_n, prioridade_atual),
  "resultados/unidade17/componentes/stack_prioridade_climatica_atual.tif",
  overwrite = TRUE
)

df <- as.data.frame(
  c(adeq_n, inc_n, prioridade_atual),
  xy = TRUE,
  na.rm = TRUE
) |>
  dplyr::rename(
    lon = x,
    lat = y
  )

readr::write_csv(
  df,
  "dados/unidade17/processados/prioridade_climatica_atual.csv"
)

# ------------------------------------------------------------
# 5. Classes de prioridade
# ------------------------------------------------------------

q_prior <- terra::global(
  prioridade_atual,
  fun = function(x, ...) {
    stats::quantile(x, probs = c(0.50, 0.75, 0.90), na.rm = TRUE)
  }
)

q50 <- as.numeric(q_prior[1, 1])
q75 <- as.numeric(q_prior[1, 2])
q90 <- as.numeric(q_prior[1, 3])

prioridade_classe <- terra::ifel(
  prioridade_atual >= q90,
  4,
  terra::ifel(
    prioridade_atual >= q75,
    3,
    terra::ifel(
      prioridade_atual >= q50,
      2,
      1
    )
  )
)

names(prioridade_classe) <- "classe_prioridade_atual"

terra::writeRaster(
  prioridade_classe,
  "resultados/unidade17/componentes/classe_prioridade_climatica_atual.tif",
  overwrite = TRUE
)

df_classe <- as.data.frame(
  prioridade_classe,
  xy = TRUE,
  na.rm = TRUE
) |>
  dplyr::rename(
    lon = x,
    lat = y,
    classe = classe_prioridade_atual
  ) |>
  dplyr::mutate(
    classe_nome = dplyr::case_when(
      classe == 4 ~ "Muito alta",
      classe == 3 ~ "Alta",
      classe == 2 ~ "Moderada",
      classe == 1 ~ "Baixa",
      TRUE ~ NA_character_
    ),
    classe_nome = factor(
      classe_nome,
      levels = c("Baixa", "Moderada", "Alta", "Muito alta")
    )
  )

readr::write_csv(
  df_classe,
  "dados/unidade17/processados/classe_prioridade_climatica_atual.csv"
)

# ------------------------------------------------------------
# 6. Resumo
# ------------------------------------------------------------

area_cell <- terra::cellSize(
  prioridade_classe,
  unit = "km"
)

area_df <- as.data.frame(
  c(area_cell, prioridade_atual, prioridade_classe),
  xy = FALSE,
  na.rm = TRUE
)

names(area_df) <- c(
  "area_km2",
  "prioridade",
  "classe"
)

resumo <- area_df |>
  dplyr::mutate(
    classe_nome = dplyr::case_when(
      classe == 4 ~ "Muito alta",
      classe == 3 ~ "Alta",
      classe == 2 ~ "Moderada",
      classe == 1 ~ "Baixa",
      TRUE ~ NA_character_
    )
  ) |>
  dplyr::group_by(classe_nome) |>
  dplyr::summarise(
    area_km2 = sum(area_km2, na.rm = TRUE),
    prioridade_media = mean(prioridade, na.rm = TRUE),
    n_pixels = dplyr::n(),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    prop_area = area_km2 / sum(area_km2, na.rm = TRUE)
  )

readr::write_csv(
  resumo,
  "tabelas/unidade17/resumo_prioridade_climatica_atual.csv"
)

limiares <- tibble::tibble(
  classe = c("Baixa", "Moderada", "Alta", "Muito alta"),
  criterio = c(
    paste0("< q50 = ", round(q50, 4)),
    paste0("q50-q75 = ", round(q50, 4), "-", round(q75, 4)),
    paste0("q75-q90 = ", round(q75, 4), "-", round(q90, 4)),
    paste0(">= q90 = ", round(q90, 4))
  )
)

readr::write_csv(
  limiares,
  "tabelas/unidade17/limiares_prioridade_climatica_atual.csv"
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
# 8. Mapa contínuo de prioridade
# ------------------------------------------------------------

g_prior <- ggplot() +
  geom_sf(data = bioma_plot, fill = "grey96", color = "grey35", linewidth = 0.30) +
  geom_raster(data = df, aes(x = lon, y = lat, fill = prioridade_climatica_atual)) +
  geom_sf(data = area_m_plot, fill = NA, color = "grey20", linewidth = 0.25, linetype = "dashed") +
  {
    if (!is.null(uc_plot) && nrow(uc_plot) > 0) {
      geom_sf(data = uc_plot, fill = NA, color = "#1A9850", linewidth = 0.35)
    }
  } +
  geom_sf(data = oc_sf, color = "black", fill = "red", shape = 21, size = 0.50, stroke = 0.13, alpha = 0.65) +
  scale_fill_viridis_c(name = "Prioridade", option = "viridis", limits = c(0, 1), na.value = NA) +
  coord_sf(xlim = c(bbox_bioma["xmin"], bbox_bioma["xmax"]),
           ylim = c(bbox_bioma["ymin"], bbox_bioma["ymax"]),
           expand = FALSE) +
  ggspatial::annotation_scale(location = "bl", width_hint = 0.25, text_cex = 0.60) +
  ggspatial::annotation_north_arrow(
    location = "tr",
    which_north = "true",
    style = ggspatial::north_arrow_fancy_orienteering
  ) +
  labs(
    title = expression("Prioridade climática atual para " * italic("Dinizia excelsa")),
    subtitle = "Alta adequabilidade combinada com baixa incerteza integrada",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_bw(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10),
    panel.grid.minor = element_blank()
  )

ggsave(
  "figuras/unidade17/unidade17_prioridade_climatica_atual.png",
  plot = g_prior,
  width = 9,
  height = 7,
  dpi = 600
)

# ------------------------------------------------------------
# 9. Mapa categórico de prioridade
# ------------------------------------------------------------

cores_prioridade <- c(
  "Baixa" = "grey85",
  "Moderada" = "#A6D96A",
  "Alta" = "#1A9850",
  "Muito alta" = "#006837"
)

g_classe <- ggplot() +
  geom_sf(data = bioma_plot, fill = "grey96", color = "grey35", linewidth = 0.30) +
  geom_raster(data = df_classe, aes(x = lon, y = lat, fill = classe_nome)) +
  geom_sf(data = area_m_plot, fill = NA, color = "grey20", linewidth = 0.25, linetype = "dashed") +
  {
    if (!is.null(uc_plot) && nrow(uc_plot) > 0) {
      geom_sf(data = uc_plot, fill = NA, color = "#08306B", linewidth = 0.35)
    }
  } +
  geom_sf(data = oc_sf, color = "black", fill = "red", shape = 21, size = 0.50, stroke = 0.13, alpha = 0.65) +
  scale_fill_manual(
    values = cores_prioridade,
    name = "Prioridade",
    drop = FALSE
  ) +
  coord_sf(xlim = c(bbox_bioma["xmin"], bbox_bioma["xmax"]),
           ylim = c(bbox_bioma["ymin"], bbox_bioma["ymax"]),
           expand = FALSE) +
  ggspatial::annotation_scale(location = "bl", width_hint = 0.25, text_cex = 0.60) +
  labs(
    title = expression("Classes de prioridade climática atual para " * italic("Dinizia excelsa")),
    subtitle = "Classificação por quantis da prioridade climática atual",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_bw(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

ggsave(
  "figuras/unidade17/unidade17_classes_prioridade_climatica_atual.png",
  plot = g_classe,
  width = 9,
  height = 7,
  dpi = 600
)

# ------------------------------------------------------------
# 10. Gráfico de área por classe
# ------------------------------------------------------------

g_bar <- ggplot(
  resumo,
  aes(
    x = reorder(classe_nome, area_km2),
    y = area_km2,
    fill = classe_nome
  )
) +
  geom_col(
    color = "grey25",
    linewidth = 0.15,
    show.legend = FALSE
  ) +
  coord_flip() +
  scale_fill_manual(
    values = cores_prioridade,
    drop = FALSE
  ) +
  scale_y_continuous(
    labels = scales::label_number(
      big.mark = ".",
      decimal.mark = ","
    )
  ) +
  labs(
    title = "Área por classe de prioridade climática atual",
    subtitle = "Síntese espacial das classes derivadas por quantis",
    x = NULL,
    y = expression("Área (km"^2*")")
  ) +
  theme_bw(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave(
  "figuras/unidade17/unidade17_area_classes_prioridade_atual.png",
  plot = g_bar,
  width = 8,
  height = 5,
  dpi = 600
)

fig <- (g_prior + g_classe) / g_bar +
  patchwork::plot_layout(
    heights = c(1.2, 0.75)
  ) +
  patchwork::plot_annotation(
    title = expression("Síntese de prioridade climática atual para conservação de " * italic("Dinizia excelsa")),
    subtitle = "Integração entre adequabilidade, incerteza e classificação espacial de prioridade"
  ) &
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 10)
  )

ggsave(
  "figuras/unidade17/unidade17_prioridade_climatica_atual_patchwork.png",
  plot = fig,
  width = 14,
  height = 13,
  dpi = 600
)

message("Prioridade climática atual calculada com sucesso.")
print(resumo)
