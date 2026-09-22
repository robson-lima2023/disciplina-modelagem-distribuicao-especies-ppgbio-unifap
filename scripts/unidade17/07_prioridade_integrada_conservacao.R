source("scripts/_bootstrap.R")

# ============================================================
# Unidade 17 - Aplicações à conservação
# Script 07: Prioridade integrada para conservação
# ============================================================

pacotes <- c(
  "dplyr", "readr", "terra", "sf", "ggplot2",
  "ggspatial", "viridis", "tibble", "tidyr", "patchwork"
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

arquivos <- c(
  prioridade_atual = "resultados/unidade17/componentes/prioridade_climatica_atual.tif",
  estabilidade_futura = "resultados/unidade17/componentes/estabilidade_futura_media.tif",
  refugios_historicos = "resultados/unidade17/componentes/estabilidade_historica_refugios.tif",
  baixa_incerteza = "resultados/unidade16/incerteza_integrada_dinizia.tif"
)

arquivo_bioma <- "dados/unidade01/brutos/amazon_biome_border.shp"
arquivo_area_m <- "dados/unidade05/processados/area_m_dinizia.gpkg"
arquivo_uc <- "dados/unidade17/processados/areas_protegidas_area_m.gpkg"
arquivo_oc <- "dados/unidade05/processados/presencas_area_m_dinizia.csv"

faltantes <- arquivos[!file.exists(arquivos)]

if (length(faltantes) > 0) {
  stop(
    "Componentes faltantes para prioridade integrada: ",
    paste(names(faltantes), collapse = ", "),
    ". Execute os scripts anteriores da Unidade 17 e a Unidade 16."
  )
}

if (!file.exists(arquivo_bioma)) stop("Limite do bioma Amazônia não encontrado.")
if (!file.exists(arquivo_area_m)) stop("Área M não encontrada.")
if (!file.exists(arquivo_oc)) stop("Ocorrências não encontradas.")

# ------------------------------------------------------------
# 2. Ler e alinhar rasters
# ------------------------------------------------------------

rasters <- lapply(arquivos, terra::rast)

ref <- rasters[[1]]

rasters <- lapply(rasters, function(r) {
  if (!terra::compareGeom(ref, r, stopOnError = FALSE)) {
    terra::resample(r, ref, method = "bilinear")
  } else {
    r
  }
})

normalizar <- function(r) {
  
  mn <- as.numeric(terra::global(r, "min", na.rm = TRUE)[1, 1])
  mx <- as.numeric(terra::global(r, "max", na.rm = TRUE)[1, 1])
  
  if (is.na(mn) || is.na(mx) || !is.finite(mn) || !is.finite(mx) || mx == mn) {
    return(r * 0)
  }
  
  (r - mn) / (mx - mn)
}

prioridade_atual_n <- normalizar(rasters$prioridade_atual)
estabilidade_futura_n <- normalizar(rasters$estabilidade_futura)
refugios_historicos_n <- normalizar(rasters$refugios_historicos)

# Incerteza entra invertida: menor incerteza = maior prioridade.
incerteza_n <- normalizar(rasters$baixa_incerteza)
baixa_incerteza_n <- 1 - incerteza_n

names(prioridade_atual_n) <- "prioridade_atual"
names(estabilidade_futura_n) <- "estabilidade_futura"
names(refugios_historicos_n) <- "refugios_historicos"
names(baixa_incerteza_n) <- "baixa_incerteza"

stack_componentes <- c(
  prioridade_atual_n,
  estabilidade_futura_n,
  refugios_historicos_n,
  baixa_incerteza_n
)

terra::writeRaster(
  stack_componentes,
  "resultados/unidade17/componentes/stack_componentes_prioridade_integrada.tif",
  overwrite = TRUE
)

# ------------------------------------------------------------
# 3. Pesos dos componentes
# ------------------------------------------------------------

pesos <- tibble::tibble(
  componente = c(
    "prioridade_atual",
    "estabilidade_futura",
    "refugios_historicos",
    "baixa_incerteza"
  ),
  peso = c(
    0.35,
    0.25,
    0.25,
    0.15
  ),
  interpretacao = c(
    "Adequabilidade atual alta e baixa incerteza",
    "Manutenção da adequabilidade sob cenários futuros",
    "Estabilidade histórica/paleoclimática",
    "Confiabilidade espacial das projeções"
  )
)

pesos$peso <- pesos$peso / sum(pesos$peso)

readr::write_csv(
  pesos,
  "tabelas/unidade17/pesos_prioridade_integrada.csv"
)

# ------------------------------------------------------------
# 4. Calcular prioridade integrada ponderada
# ------------------------------------------------------------

prioridade_integrada <- terra::app(
  stack_componentes,
  fun = function(x) {
    stats::weighted.mean(
      x,
      w = pesos$peso,
      na.rm = TRUE
    )
  }
)

names(prioridade_integrada) <- "prioridade_integrada"

terra::writeRaster(
  prioridade_integrada,
  "resultados/unidade17/prioridade_integrada_conservacao.tif",
  overwrite = TRUE
)

terra::writeRaster(
  c(stack_componentes, prioridade_integrada),
  "resultados/unidade17/componentes/stack_prioridade_integrada_conservacao.tif",
  overwrite = TRUE
)

# ------------------------------------------------------------
# 5. Classes de prioridade integrada
# ------------------------------------------------------------

q <- as.numeric(
  terra::global(
    prioridade_integrada,
    fun = function(x, ...) {
      stats::quantile(
        x,
        probs = c(0.50, 0.75, 0.90),
        na.rm = TRUE
      )
    }
  )[1, ]
)

q50 <- q[1]
q75 <- q[2]
q90 <- q[3]

classe_prioridade <- terra::ifel(
  prioridade_integrada >= q90,
  4,
  terra::ifel(
    prioridade_integrada >= q75,
    3,
    terra::ifel(
      prioridade_integrada >= q50,
      2,
      1
    )
  )
)

names(classe_prioridade) <- "classe_prioridade_integrada"

terra::writeRaster(
  classe_prioridade,
  "resultados/unidade17/componentes/classes_prioridade_integrada.tif",
  overwrite = TRUE
)

# ------------------------------------------------------------
# 6. Exportar tabelas espaciais
# ------------------------------------------------------------

df <- as.data.frame(
  c(stack_componentes, prioridade_integrada, classe_prioridade),
  xy = TRUE,
  na.rm = TRUE
) |>
  dplyr::rename(
    lon = x,
    lat = y
  ) |>
  dplyr::mutate(
    classe_nome = dplyr::case_when(
      classe_prioridade_integrada == 4 ~ "Muito alta",
      classe_prioridade_integrada == 3 ~ "Alta",
      classe_prioridade_integrada == 2 ~ "Moderada",
      classe_prioridade_integrada == 1 ~ "Baixa",
      TRUE ~ NA_character_
    ),
    classe_nome = factor(
      classe_nome,
      levels = c("Baixa", "Moderada", "Alta", "Muito alta")
    )
  )

readr::write_csv(
  df,
  "dados/unidade17/processados/prioridade_integrada_conservacao.csv"
)

# ------------------------------------------------------------
# 7. Síntese de área por classe
# ------------------------------------------------------------

area_cell <- terra::cellSize(
  prioridade_integrada,
  unit = "km"
)

area_df <- as.data.frame(
  c(area_cell, prioridade_integrada, classe_prioridade),
  xy = FALSE,
  na.rm = TRUE
)

names(area_df) <- c(
  "area_km2",
  "prioridade_integrada",
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
    ),
    classe_nome = factor(
      classe_nome,
      levels = c("Baixa", "Moderada", "Alta", "Muito alta")
    )
  ) |>
  dplyr::group_by(classe_nome) |>
  dplyr::summarise(
    area_km2 = sum(area_km2, na.rm = TRUE),
    prioridade_media = mean(prioridade_integrada, na.rm = TRUE),
    n_pixels = dplyr::n(),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    prop_area = area_km2 / sum(area_km2, na.rm = TRUE)
  )

readr::write_csv(
  resumo,
  "tabelas/unidade17/resumo_prioridade_integrada_conservacao.csv"
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
  "tabelas/unidade17/limiares_prioridade_integrada.csv"
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
# 9. Mapa contínuo
# ------------------------------------------------------------

g_prior <- ggplot() +
  geom_sf(
    data = bioma_plot,
    fill = "grey96",
    color = "grey35",
    linewidth = 0.30
  ) +
  geom_raster(
    data = df,
    aes(x = lon, y = lat, fill = prioridade_integrada)
  ) +
  geom_sf(
    data = area_m_plot,
    fill = NA,
    color = "grey25",
    linewidth = 0.25,
    linetype = "dashed"
  ) +
  {
    if (!is.null(uc_plot) && nrow(uc_plot) > 0) {
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
  scale_fill_viridis_c(
    name = "Prioridade",
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
    width_hint = 0.25,
    text_cex = 0.60
  ) +
  ggspatial::annotation_north_arrow(
    location = "tr",
    which_north = "true",
    style = ggspatial::north_arrow_fancy_orienteering
  ) +
  labs(
    title = expression("Prioridade integrada para conservação de " * italic("Dinizia excelsa")),
    subtitle = "Adequabilidade atual, estabilidade futura, refúgios históricos e baixa incerteza",
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
  "figuras/unidade17/unidade17_prioridade_integrada.png",
  plot = g_prior,
  width = 9.5,
  height = 7,
  dpi = 600
)

# ------------------------------------------------------------
# 10. Mapa categórico
# ------------------------------------------------------------

cores_prioridade <- c(
  "Baixa" = "grey85",
  "Moderada" = "#A6D96A",
  "Alta" = "#1A9850",
  "Muito alta" = "#006837"
)

g_classe <- ggplot() +
  geom_sf(
    data = bioma_plot,
    fill = "grey96",
    color = "grey35",
    linewidth = 0.30
  ) +
  geom_raster(
    data = df,
    aes(x = lon, y = lat, fill = classe_nome)
  ) +
  geom_sf(
    data = area_m_plot,
    fill = NA,
    color = "grey25",
    linewidth = 0.25,
    linetype = "dashed"
  ) +
  {
    if (!is.null(uc_plot) && nrow(uc_plot) > 0) {
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
    values = cores_prioridade,
    name = "Prioridade",
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
  labs(
    title = expression("Classes de prioridade integrada para " * italic("Dinizia excelsa")),
    subtitle = "Classes definidas por quantis da prioridade integrada",
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
  "figuras/unidade17/unidade17_classes_prioridade_integrada.png",
  plot = g_classe,
  width = 9.5,
  height = 7,
  dpi = 600
)

# ------------------------------------------------------------
# 11. Componentes
# ------------------------------------------------------------

df_comp <- df |>
  dplyr::select(
    lon,
    lat,
    prioridade_atual,
    estabilidade_futura,
    refugios_historicos,
    baixa_incerteza
  ) |>
  tidyr::pivot_longer(
    cols = -c(lon, lat),
    names_to = "componente",
    values_to = "valor"
  ) |>
  dplyr::mutate(
    componente = dplyr::case_when(
      componente == "prioridade_atual" ~ "Prioridade atual",
      componente == "estabilidade_futura" ~ "Estabilidade futura",
      componente == "refugios_historicos" ~ "Refúgios históricos",
      componente == "baixa_incerteza" ~ "Baixa incerteza",
      TRUE ~ componente
    )
  )

g_comp <- ggplot() +
  geom_sf(
    data = bioma_plot,
    fill = "grey96",
    color = "grey35",
    linewidth = 0.25
  ) +
  geom_raster(
    data = df_comp,
    aes(x = lon, y = lat, fill = valor)
  ) +
  geom_sf(
    data = area_m_plot,
    fill = NA,
    color = "grey25",
    linewidth = 0.20,
    linetype = "dashed"
  ) +
  scale_fill_viridis_c(
    name = "Valor",
    option = "viridis",
    limits = c(0, 1),
    na.value = NA
  ) +
  coord_sf(
    xlim = c(bbox_bioma["xmin"], bbox_bioma["xmax"]),
    ylim = c(bbox_bioma["ymin"], bbox_bioma["ymax"]),
    expand = FALSE
  ) +
  facet_wrap(~ componente, ncol = 2) +
  labs(
    title = expression("Componentes da prioridade integrada para " * italic("Dinizia excelsa")),
    subtitle = "Camadas normalizadas usadas no índice final de conservação",
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
  "figuras/unidade17/unidade17_componentes_prioridade_integrada.png",
  plot = g_comp,
  width = 12,
  height = 9,
  dpi = 600
)

# ------------------------------------------------------------
# 12. Síntese gráfica
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
  scale_fill_manual(values = cores_prioridade, drop = FALSE) +
  scale_y_continuous(
    labels = scales::label_number(
      big.mark = ".",
      decimal.mark = ","
    )
  ) +
  labs(
    title = "Área por classe de prioridade integrada",
    subtitle = "Classes de prioridade para ações de conservação",
    x = NULL,
    y = expression("Área (km"^2*")")
  ) +
  theme_bw(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave(
  "figuras/unidade17/unidade17_area_classes_prioridade_integrada.png",
  plot = g_bar,
  width = 8.5,
  height = 5.5,
  dpi = 600
)

fig <- (g_prior + g_classe) / g_bar +
  patchwork::plot_layout(
    heights = c(1.25, 0.75)
  ) +
  patchwork::plot_annotation(
    title = expression("Síntese da prioridade integrada para conservação de " * italic("Dinizia excelsa")),
    subtitle = "Índice contínuo, classes espaciais e área por classe"
  ) &
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 10)
  )

ggsave(
  "figuras/unidade17/unidade17_prioridade_integrada_patchwork.png",
  plot = fig,
  width = 14,
  height = 13,
  dpi = 600
)

message("Prioridade integrada para conservação calculada com sucesso.")
print(resumo)
