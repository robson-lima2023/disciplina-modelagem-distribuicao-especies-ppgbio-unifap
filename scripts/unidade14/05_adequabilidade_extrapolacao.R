source("scripts/_bootstrap.R")

# ============================================================
# Unidade 14 - Transferência, extrapolação, MESS e MOP
# Script 05: Adequabilidade futura sob extrapolação
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
  "tidyr",
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

dir.create("dados/unidade14/processados", recursive = TRUE, showWarnings = FALSE)
dir.create("figuras/unidade14", recursive = TRUE, showWarnings = FALSE)
dir.create("tabelas/unidade14", recursive = TRUE, showWarnings = FALSE)
dir.create("resultados/unidade14/extrapolacao", recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# 4. Configurações
# ------------------------------------------------------------

cenarios <- c("ssp126", "ssp245", "ssp370", "ssp585")

periodo <- "2061-2080"
modelo_gcm <- "MIROC6"

arquivo_bioma <- "dados/unidade01/brutos/amazon_biome_border.shp"
arquivo_m <- "dados/unidade05/processados/area_m_dinizia.gpkg"
arquivo_oc <- "dados/unidade05/processados/presencas_area_m_dinizia.csv"

if (!file.exists(arquivo_bioma)) {
  stop("Limite do bioma Amazônia não encontrado: ", arquivo_bioma)
}

if (!file.exists(arquivo_oc)) {
  stop("Arquivo de ocorrências não encontrado: ", arquivo_oc)
}

if (!file.exists(arquivo_m)) {
  warning("Área M não encontrada. Os mapas serão gerados sem contorno da área M.")
}

# ------------------------------------------------------------
# 5. Elementos cartográficos
# ------------------------------------------------------------

bioma_raw <- sf::st_read(
  arquivo_bioma,
  quiet = TRUE
)

oc <- readr::read_csv(
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

if (!all(c("lon", "lat") %in% names(oc))) {
  stop("O arquivo de ocorrências precisa conter as colunas 'lon' e 'lat'.")
}

# ------------------------------------------------------------
# 6. Avaliar extrapolação combinada por cenário
# ------------------------------------------------------------

dados_all <- list()
resumos <- list()
rasters_ref <- list()

for (ssp in cenarios) {
  
  message("Avaliando adequabilidade sob extrapolação para: ", ssp)
  
  arquivo_ens <- paste0(
    "resultados/unidade13/ensemble/ensemble_futuro_",
    ssp,
    "_",
    periodo,
    "_",
    modelo_gcm,
    ".tif"
  )
  
  arquivo_mess <- paste0(
    "resultados/unidade14/mess/mess_",
    ssp,
    "_",
    periodo,
    "_",
    modelo_gcm,
    ".tif"
  )
  
  arquivo_mop <- paste0(
    "resultados/unidade14/mop/mop_extrapolacao_",
    ssp,
    "_",
    periodo,
    "_",
    modelo_gcm,
    ".tif"
  )
  
  if (!file.exists(arquivo_ens)) {
    stop("Ensemble futuro não encontrado para ", ssp, ". Execute a Unidade 13, Script 04.")
  }
  
  if (!file.exists(arquivo_mess)) {
    stop("MESS não encontrado para ", ssp, ". Execute a Unidade 14, Script 02.")
  }
  
  if (!file.exists(arquivo_mop)) {
    stop("MOP não encontrado para ", ssp, ". Execute a Unidade 14, Script 04.")
  }
  
  ens <- terra::rast(arquivo_ens)
  mess <- terra::rast(arquivo_mess)
  mop <- terra::rast(arquivo_mop)
  
  names(ens) <- "adequabilidade"
  names(mess) <- "MESS"
  names(mop) <- "MOP_extrapolacao"
  
  if (!terra::compareGeom(ens, mess, stopOnError = FALSE)) {
    mess <- terra::resample(
      mess,
      ens,
      method = "bilinear"
    )
  }
  
  if (!terra::compareGeom(ens, mop, stopOnError = FALSE)) {
    mop <- terra::resample(
      mop,
      ens,
      method = "near"
    )
  }
  
  extrap_mess <- terra::ifel(
    mess < 0,
    1,
    0
  )
  
  names(extrap_mess) <- "extrapolacao_MESS"
  
  extrap_combinada <- terra::ifel(
    extrap_mess == 1 | mop == 1,
    1,
    0
  )
  
  names(extrap_combinada) <- "extrapolacao_combinada"
  
  adequabilidade_sem_extrapolacao <- terra::ifel(
    extrap_combinada == 0,
    ens,
    NA
  )
  
  names(adequabilidade_sem_extrapolacao) <- "adequabilidade_sem_extrapolacao"
  
  adequabilidade_sob_extrapolacao <- terra::ifel(
    extrap_combinada == 1,
    ens,
    NA
  )
  
  names(adequabilidade_sob_extrapolacao) <- "adequabilidade_sob_extrapolacao"
  
  # ----------------------------------------------------------
  # Salvar rasters individuais
  # ----------------------------------------------------------
  
  terra::writeRaster(
    extrap_combinada,
    paste0(
      "resultados/unidade14/extrapolacao/extrapolacao_combinada_",
      ssp,
      "_",
      periodo,
      "_",
      modelo_gcm,
      ".tif"
    ),
    overwrite = TRUE
  )
  
  terra::writeRaster(
    adequabilidade_sem_extrapolacao,
    paste0(
      "resultados/unidade14/extrapolacao/adequabilidade_sem_extrapolacao_",
      ssp,
      "_",
      periodo,
      "_",
      modelo_gcm,
      ".tif"
    ),
    overwrite = TRUE
  )
  
  terra::writeRaster(
    adequabilidade_sob_extrapolacao,
    paste0(
      "resultados/unidade14/extrapolacao/adequabilidade_sob_extrapolacao_",
      ssp,
      "_",
      periodo,
      "_",
      modelo_gcm,
      ".tif"
    ),
    overwrite = TRUE
  )
  
  stack_diag <- c(
    ens,
    mess,
    mop,
    extrap_mess,
    extrap_combinada,
    adequabilidade_sem_extrapolacao,
    adequabilidade_sob_extrapolacao
  )
  
  names(stack_diag) <- c(
    "adequabilidade",
    "MESS",
    "MOP_extrapolacao",
    "extrapolacao_MESS",
    "extrapolacao_combinada",
    "adequabilidade_sem_extrapolacao",
    "adequabilidade_sob_extrapolacao"
  )
  
  terra::writeRaster(
    stack_diag,
    paste0(
      "resultados/unidade14/extrapolacao/diagnostico_extrapolacao_adequabilidade_",
      ssp,
      "_",
      periodo,
      "_",
      modelo_gcm,
      ".tif"
    ),
    overwrite = TRUE
  )
  
  # ----------------------------------------------------------
  # Converter para data.frame
  # Importante: usar na.rm = FALSE para não remover células
  # onde apenas uma das camadas mascaradas é NA.
  # ----------------------------------------------------------
  
  df <- as.data.frame(
    stack_diag,
    xy = TRUE,
    na.rm = FALSE
  ) |>
    dplyr::rename(
      lon = x,
      lat = y
    ) |>
    dplyr::filter(
      !is.na(adequabilidade)
    ) |>
    dplyr::mutate(
      cenario = ssp,
      classe = dplyr::case_when(
        extrapolacao_combinada == 1 ~ "Sob extrapolação",
        extrapolacao_combinada == 0 ~ "Sem extrapolação",
        TRUE ~ NA_character_
      ),
      classe = factor(
        classe,
        levels = c("Sem extrapolação", "Sob extrapolação")
      )
    )
  
  if (nrow(df) == 0) {
    stop("O data.frame ficou vazio para o cenário ", ssp, ". Verifique os rasters de entrada.")
  }
  
  dados_all[[ssp]] <- df
  rasters_ref[[ssp]] <- ens
  
  # ----------------------------------------------------------
  # Resumo por área
  # ----------------------------------------------------------
  
  area_cell <- terra::cellSize(
    ens,
    unit = "km"
  )
  
  area_stack <- c(
    area_cell,
    extrap_combinada,
    ens
  )
  
  names(area_stack) <- c(
    "area_km2",
    "extrapolacao_combinada",
    "adequabilidade"
  )
  
  area_df <- as.data.frame(
    area_stack,
    xy = TRUE,
    na.rm = FALSE
  ) |>
    dplyr::rename(
      lon = x,
      lat = y
    ) |>
    dplyr::filter(
      !is.na(adequabilidade)
    )
  
  resumo_ssp <- area_df |>
    dplyr::mutate(
      classe = dplyr::case_when(
        extrapolacao_combinada == 1 ~ "Sob extrapolação",
        extrapolacao_combinada == 0 ~ "Sem extrapolação",
        TRUE ~ NA_character_
      )
    ) |>
    dplyr::filter(
      !is.na(classe)
    ) |>
    dplyr::group_by(classe) |>
    dplyr::summarise(
      cenario = ssp,
      periodo = periodo,
      gcm = modelo_gcm,
      area_km2 = sum(area_km2, na.rm = TRUE),
      adequabilidade_media = mean(adequabilidade, na.rm = TRUE),
      adequabilidade_mediana = median(adequabilidade, na.rm = TRUE),
      adequabilidade_max = max(adequabilidade, na.rm = TRUE),
      n_pixels = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::group_by(cenario) |>
    dplyr::mutate(
      proporcao_area = area_km2 / sum(area_km2, na.rm = TRUE)
    ) |>
    dplyr::ungroup()
  
  resumos[[ssp]] <- resumo_ssp
}

dados_all <- dplyr::bind_rows(dados_all)
resumos <- dplyr::bind_rows(resumos)

if (nrow(dados_all) == 0) {
  stop(
    "A tabela dados_all ficou vazia. Verifique se os rasters de ensemble, MESS e MOP possuem valores válidos."
  )
}

readr::write_csv(
  dados_all,
  "dados/unidade14/processados/adequabilidade_extrapolacao.csv"
)

readr::write_csv(
  resumos,
  "tabelas/unidade14/resumo_adequabilidade_extrapolacao.csv"
)

# ------------------------------------------------------------
# 7. Preparar bioma, área M e ocorrências
# ------------------------------------------------------------

ref <- rasters_ref[[1]]

bioma_proj <- bioma_raw |>
  sf::st_transform(5880) |>
  sf::st_make_valid() |>
  sf::st_union() |>
  sf::st_as_sf() |>
  sf::st_make_valid()

bioma <- bioma_proj |>
  sf::st_transform(terra::crs(ref)) |>
  sf::st_make_valid()

bioma_plot <- sf::st_transform(
  bioma,
  4326
)

bbox_bioma <- sf::st_bbox(
  bioma_plot
)

area_m_plot <- NULL

if (!is.null(area_m)) {
  area_m_plot <- area_m |>
    sf::st_make_valid() |>
    sf::st_transform(4326)
}

oc_sf <- sf::st_as_sf(
  oc,
  coords = c("lon", "lat"),
  crs = 4326,
  remove = FALSE
)

# ------------------------------------------------------------
# 8. Mapa binário de extrapolação combinada
# ------------------------------------------------------------

g_extrap <- ggplot() +
  geom_sf(
    data = bioma_plot,
    fill = "grey96",
    color = "grey35",
    linewidth = 0.30
  ) +
  geom_raster(
    data = dados_all,
    aes(
      x = lon,
      y = lat,
      fill = classe
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
  scale_fill_manual(
    name = "Classe",
    values = c(
      "Sem extrapolação" = "#1B7837",
      "Sob extrapolação" = "#B2182B"
    ),
    drop = FALSE
  ) +
  coord_sf(
    xlim = c(bbox_bioma["xmin"], bbox_bioma["xmax"]),
    ylim = c(bbox_bioma["ymin"], bbox_bioma["ymax"]),
    expand = FALSE
  ) +
  facet_wrap(
    ~ cenario,
    ncol = 2
  ) +
  ggspatial::annotation_scale(
    location = "bl",
    width_hint = 0.25,
    text_cex = 0.60
  ) +
  labs(
    title = expression("Diagnóstico de extrapolação para " * italic("Dinizia excelsa")),
    subtitle = "Extrapolação combinada por MESS < 0 ou MOP extrapolado",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_bw(base_size = 10) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10),
    strip.text = element_text(face = "bold"),
    panel.grid.major = element_line(color = "grey88", linewidth = 0.20),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )

ggsave(
  "figuras/unidade14/unidade14_adequabilidade_extrapolacao.png",
  plot = g_extrap,
  width = 12,
  height = 9,
  dpi = 600
)

# ------------------------------------------------------------
# 9. Mapa de adequabilidade confiável
# ------------------------------------------------------------

g_conf <- ggplot() +
  geom_sf(
    data = bioma_plot,
    fill = "grey96",
    color = "grey35",
    linewidth = 0.30
  ) +
  geom_raster(
    data = dados_all,
    aes(
      x = lon,
      y = lat,
      fill = adequabilidade_sem_extrapolacao
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
    name = "Adequabilidade",
    option = "viridis",
    limits = c(0, 1),
    na.value = "grey90"
  ) +
  coord_sf(
    xlim = c(bbox_bioma["xmin"], bbox_bioma["xmax"]),
    ylim = c(bbox_bioma["ymin"], bbox_bioma["ymax"]),
    expand = FALSE
  ) +
  facet_wrap(
    ~ cenario,
    ncol = 2
  ) +
  ggspatial::annotation_scale(
    location = "bl",
    width_hint = 0.25,
    text_cex = 0.60
  ) +
  labs(
    title = expression("Adequabilidade futura fora de extrapolação para " * italic("Dinizia excelsa")),
    subtitle = "Áreas em cinza representam regiões sob extrapolação ambiental combinada",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_bw(base_size = 10) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10),
    strip.text = element_text(face = "bold"),
    panel.grid.major = element_line(color = "grey88", linewidth = 0.20),
    panel.grid.minor = element_blank()
  )

ggsave(
  "figuras/unidade14/unidade14_adequabilidade_sem_extrapolacao.png",
  plot = g_conf,
  width = 12,
  height = 9,
  dpi = 600
)

# ------------------------------------------------------------
# 10. Boxplot de adequabilidade por classe
# ------------------------------------------------------------

g_box <- ggplot(
  dados_all,
  aes(
    x = classe,
    y = adequabilidade,
    fill = classe
  )
) +
  geom_boxplot(
    outlier.alpha = 0.20,
    linewidth = 0.25
  ) +
  facet_wrap(
    ~ cenario,
    ncol = 2
  ) +
  scale_fill_manual(
    values = c(
      "Sem extrapolação" = "#1B7837",
      "Sob extrapolação" = "#B2182B"
    ),
    drop = FALSE
  ) +
  labs(
    title = "Adequabilidade futura por classe de extrapolação",
    subtitle = "Comparação entre pixels com e sem extrapolação ambiental combinada",
    x = NULL,
    y = "Adequabilidade futura"
  ) +
  theme_bw(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "none",
    panel.grid.minor = element_blank()
  )

ggsave(
  "figuras/unidade14/unidade14_boxplot_adequabilidade_extrapolacao.png",
  plot = g_box,
  width = 10,
  height = 7,
  dpi = 600
)

# ------------------------------------------------------------
# 11. Figura composta
# ------------------------------------------------------------

fig_composta <- (g_extrap / g_conf / g_box) +
  patchwork::plot_annotation(
    title = expression("Adequabilidade futura e extrapolação ambiental para " * italic("Dinizia excelsa")),
    subtitle = "Diagnóstico combinado por MESS e MOP aplicado aos ensembles futuros"
  ) &
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 10)
  )

ggsave(
  "figuras/unidade14/unidade14_adequabilidade_extrapolacao_patchwork.png",
  plot = fig_composta,
  width = 12,
  height = 22,
  dpi = 600
)

# ------------------------------------------------------------
# 12. Mensagem final
# ------------------------------------------------------------

message("Adequabilidade sob extrapolação avaliada com sucesso.")
message("Cenários processados: ", paste(cenarios, collapse = ", "))

print(resumos)
