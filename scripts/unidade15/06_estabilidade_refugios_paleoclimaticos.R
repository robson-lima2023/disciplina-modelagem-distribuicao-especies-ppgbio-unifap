source("scripts/_bootstrap.R")

# ============================================================
# Unidade 15 - Paleoclima e nicho climático passado
# Script 06: Estabilidade histórica e refúgios climáticos
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

dir.create("dados/unidade15/processados", recursive = TRUE, showWarnings = FALSE)
dir.create("figuras/unidade15", recursive = TRUE, showWarnings = FALSE)
dir.create("tabelas/unidade15", recursive = TRUE, showWarnings = FALSE)
dir.create("resultados/unidade15/estabilidade", recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# 4. Configurações
# ------------------------------------------------------------

periodos <- tibble::tibble(
  periodo = c("lig", "lgm", "holoceno_medio"),
  periodo_legenda = c(
    "Último Interglacial",
    "Último Máximo Glacial",
    "Holoceno Médio"
  ),
  ordem = c(1, 2, 3)
)

arquivo_atual <- "resultados/unidade11/ensemble_media_simples_dinizia.tif"
arquivo_metricas <- "tabelas/unidade12/metricas_integradas_modelos.csv"

arquivo_bioma <- "dados/unidade01/brutos/amazon_biome_border.shp"
arquivo_m <- "dados/unidade05/processados/area_m_dinizia.gpkg"
arquivo_oc <- "dados/unidade05/processados/presencas_area_m_dinizia.csv"

if (!file.exists(arquivo_atual)) {
  stop("Ensemble atual não encontrado. Execute a Unidade 11.")
}

if (!file.exists(arquivo_metricas)) {
  stop("Métricas da Unidade 12 não encontradas. Execute a Unidade 12.")
}

if (!file.exists(arquivo_bioma)) {
  stop("Limite do bioma Amazônia não encontrado.")
}

if (!file.exists(arquivo_oc)) {
  stop("Arquivo de ocorrências não encontrado.")
}

if (!file.exists(arquivo_m)) {
  warning("Área M não encontrada. Os mapas serão gerados sem contorno da área M.")
}

# ------------------------------------------------------------
# 5. Ler ensemble atual e limiar
# ------------------------------------------------------------

atual <- terra::rast(arquivo_atual)
names(atual) <- "ensemble_atual"

metricas <- readr::read_csv(
  arquivo_metricas,
  show_col_types = FALSE
)

limiar <- metricas |>
  dplyr::filter(
    modelo %in% c("Ensemble média", "Ensemble_media", "Ensemble_Media")
  ) |>
  dplyr::pull(limiar_TSS)

if (length(limiar) == 0 || is.na(limiar[1]) || !is.finite(limiar[1])) {
  warning("Limiar TSS do ensemble não encontrado. Usando limiar padrão 0.5.")
  limiar <- 0.5
}

limiar <- as.numeric(limiar[1])

# ------------------------------------------------------------
# 6. Binarizar ensemble atual
# ------------------------------------------------------------

bin_atual <- terra::ifel(
  atual >= limiar,
  1,
  0
)

names(bin_atual) <- "binario_atual"

terra::writeRaster(
  bin_atual,
  "resultados/unidade15/estabilidade/binario_atual_ensemble_dinizia.tif",
  overwrite = TRUE
)

# ------------------------------------------------------------
# 7. Comparar presente e passado
# ------------------------------------------------------------

classes_all <- list()
resumos <- list()
rasters_ref <- list()

for (i in seq_len(nrow(periodos))) {
  
  periodo <- periodos$periodo[i]
  periodo_legenda <- periodos$periodo_legenda[i]
  
  message("Calculando estabilidade histórica para: ", periodo_legenda)
  
  arquivo_passado <- paste0(
    "resultados/unidade15/ensemble/ensemble_passado_",
    periodo,
    ".tif"
  )
  
  if (!file.exists(arquivo_passado)) {
    stop("Ensemble paleoclimático não encontrado para ", periodo, ". Execute o Script 04 da Unidade 15.")
  }
  
  passado <- terra::rast(arquivo_passado)
  names(passado) <- "ensemble_passado"
  
  if (!terra::compareGeom(atual, passado, stopOnError = FALSE)) {
    passado <- terra::resample(
      passado,
      atual,
      method = "bilinear"
    )
  }
  
  bin_passado <- terra::ifel(
    passado >= limiar,
    1,
    0
  )
  
  names(bin_passado) <- paste0("binario_passado_", periodo)
  
  # Código:
  # 0 = inadequado em ambos
  # 1 = adequado apenas no atual
  # 2 = adequado apenas no passado
  # 3 = adequado no atual e no passado
  
  classe <- bin_atual + bin_passado * 2
  
  names(classe) <- paste0("estabilidade_", periodo)
  
  terra::writeRaster(
    bin_passado,
    paste0(
      "resultados/unidade15/estabilidade/binario_passado_",
      periodo,
      ".tif"
    ),
    overwrite = TRUE
  )
  
  terra::writeRaster(
    classe,
    paste0(
      "resultados/unidade15/estabilidade/estabilidade_refugio_",
      periodo,
      ".tif"
    ),
    overwrite = TRUE
  )
  
  df <- as.data.frame(
    classe,
    xy = TRUE,
    na.rm = TRUE
  ) |>
    dplyr::rename(
      lon = x,
      lat = y,
      classe = all_of(names(classe))
    ) |>
    dplyr::mutate(
      periodo = periodo,
      periodo_legenda = periodo_legenda,
      ordem = periodos$ordem[i],
      classe_nome = dplyr::case_when(
        classe == 0 ~ "Inadequado em ambos",
        classe == 1 ~ "Adequado apenas atual",
        classe == 2 ~ "Adequado apenas passado",
        classe == 3 ~ "Estável adequado",
        TRUE ~ NA_character_
      ),
      classe_nome = factor(
        classe_nome,
        levels = c(
          "Inadequado em ambos",
          "Adequado apenas passado",
          "Adequado apenas atual",
          "Estável adequado"
        )
      )
    )
  
  classes_all[[periodo]] <- df
  rasters_ref[[periodo]] <- classe
  
  area_cell <- terra::cellSize(
    classe,
    unit = "km"
  )
  
  area_df <- as.data.frame(
    c(area_cell, classe),
    xy = FALSE,
    na.rm = TRUE
  )
  
  names(area_df) <- c("area_km2", "classe")
  
  resumo <- area_df |>
    dplyr::mutate(
      classe_nome = dplyr::case_when(
        classe == 0 ~ "Inadequado em ambos",
        classe == 1 ~ "Adequado apenas atual",
        classe == 2 ~ "Adequado apenas passado",
        classe == 3 ~ "Estável adequado",
        TRUE ~ NA_character_
      )
    ) |>
    dplyr::group_by(classe_nome) |>
    dplyr::summarise(
      periodo = periodo,
      periodo_legenda = periodo_legenda,
      limiar = limiar,
      area_km2 = sum(area_km2, na.rm = TRUE),
      n_pixels = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::group_by(periodo) |>
    dplyr::mutate(
      area_total_km2 = sum(area_km2, na.rm = TRUE),
      proporcao = area_km2 / area_total_km2
    ) |>
    dplyr::ungroup()
  
  resumos[[periodo]] <- resumo
}

classes_all <- dplyr::bind_rows(classes_all)
resumos <- dplyr::bind_rows(resumos)

readr::write_csv(
  classes_all,
  "dados/unidade15/processados/estabilidade_refugios_paleoclimaticos.csv"
)

readr::write_csv(
  resumos,
  "tabelas/unidade15/resumo_estabilidade_refugios_paleoclimaticos.csv"
)

# ------------------------------------------------------------
# 8. Elementos cartográficos
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

area_m_plot <- NULL

if (file.exists(arquivo_m)) {
  area_m_plot <- sf::st_read(
    arquivo_m,
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
# 9. Mapa de estabilidade histórica
# ------------------------------------------------------------

cores_classes <- c(
  "Inadequado em ambos" = "grey85",
  "Adequado apenas passado" = "#4575B4",
  "Adequado apenas atual" = "#FDAE61",
  "Estável adequado" = "#1A9850"
)

g_mapa <- ggplot() +
  geom_sf(
    data = bioma_plot,
    fill = "grey96",
    color = "grey35",
    linewidth = 0.30
  ) +
  geom_raster(
    data = classes_all,
    aes(
      x = lon,
      y = lat,
      fill = classe_nome
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
    values = cores_classes,
    drop = FALSE
  ) +
  coord_sf(
    xlim = c(bbox_bioma["xmin"], bbox_bioma["xmax"]),
    ylim = c(bbox_bioma["ymin"], bbox_bioma["ymax"]),
    expand = FALSE
  ) +
  facet_wrap(
    ~ periodo_legenda,
    ncol = 3
  ) +
  ggspatial::annotation_scale(
    location = "bl",
    width_hint = 0.22,
    text_cex = 0.55
  ) +
  labs(
    title = expression("Estabilidade paleoclimática de " * italic("Dinizia excelsa")),
    subtitle = "Áreas adequadas atuais e passadas como hipóteses de refúgios climáticos",
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
  "figuras/unidade15/unidade15_estabilidade_refugios.png",
  plot = g_mapa,
  width = 13,
  height = 6.8,
  dpi = 600
)

# ------------------------------------------------------------
# 10. Gráfico de proporção das classes
# ------------------------------------------------------------

g_bar <- ggplot(
  resumos,
  aes(
    x = periodo_legenda,
    y = proporcao,
    fill = classe_nome
  )
) +
  geom_col(
    color = "grey25",
    linewidth = 0.15
  ) +
  scale_fill_manual(
    values = cores_classes,
    drop = FALSE,
    name = "Classe"
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1)
  ) +
  labs(
    title = "Proporção de área por classe de estabilidade histórica",
    subtitle = expression("Refúgios climáticos potenciais para " * italic("Dinizia excelsa")),
    x = "Período paleoclimático",
    y = "Proporção da área"
  ) +
  theme_bw(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 20, hjust = 1),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

ggsave(
  "figuras/unidade15/unidade15_proporcao_estabilidade_refugios.png",
  plot = g_bar,
  width = 10,
  height = 6,
  dpi = 600
)

# ------------------------------------------------------------
# 11. Figura composta
# ------------------------------------------------------------

fig_composta <- g_mapa / g_bar +
  patchwork::plot_layout(
    heights = c(1.25, 0.85)
  ) +
  patchwork::plot_annotation(
    title = expression("Síntese espacial de refúgios climáticos históricos para " * italic("Dinizia excelsa")),
    subtitle = "Mapas categóricos e proporções de estabilidade, perda histórica e ganho atual"
  ) &
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 10)
  )

ggsave(
  "figuras/unidade15/unidade15_estabilidade_refugios_patchwork.png",
  plot = fig_composta,
  width = 13,
  height = 13,
  dpi = 600
)

# ------------------------------------------------------------
# 12. Mensagem final
# ------------------------------------------------------------

message("Estabilidade paleoclimática e refúgios calculados com sucesso.")
message("Limiar usado: ", round(limiar, 3))

print(resumos)
