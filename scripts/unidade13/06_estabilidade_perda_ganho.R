source("scripts/_bootstrap.R")

# ============================================================
# Unidade 13 - Projeções climáticas futuras em SDM
# Script 06: Estabilidade, perda e ganho
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

dir.create("dados/unidade13/processados", recursive = TRUE, showWarnings = FALSE)
dir.create("figuras/unidade13", recursive = TRUE, showWarnings = FALSE)
dir.create("tabelas/unidade13", recursive = TRUE, showWarnings = FALSE)
dir.create("resultados/unidade13/binarios", recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# 4. Configurações
# ------------------------------------------------------------

cenarios <- c("ssp126", "ssp245", "ssp370", "ssp585")

periodo <- "2061-2080"

modelo_gcm <- "MIROC6"

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
  stop("Limite do bioma Amazônia não encontrado: ", arquivo_bioma)
}

if (!file.exists(arquivo_oc)) {
  stop("Arquivo de ocorrências não encontrado: ", arquivo_oc)
}

if (!file.exists(arquivo_m)) {
  warning("Área M não encontrada. Os mapas serão gerados sem contorno da área M.")
}

# ------------------------------------------------------------
# 5. Ler dados
# ------------------------------------------------------------

atual <- terra::rast(
  arquivo_atual
)

names(atual) <- "ensemble_atual"

metricas <- read_csv(
  arquivo_metricas,
  show_col_types = FALSE
) |>
  mutate(
    modelo = as.character(modelo),
    limiar_TSS = as.numeric(limiar_TSS)
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

if (!all(c("lon", "lat") %in% names(oc))) {
  stop("O arquivo de ocorrências precisa conter as colunas 'lon' e 'lat'.")
}

# ------------------------------------------------------------
# 6. Definir limiar do ensemble atual
# ------------------------------------------------------------

limiar_atual <- metricas |>
  filter(
    modelo %in% c(
      "Ensemble_media",
      "Ensemble média",
      "Ensemble_Media"
    )
  ) |>
  pull(limiar_TSS)

if (length(limiar_atual) == 0 || is.na(limiar_atual[1]) || !is.finite(limiar_atual[1])) {
  warning("Limiar TSS do ensemble média não encontrado. Usando limiar padrão 0.5.")
  limiar_atual <- 0.5
}

limiar_atual <- as.numeric(limiar_atual[1])

if (limiar_atual <= 0 || limiar_atual >= 1) {
  warning("Limiar TSS inválido. Usando limiar padrão 0.5.")
  limiar_atual <- 0.5
}

# ------------------------------------------------------------
# 7. Binarizar ensemble atual
# ------------------------------------------------------------

bin_atual <- terra::ifel(
  atual >= limiar_atual,
  1,
  0
)

names(bin_atual) <- "binario_atual"

terra::writeRaster(
  bin_atual,
  "resultados/unidade13/binarios/binario_atual_ensemble_dinizia.tif",
  overwrite = TRUE
)

# ------------------------------------------------------------
# 8. Classificar estabilidade, perda e ganho
# ------------------------------------------------------------

classes_all <- list()
resumos <- list()

for (ssp in cenarios) {
  
  message("Classificando estabilidade, perda e ganho para: ", ssp)
  
  arquivo_futuro <- paste0(
    "resultados/unidade13/ensemble/ensemble_futuro_",
    ssp,
    "_",
    periodo,
    "_",
    modelo_gcm,
    ".tif"
  )
  
  if (!file.exists(arquivo_futuro)) {
    stop(
      "Ensemble futuro não encontrado para ",
      ssp,
      ". Execute o Script 04 da Unidade 13."
    )
  }
  
  futuro <- terra::rast(
    arquivo_futuro
  )
  
  names(futuro) <- "ensemble_futuro"
  
  if (!terra::compareGeom(atual, futuro, stopOnError = FALSE)) {
    futuro <- terra::resample(
      futuro,
      atual,
      method = "bilinear"
    )
  }
  
  bin_futuro <- terra::ifel(
    futuro >= limiar_atual,
    1,
    0
  )
  
  names(bin_futuro) <- paste0("binario_futuro_", ssp)
  
  terra::writeRaster(
    bin_futuro,
    paste0(
      "resultados/unidade13/binarios/binario_futuro_",
      ssp,
      "_",
      periodo,
      "_",
      modelo_gcm,
      ".tif"
    ),
    overwrite = TRUE
  )
  
  # Códigos:
  # 0 = atual 0, futuro 0 = Inadequado estável
  # 1 = atual 1, futuro 0 = Perda potencial
  # 2 = atual 0, futuro 1 = Ganho potencial
  # 3 = atual 1, futuro 1 = Adequado estável
  
  classe <- bin_atual + (bin_futuro * 2)
  
  names(classe) <- paste0("classe_", ssp)
  
  terra::writeRaster(
    classe,
    paste0(
      "resultados/unidade13/binarios/estabilidade_perda_ganho_",
      ssp,
      "_",
      periodo,
      "_",
      modelo_gcm,
      ".tif"
    ),
    overwrite = TRUE
  )
  
  df <- as.data.frame(
    classe,
    xy = TRUE,
    na.rm = TRUE
  ) |>
    rename(
      lon = x,
      lat = y,
      classe = all_of(names(classe))
    ) |>
    mutate(
      cenario = ssp,
      classe_nome = case_when(
        classe == 0 ~ "Inadequado estável",
        classe == 1 ~ "Perda potencial",
        classe == 2 ~ "Ganho potencial",
        classe == 3 ~ "Adequado estável",
        TRUE ~ NA_character_
      ),
      classe_nome = factor(
        classe_nome,
        levels = c(
          "Inadequado estável",
          "Perda potencial",
          "Ganho potencial",
          "Adequado estável"
        )
      )
    )
  
  classes_all[[ssp]] <- df
  
  resumo_ssp <- df |>
    count(
      classe_nome,
      name = "n_pixels"
    ) |>
    mutate(
      cenario = ssp,
      periodo = periodo,
      gcm = modelo_gcm,
      limiar = limiar_atual,
      proporcao = n_pixels / sum(n_pixels)
    ) |>
    select(
      cenario,
      periodo,
      gcm,
      limiar,
      classe_nome,
      n_pixels,
      proporcao
    )
  
  resumos[[ssp]] <- resumo_ssp
}

df_all <- bind_rows(classes_all)

resumos <- bind_rows(resumos)

write_csv(
  df_all,
  "dados/unidade13/processados/estabilidade_perda_ganho.csv"
)

write_csv(
  resumos,
  "tabelas/unidade13/resumo_estabilidade_perda_ganho.csv"
)

# ------------------------------------------------------------
# 9. Preparar elementos cartográficos
# ------------------------------------------------------------

bioma_proj <- bioma_raw |>
  sf::st_transform(5880) |>
  sf::st_make_valid() |>
  sf::st_union() |>
  sf::st_as_sf() |>
  sf::st_make_valid()

bioma <- bioma_proj |>
  sf::st_transform(terra::crs(atual)) |>
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
# 10. Mapa principal
# ------------------------------------------------------------

cores_classes <- c(
  "Inadequado estável" = "grey85",
  "Perda potencial" = "#B2182B",
  "Ganho potencial" = "#2166AC",
  "Adequado estável" = "#1B7837"
)

g_classes <- ggplot() +
  geom_sf(
    data = bioma_plot,
    fill = "grey96",
    color = "grey35",
    linewidth = 0.30
  ) +
  geom_raster(
    data = df_all,
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
    size = 0.55,
    stroke = 0.14,
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
    ~ cenario,
    ncol = 2
  ) +
  ggspatial::annotation_scale(
    location = "bl",
    width_hint = 0.25,
    text_cex = 0.60
  ) +
  labs(
    title = expression("Estabilidade, perda e ganho para " * italic("Dinizia excelsa")),
    subtitle = paste0(
      "Comparação entre ensemble atual e futuro usando limiar TSS = ",
      round(limiar_atual, 3),
      " | ",
      modelo_gcm,
      " | ",
      periodo
    ),
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
  "figuras/unidade13/unidade13_estabilidade_perda_ganho.png",
  plot = g_classes,
  width = 12,
  height = 9,
  dpi = 600
)

# ------------------------------------------------------------
# 11. Gráfico de barras das proporções
# ------------------------------------------------------------

g_barras <- ggplot(
  resumos,
  aes(
    x = cenario,
    y = proporcao,
    fill = classe_nome
  )
) +
  geom_col(
    color = "grey25",
    linewidth = 0.15,
    position = "stack"
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
    title = "Proporção de áreas por classe de mudança",
    subtitle = expression("Estabilidade, perda e ganho projetados para " * italic("Dinizia excelsa")),
    x = "Cenário",
    y = "Proporção de pixels"
  ) +
  theme_bw(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "right",
    panel.grid.minor = element_blank()
  )

ggsave(
  "figuras/unidade13/unidade13_proporcao_estabilidade_perda_ganho.png",
  plot = g_barras,
  width = 9,
  height = 6,
  dpi = 600
)

# ------------------------------------------------------------
# 12. Figura composta
# ------------------------------------------------------------

fig_composta <- g_classes / g_barras +
  patchwork::plot_layout(
    heights = c(1.4, 0.8)
  ) +
  patchwork::plot_annotation(
    title = expression("Síntese espacial de estabilidade, perda e ganho para " * italic("Dinizia excelsa")),
    subtitle = "Mapas categóricos e proporções relativas por cenário climático futuro"
  ) &
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 10)
  )

ggsave(
  "figuras/unidade13/unidade13_estabilidade_perda_ganho_patchwork.png",
  plot = fig_composta,
  width = 12,
  height = 15,
  dpi = 600
)

# ------------------------------------------------------------
# 13. Mensagem final
# ------------------------------------------------------------

message("Mapas de estabilidade, perda e ganho gerados com sucesso.")
message("Limiar usado: ", round(limiar_atual, 3))
message("Cenários processados: ", paste(cenarios, collapse = ", "))

print(resumos)
