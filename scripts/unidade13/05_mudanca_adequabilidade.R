source("scripts/_bootstrap.R")

# ============================================================
# Unidade 13 - Projeções climáticas futuras em SDM
# Script 05: Mudança de adequabilidade futura
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
dir.create("resultados/unidade13/mudancas", recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# 4. Configurações
# ------------------------------------------------------------

cenarios <- c("ssp126", "ssp245", "ssp370", "ssp585")

periodo <- "2061-2080"

modelo_gcm <- "MIROC6"

limiar_mudanca <- 0.10

arquivo_atual <- "resultados/unidade11/ensemble_media_simples_dinizia.tif"

arquivo_bioma <- "dados/unidade01/brutos/amazon_biome_border.shp"

arquivo_m <- "dados/unidade05/processados/area_m_dinizia.gpkg"

arquivo_oc <- "dados/unidade05/processados/presencas_area_m_dinizia.csv"

if (!file.exists(arquivo_atual)) {
  stop("Ensemble atual não encontrado. Execute a Unidade 11.")
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
# 5. Leitura dos dados
# ------------------------------------------------------------

atual <- terra::rast(
  arquivo_atual
)

names(atual) <- "ensemble_atual"

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
# 6. Função para categorizar mudança
# ------------------------------------------------------------

categorizar_mudanca <- function(delta, limiar = 0.10) {
  
  terra::classify(
    delta,
    rcl = matrix(
      c(
        -Inf, -limiar, -1,
        -limiar, limiar, 0,
        limiar, Inf, 1
      ),
      ncol = 3,
      byrow = TRUE
    )
  )
}

# ------------------------------------------------------------
# 7. Calcular mudanças por cenário
# ------------------------------------------------------------

mudancas <- list()
categorias <- list()
resumos <- list()

for (ssp in cenarios) {
  
  message("Calculando mudança de adequabilidade para: ", ssp)
  
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
  
  delta <- futuro - atual
  names(delta) <- paste0("delta_", ssp)
  
  categoria <- categorizar_mudanca(
    delta,
    limiar = limiar_mudanca
  )
  
  names(categoria) <- paste0("classe_mudanca_", ssp)
  
  terra::writeRaster(
    delta,
    paste0(
      "resultados/unidade13/mudancas/mudanca_adequabilidade_",
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
    categoria,
    paste0(
      "resultados/unidade13/mudancas/classe_mudanca_adequabilidade_",
      ssp,
      "_",
      periodo,
      "_",
      modelo_gcm,
      ".tif"
    ),
    overwrite = TRUE
  )
  
  df_delta <- as.data.frame(
    delta,
    xy = TRUE,
    na.rm = TRUE
  ) |>
    rename(
      lon = x,
      lat = y,
      delta = all_of(names(delta))
    ) |>
    mutate(
      cenario = ssp
    )
  
  df_cat <- as.data.frame(
    categoria,
    xy = TRUE,
    na.rm = TRUE
  ) |>
    rename(
      lon = x,
      lat = y,
      classe_valor = all_of(names(categoria))
    ) |>
    mutate(
      cenario = ssp,
      classe = case_when(
        classe_valor == -1 ~ "Perda",
        classe_valor == 0 ~ "Estável",
        classe_valor == 1 ~ "Ganho",
        TRUE ~ NA_character_
      ),
      classe = factor(
        classe,
        levels = c("Perda", "Estável", "Ganho")
      )
    )
  
  mudancas[[ssp]] <- df_delta
  categorias[[ssp]] <- df_cat
  
  freq_cat <- df_cat |>
    count(
      classe,
      name = "n_pixels"
    ) |>
    mutate(
      cenario = ssp,
      proporcao = n_pixels / sum(n_pixels),
      limiar_mudanca = limiar_mudanca
    )
  
  resumo_delta <- tibble(
    cenario = ssp,
    periodo = periodo,
    gcm = modelo_gcm,
    n_pixels = nrow(df_delta),
    delta_min = min(df_delta$delta, na.rm = TRUE),
    delta_q25 = quantile(df_delta$delta, 0.25, na.rm = TRUE),
    delta_mediana = median(df_delta$delta, na.rm = TRUE),
    delta_media = mean(df_delta$delta, na.rm = TRUE),
    delta_q75 = quantile(df_delta$delta, 0.75, na.rm = TRUE),
    delta_max = max(df_delta$delta, na.rm = TRUE),
    delta_sd = sd(df_delta$delta, na.rm = TRUE),
    prop_perda = freq_cat$proporcao[freq_cat$classe == "Perda"] %||% 0,
    prop_estavel = freq_cat$proporcao[freq_cat$classe == "Estável"] %||% 0,
    prop_ganho = freq_cat$proporcao[freq_cat$classe == "Ganho"] %||% 0
  )
  
  resumos[[ssp]] <- resumo_delta
}

df_mudancas <- bind_rows(mudancas)
df_categorias <- bind_rows(categorias)
resumos <- bind_rows(resumos)

write_csv(
  df_mudancas,
  "dados/unidade13/processados/mudanca_adequabilidade_futura.csv"
)

write_csv(
  df_categorias,
  "dados/unidade13/processados/classe_mudanca_adequabilidade_futura.csv"
)

write_csv(
  resumos,
  "tabelas/unidade13/resumo_mudanca_adequabilidade_futura.csv"
)

# ------------------------------------------------------------
# 8. Preparar elementos cartográficos
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
# 9. Limite simétrico para mapa contínuo
# ------------------------------------------------------------

lim_delta <- max(
  abs(df_mudancas$delta),
  na.rm = TRUE
)

# ------------------------------------------------------------
# 10. Mapa contínuo de mudança
# ------------------------------------------------------------

g_delta <- ggplot() +
  geom_sf(
    data = bioma_plot,
    fill = "grey96",
    color = "grey35",
    linewidth = 0.30
  ) +
  geom_raster(
    data = df_mudancas,
    aes(
      x = lon,
      y = lat,
      fill = delta
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
  scale_fill_gradient2(
    name = expression(Delta~"adequabilidade"),
    low = "#2166AC",
    mid = "white",
    high = "#B2182B",
    midpoint = 0,
    limits = c(-lim_delta, lim_delta),
    na.value = NA
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
    title = expression("Mudança futura de adequabilidade para " * italic("Dinizia excelsa")),
    subtitle = paste0(
      "Diferença entre ensemble futuro e ensemble atual | ",
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
    panel.grid.minor = element_blank()
  )

ggsave(
  "figuras/unidade13/unidade13_mudanca_adequabilidade.png",
  plot = g_delta,
  width = 12,
  height = 9,
  dpi = 600
)

# ------------------------------------------------------------
# 11. Mapa categórico de perda, estabilidade e ganho
# ------------------------------------------------------------

g_cat <- ggplot() +
  geom_sf(
    data = bioma_plot,
    fill = "grey96",
    color = "grey35",
    linewidth = 0.30
  ) +
  geom_raster(
    data = df_categorias,
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
    size = 0.55,
    stroke = 0.14,
    alpha = 0.65
  ) +
  scale_fill_manual(
    name = "Classe",
    values = c(
      "Perda" = "#B2182B",
      "Estável" = "grey85",
      "Ganho" = "#2166AC"
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
    title = expression("Classes de mudança futura para " * italic("Dinizia excelsa")),
    subtitle = paste0(
      "Perda, estabilidade e ganho definidos por |delta adequabilidade| >= ",
      limiar_mudanca
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
    legend.position = "right"
  )

ggsave(
  "figuras/unidade13/unidade13_classes_mudanca_adequabilidade.png",
  plot = g_cat,
  width = 12,
  height = 9,
  dpi = 600
)

# ------------------------------------------------------------
# 12. Figura composta
# ------------------------------------------------------------

fig_composta <- g_delta / g_cat +
  patchwork::plot_annotation(
    title = expression("Mudança futura de adequabilidade para " * italic("Dinizia excelsa")),
    subtitle = "Mapas contínuos de variação e classes espaciais de perda, estabilidade e ganho"
  ) &
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 10)
  )

ggsave(
  "figuras/unidade13/unidade13_mudanca_adequabilidade_patchwork.png",
  plot = fig_composta,
  width = 12,
  height = 16,
  dpi = 600
)

# ------------------------------------------------------------
# 13. Mensagem final
# ------------------------------------------------------------

message("Mudanças de adequabilidade futura calculadas com sucesso.")
message("Limiar de mudança usado para classes: ", limiar_mudanca)
message("Cenários processados: ", paste(cenarios, collapse = ", "))

print(resumos)
