source("scripts/_bootstrap.R")

# ============================================================
# Unidade 14 - Transferência, extrapolação, MESS e MOP
# Script 04: Calcular MOP simplificado para cenários futuros
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
  "tidyr",
  "FNN",
  "tibble",
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
dir.create("resultados/unidade14/mop", recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# 4. Configurações
# ------------------------------------------------------------

cenarios <- c("ssp126", "ssp245", "ssp370", "ssp585")

periodo <- "2061-2080"

modelo_gcm <- "MIROC6"

n_amostra_calibracao <- 10000

quantil_limiar_mop <- 0.95

# ------------------------------------------------------------
# 5. Arquivos de entrada
# ------------------------------------------------------------

arquivo_calibracao <- "dados/unidade06/processados/dados_presenca_background_bioma_glm_dinizia.csv"

arquivo_vars <- "dados/unidade04/processados/variaveis_selecionadas_final.csv"

arquivo_bioma <- "dados/unidade01/brutos/amazon_biome_border.shp"

arquivo_m <- "dados/unidade05/processados/area_m_dinizia.gpkg"

arquivo_oc <- "dados/unidade05/processados/presencas_area_m_dinizia.csv"

if (!file.exists(arquivo_calibracao)) {
  stop("Base de calibração não encontrada. Execute a Unidade 6, Script 02.")
}

if (!file.exists(arquivo_vars)) {
  stop("Arquivo de variáveis selecionadas não encontrado: ", arquivo_vars)
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
# 6. Leitura da calibração
# ------------------------------------------------------------

dados_cal <- read_csv(
  arquivo_calibracao,
  show_col_types = FALSE
)

vars <- read_csv(
  arquivo_vars,
  show_col_types = FALSE
)$variavel |>
  unique()

vars <- vars[vars %in% names(dados_cal)]

if (length(vars) < 2) {
  stop("Número insuficiente de variáveis ambientais para calcular MOP.")
}

cal_env <- dados_cal |>
  select(all_of(vars)) |>
  tidyr::drop_na()

if (nrow(cal_env) < 10) {
  stop("Poucos pontos de calibração após remover NA.")
}

# ------------------------------------------------------------
# 7. Amostragem e padronização da calibração
# ------------------------------------------------------------

set.seed(123)

cal_env_amostra <- cal_env |>
  sample_n(
    size = min(n_amostra_calibracao, nrow(cal_env))
  )

medias <- sapply(
  cal_env_amostra,
  mean,
  na.rm = TRUE
)

sds <- sapply(
  cal_env_amostra,
  sd,
  na.rm = TRUE
)

sds[sds == 0 | is.na(sds)] <- 1

cal_scaled <- scale(
  cal_env_amostra,
  center = medias,
  scale = sds
)

cal_scaled <- as.matrix(cal_scaled)

# ------------------------------------------------------------
# 8. Limiar MOP pela distância interna de calibração
# ------------------------------------------------------------

nn_cal <- FNN::get.knn(
  data = cal_scaled,
  k = 2
)

dist_cal <- nn_cal$nn.dist[, 2]

limiar_mop <- as.numeric(
  stats::quantile(
    dist_cal,
    probs = quantil_limiar_mop,
    na.rm = TRUE
  )
)

limiar_tab <- tibble(
  metodo = "Distância ao vizinho mais próximo no espaço ambiental padronizado",
  quantil = quantil_limiar_mop,
  limiar_mop = limiar_mop,
  n_calibracao_total = nrow(cal_env),
  n_calibracao_amostra = nrow(cal_env_amostra),
  n_variaveis = length(vars),
  variaveis = paste(vars, collapse = ", ")
)

write_csv(
  limiar_tab,
  "tabelas/unidade14/limiar_mop_calibracao.csv"
)

parametros_padronizacao <- tibble(
  variavel = vars,
  media_calibracao = as.numeric(medias[vars]),
  sd_calibracao = as.numeric(sds[vars])
)

write_csv(
  parametros_padronizacao,
  "tabelas/unidade14/parametros_padronizacao_mop.csv"
)

# ------------------------------------------------------------
# 9. Elementos cartográficos
# ------------------------------------------------------------

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
  stop("O arquivo de ocorrências precisa conter as colunas lon e lat.")
}

# ------------------------------------------------------------
# 10. Calcular MOP por cenário
# ------------------------------------------------------------

mop_all <- list()
resumos <- list()
rasters_mop <- list()

for (ssp in cenarios) {
  
  message("Calculando MOP para ", ssp)
  
  arquivo_fut <- paste0(
    "dados/unidade13/processados/variaveis_futuras_",
    ssp,
    "_",
    periodo,
    "_",
    modelo_gcm,
    ".tif"
  )
  
  if (!file.exists(arquivo_fut)) {
    stop(
      "Variáveis futuras não encontradas para ",
      ssp,
      ". Execute a Unidade 13, Script 02."
    )
  }
  
  r_fut <- terra::rast(
    arquivo_fut
  )
  
  vars_disp <- vars[vars %in% names(r_fut)]
  
  if (length(vars_disp) != length(vars)) {
    stop(
      "Variáveis ausentes no raster futuro de ",
      ssp,
      ": ",
      paste(setdiff(vars, vars_disp), collapse = ", ")
    )
  }
  
  r_fut <- r_fut[[vars]]
  
  df_fut <- as.data.frame(
    r_fut,
    xy = TRUE,
    na.rm = TRUE
  ) |>
    rename(
      lon = x,
      lat = y
    )
  
  fut_env <- df_fut |>
    select(all_of(vars))
  
  for (v in vars) {
    fut_env[[v]] <- as.numeric(fut_env[[v]])
  }
  
  fut_scaled <- scale(
    fut_env,
    center = medias,
    scale = sds
  )
  
  fut_scaled <- as.matrix(fut_scaled)
  
  nn <- FNN::get.knnx(
    data = cal_scaled,
    query = fut_scaled,
    k = 1
  )
  
  mop_dist <- as.numeric(
    nn$nn.dist[, 1]
  )
  
  mop_extrap <- ifelse(
    mop_dist > limiar_mop,
    1,
    0
  )
  
  mop_df <- df_fut |>
    select(lon, lat) |>
    mutate(
      MOP_distancia = mop_dist,
      MOP_extrapolacao = mop_extrap,
      classe_mop = ifelse(
        MOP_extrapolacao == 1,
        "Extrapolação",
        "Sem extrapolação"
      ),
      cenario = ssp
    )
  
  write_csv(
    mop_df,
    paste0(
      "dados/unidade14/processados/mop_",
      ssp,
      "_",
      periodo,
      "_",
      modelo_gcm,
      ".csv"
    )
  )
  
  r_mop <- r_fut[[1]]
  terra::values(r_mop) <- NA_real_
  
  cel <- terra::cellFromXY(
    r_mop,
    as.matrix(mop_df[, c("lon", "lat")])
  )
  
  r_mop[cel] <- mop_df$MOP_distancia
  names(r_mop) <- paste0("MOP_distancia_", ssp)
  
  r_mop_bin <- r_fut[[1]]
  terra::values(r_mop_bin) <- NA_real_
  r_mop_bin[cel] <- mop_df$MOP_extrapolacao
  names(r_mop_bin) <- paste0("MOP_extrapolacao_", ssp)
  
  terra::writeRaster(
    r_mop,
    paste0(
      "resultados/unidade14/mop/mop_distancia_",
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
    r_mop_bin,
    paste0(
      "resultados/unidade14/mop/mop_extrapolacao_",
      ssp,
      "_",
      periodo,
      "_",
      modelo_gcm,
      ".tif"
    ),
    overwrite = TRUE
  )
  
  rasters_mop[[ssp]] <- r_mop
  
  resumo_ssp <- mop_df |>
    summarise(
      cenario = ssp,
      periodo = periodo,
      gcm = modelo_gcm,
      n_pixels = n(),
      MOP_min = min(MOP_distancia, na.rm = TRUE),
      MOP_q25 = quantile(MOP_distancia, 0.25, na.rm = TRUE),
      MOP_mediana = median(MOP_distancia, na.rm = TRUE),
      MOP_media = mean(MOP_distancia, na.rm = TRUE),
      MOP_q75 = quantile(MOP_distancia, 0.75, na.rm = TRUE),
      MOP_max = max(MOP_distancia, na.rm = TRUE),
      MOP_sd = sd(MOP_distancia, na.rm = TRUE),
      limiar_mop = limiar_mop,
      n_extrapolacao = sum(MOP_extrapolacao == 1, na.rm = TRUE),
      prop_extrapolacao = mean(MOP_extrapolacao == 1, na.rm = TRUE)
    )
  
  resumos[[ssp]] <- resumo_ssp
  mop_all[[ssp]] <- mop_df
}

mop_all <- bind_rows(mop_all)
resumos <- bind_rows(resumos)

write_csv(
  mop_all,
  "dados/unidade14/processados/mop_todos_cenarios.csv"
)

write_csv(
  resumos,
  "tabelas/unidade14/resumo_mop_cenarios.csv"
)

# ------------------------------------------------------------
# 11. Preparar bioma, área M e ocorrências para mapas
# ------------------------------------------------------------

ref <- rasters_mop[[1]]

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
# 12. Mapa contínuo de distância MOP
# ------------------------------------------------------------

g_mop <- ggplot() +
  geom_sf(
    data = bioma_plot,
    fill = "grey96",
    color = "grey35",
    linewidth = 0.30
  ) +
  geom_raster(
    data = mop_all,
    aes(
      x = lon,
      y = lat,
      fill = MOP_distancia
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
    name = "Distância MOP",
    option = "magma",
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
    title = expression("MOP futuro para " * italic("Dinizia excelsa")),
    subtitle = "Distância ambiental ao domínio de calibração no espaço ambiental padronizado",
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
  "figuras/unidade14/unidade14_mop_cenarios.png",
  plot = g_mop,
  width = 12,
  height = 9,
  dpi = 600
)

# ------------------------------------------------------------
# 13. Mapa binário de extrapolação MOP
# ------------------------------------------------------------

mop_all <- mop_all |>
  mutate(
    classe_mop = factor(
      classe_mop,
      levels = c("Sem extrapolação", "Extrapolação")
    )
  )

g_mop_bin <- ggplot() +
  geom_sf(
    data = bioma_plot,
    fill = "grey96",
    color = "grey35",
    linewidth = 0.30
  ) +
  geom_raster(
    data = mop_all,
    aes(
      x = lon,
      y = lat,
      fill = classe_mop
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
      "Sem extrapolação" = "grey85",
      "Extrapolação" = "#B2182B"
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
    title = expression("Extrapolação MOP para " * italic("Dinizia excelsa")),
    subtitle = paste0(
      "Células com distância MOP superior ao quantil ",
      quantil_limiar_mop,
      " da calibração"
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
  "figuras/unidade14/unidade14_mop_extrapolacao_cenarios.png",
  plot = g_mop_bin,
  width = 12,
  height = 9,
  dpi = 600
)

# ------------------------------------------------------------
# 14. Figura composta
# ------------------------------------------------------------

fig_composta <- g_mop / g_mop_bin +
  patchwork::plot_annotation(
    title = expression("MOP e extrapolação ambiental futura para " * italic("Dinizia excelsa")),
    subtitle = "Distância ambiental ao domínio calibrado e máscara binária de extrapolação"
  ) &
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 10)
  )

ggsave(
  "figuras/unidade14/unidade14_mop_extrapolacao_patchwork.png",
  plot = fig_composta,
  width = 12,
  height = 16,
  dpi = 600
)

# ------------------------------------------------------------
# 15. Mensagem final
# ------------------------------------------------------------

message("MOP calculado para todos os cenários com sucesso.")
message("Limiar MOP: ", round(limiar_mop, 4))
message("Cenários processados: ", paste(cenarios, collapse = ", "))

print(resumos)
