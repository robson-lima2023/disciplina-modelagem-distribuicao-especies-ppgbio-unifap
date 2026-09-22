source("scripts/_bootstrap.R")

# ============================================================
# Unidade 14 - Transferência, extrapolação, MESS e MOP
# Script 02: Calcular MESS para cenários futuros
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
  "stringr",
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
dir.create("resultados/unidade14/mess", recursive = TRUE, showWarnings = FALSE)
dir.create("resultados/unidade14/extrapolacao", recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# 4. Configurações
# ------------------------------------------------------------

cenarios <- c("ssp126", "ssp245", "ssp370", "ssp585")

periodo <- "2061-2080"

modelo_gcm <- "MIROC6"

# ------------------------------------------------------------
# 5. Entradas
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
# 6. Leitura dos dados de calibração
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
  stop("Número insuficiente de variáveis ambientais na base de calibração.")
}

cal_env <- dados_cal |>
  select(all_of(vars)) |>
  tidyr::drop_na()

if (nrow(cal_env) == 0) {
  stop("A matriz ambiental de calibração está vazia após remover NA.")
}

# ------------------------------------------------------------
# 7. Limites ambientais da calibração
# ------------------------------------------------------------

limites_long <- tibble(
  variavel = vars,
  min_cal = sapply(
    vars,
    function(v) min(cal_env[[v]], na.rm = TRUE)
  ),
  max_cal = sapply(
    vars,
    function(v) max(cal_env[[v]], na.rm = TRUE)
  ),
  media_cal = sapply(
    vars,
    function(v) mean(cal_env[[v]], na.rm = TRUE)
  ),
  sd_cal = sapply(
    vars,
    function(v) sd(cal_env[[v]], na.rm = TRUE)
  )
) |>
  mutate(
    amplitude_cal = max_cal - min_cal
  )

write_csv(
  limites_long,
  "tabelas/unidade14/limites_calibracao_mess.csv"
)

# ------------------------------------------------------------
# 8. Função para calcular MESS em data.frame
# ------------------------------------------------------------

calcular_mess_df <- function(df, limites_long) {
  
  out <- list()
  
  for (i in seq_len(nrow(limites_long))) {
    
    v <- limites_long$variavel[i]
    mn <- limites_long$min_cal[i]
    mx <- limites_long$max_cal[i]
    rg <- limites_long$amplitude_cal[i]
    
    if (!v %in% names(df)) {
      warning("Variável ausente no cenário futuro: ", v)
      out[[v]] <- rep(NA_real_, nrow(df))
      next
    }
    
    if (is.na(rg) || rg == 0) {
      out[[v]] <- rep(NA_real_, nrow(df))
      next
    }
    
    x <- df[[v]]
    
    sim <- case_when(
      x < mn ~ 100 * (x - mn) / rg,
      x > mx ~ 100 * (mx - x) / rg,
      TRUE ~ 100 * pmin(x - mn, mx - x) / rg
    )
    
    out[[v]] <- sim
  }
  
  sim_df <- as.data.frame(out)
  
  mess <- apply(
    sim_df,
    1,
    min,
    na.rm = TRUE
  )
  
  var_limitante <- names(sim_df)[
    apply(
      sim_df,
      1,
      which.min
    )
  ]
  
  bind_cols(
    df |>
      select(lon, lat),
    tibble(
      MESS = as.numeric(mess),
      extrapolacao = MESS < 0,
      variavel_limitante = var_limitante
    ),
    sim_df |>
      rename_with(
        ~ paste0("similaridade_", .x)
      )
  )
}

# ------------------------------------------------------------
# 9. Preparar elementos cartográficos
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
# 10. Calcular MESS por cenário
# ------------------------------------------------------------

mess_all <- list()
resumos <- list()
rasters_mess <- list()
rasters_extrap <- list()

for (ssp in cenarios) {
  
  message("Calculando MESS para ", ssp)
  
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
  
  for (v in vars) {
    df_fut[[v]] <- as.numeric(df_fut[[v]])
  }
  
  mess_df <- calcular_mess_df(
    df = df_fut,
    limites_long = limites_long
  ) |>
    mutate(
      cenario = ssp
    )
  
  write_csv(
    mess_df,
    paste0(
      "dados/unidade14/processados/mess_",
      ssp,
      "_",
      periodo,
      "_",
      modelo_gcm,
      ".csv"
    )
  )
  
  r_mess <- r_fut[[1]]
  terra::values(r_mess) <- NA_real_
  
  cel <- terra::cellFromXY(
    r_mess,
    as.matrix(mess_df[, c("lon", "lat")])
  )
  
  r_mess[cel] <- mess_df$MESS
  names(r_mess) <- paste0("MESS_", ssp)
  
  r_extrap <- terra::ifel(
    r_mess < 0,
    1,
    0
  )
  
  names(r_extrap) <- paste0("extrapolacao_", ssp)
  
  terra::writeRaster(
    r_mess,
    paste0(
      "resultados/unidade14/mess/mess_",
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
    r_extrap,
    paste0(
      "resultados/unidade14/extrapolacao/extrapolacao_mess_",
      ssp,
      "_",
      periodo,
      "_",
      modelo_gcm,
      ".tif"
    ),
    overwrite = TRUE
  )
  
  rasters_mess[[ssp]] <- r_mess
  rasters_extrap[[ssp]] <- r_extrap
  
  resumo_ssp <- mess_df |>
    summarise(
      cenario = ssp,
      periodo = periodo,
      gcm = modelo_gcm,
      n_pixels = n(),
      MESS_min = min(MESS, na.rm = TRUE),
      MESS_q25 = quantile(MESS, 0.25, na.rm = TRUE),
      MESS_mediana = median(MESS, na.rm = TRUE),
      MESS_media = mean(MESS, na.rm = TRUE),
      MESS_q75 = quantile(MESS, 0.75, na.rm = TRUE),
      MESS_max = max(MESS, na.rm = TRUE),
      MESS_sd = sd(MESS, na.rm = TRUE),
      n_extrapolacao = sum(extrapolacao, na.rm = TRUE),
      prop_extrapolacao = mean(extrapolacao, na.rm = TRUE)
    )
  
  resumos[[ssp]] <- resumo_ssp
  
  mess_all[[ssp]] <- mess_df |>
    select(
      lon,
      lat,
      MESS,
      extrapolacao,
      variavel_limitante,
      cenario
    )
}

mess_all <- bind_rows(mess_all)
resumos <- bind_rows(resumos)

write_csv(
  mess_all,
  "dados/unidade14/processados/mess_todos_cenarios.csv"
)

write_csv(
  resumos,
  "tabelas/unidade14/resumo_mess_cenarios.csv"
)

# ------------------------------------------------------------
# 11. Preparar mapas
# ------------------------------------------------------------

ref <- rasters_mess[[1]]

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

lim_mess <- max(
  abs(mess_all$MESS),
  na.rm = TRUE
)

# ------------------------------------------------------------
# 12. Mapa MESS
# ------------------------------------------------------------

g_mess <- ggplot() +
  geom_sf(
    data = bioma_plot,
    fill = "grey96",
    color = "grey35",
    linewidth = 0.30
  ) +
  geom_raster(
    data = mess_all,
    aes(
      x = lon,
      y = lat,
      fill = MESS
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
  scale_fill_gradient2(
    name = "MESS",
    low = "#B2182B",
    mid = "white",
    high = "#2166AC",
    midpoint = 0,
    limits = c(-lim_mess, lim_mess),
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
    title = expression("MESS futuro para " * italic("Dinizia excelsa")),
    subtitle = "Valores negativos indicam extrapolação ambiental em relação ao domínio de calibração",
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
  "figuras/unidade14/unidade14_mess_cenarios.png",
  plot = g_mess,
  width = 12,
  height = 9,
  dpi = 600
)

# ------------------------------------------------------------
# 13. Mapa binário de extrapolação
# ------------------------------------------------------------

extrap_df <- mess_all |>
  mutate(
    classe_extrapolacao = ifelse(
      extrapolacao,
      "Extrapolação",
      "Sem extrapolação"
    ),
    classe_extrapolacao = factor(
      classe_extrapolacao,
      levels = c("Sem extrapolação", "Extrapolação")
    )
  )

g_extrap <- ggplot() +
  geom_sf(
    data = bioma_plot,
    fill = "grey96",
    color = "grey35",
    linewidth = 0.30
  ) +
  geom_raster(
    data = extrap_df,
    aes(
      x = lon,
      y = lat,
      fill = classe_extrapolacao
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
    title = expression("Áreas de extrapolação ambiental para " * italic("Dinizia excelsa")),
    subtitle = "Células com MESS < 0 indicam combinações ambientais fora do intervalo calibrado",
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
  "figuras/unidade14/unidade14_extrapolacao_mess_cenarios.png",
  plot = g_extrap,
  width = 12,
  height = 9,
  dpi = 600
)

# ------------------------------------------------------------
# 14. Figura composta
# ------------------------------------------------------------

fig_composta <- g_mess / g_extrap +
  patchwork::plot_annotation(
    title = expression("Transferibilidade ambiental futura para " * italic("Dinizia excelsa")),
    subtitle = "MESS contínuo e máscara binária de extrapolação ambiental"
  ) &
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 10)
  )

ggsave(
  "figuras/unidade14/unidade14_mess_extrapolacao_patchwork.png",
  plot = fig_composta,
  width = 12,
  height = 16,
  dpi = 600
)

# ------------------------------------------------------------
# 15. Mensagem final
# ------------------------------------------------------------

message("MESS calculado para todos os cenários com sucesso.")
message("Cenários processados: ", paste(cenarios, collapse = ", "))

print(resumos)

