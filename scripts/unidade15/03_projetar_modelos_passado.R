source("scripts/_bootstrap.R")

# ============================================================
# Unidade 15 - Paleoclima e nicho climático passado
# Script 03: Projetar modelos para períodos paleoclimáticos
# ============================================================

pacotes <- c(
  "dplyr", "readr", "terra", "sf", "ggplot2",
  "ggspatial", "viridis", "mgcv", "ranger",
  "gbm", "maxnet", "tidyr", "patchwork"
)

require_packages(pacotes)
invisible(lapply(pacotes, library, character.only = TRUE))

sf::sf_use_s2(FALSE)
dir.create("dados/unidade15/processados", recursive = TRUE, showWarnings = FALSE)
dir.create("figuras/unidade15", recursive = TRUE, showWarnings = FALSE)
dir.create("tabelas/unidade15", recursive = TRUE, showWarnings = FALSE)
dir.create("resultados/unidade15/modelos_individuais", recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# 1. Configurações
# ------------------------------------------------------------

periodos <- tibble::tibble(
  periodo = c("lig", "lgm", "holoceno_medio"),
  periodo_legenda = c(
    "Último Interglacial",
    "Último Máximo Glacial",
    "Holoceno Médio"
  )
)

arquivo_bioma <- "dados/unidade01/brutos/amazon_biome_border.shp"
arquivo_m <- "dados/unidade05/processados/area_m_dinizia.gpkg"
arquivo_oc <- "dados/unidade05/processados/presencas_area_m_dinizia.csv"

# ------------------------------------------------------------
# 2. Arquivos dos modelos
# ------------------------------------------------------------

arquivos_obrigatorios <- c(
  "resultados/unidade06/modelo_glm_dinizia.rds",
  "resultados/unidade07/modelo_gam_dinizia.rds",
  "resultados/unidade08/modelo_rf_dinizia.rds",
  "resultados/unidade09/modelo_brt_dinizia.rds",
  "resultados/unidade10/modelo_maxent_dinizia.rds",
  "resultados/unidade06/parametros_padronizacao_glm.csv",
  "resultados/unidade07/parametros_padronizacao_gam.csv",
  "tabelas/unidade09/parametros_brt.csv",
  "resultados/unidade10/variaveis_maxent.csv",
  arquivo_bioma,
  arquivo_oc
)

faltantes <- arquivos_obrigatorios[!file.exists(arquivos_obrigatorios)]

if (length(faltantes) > 0) {
  stop("Arquivos obrigatórios faltantes: ", paste(faltantes, collapse = ", "))
}

# ------------------------------------------------------------
# 3. Carregar modelos e parâmetros
# ------------------------------------------------------------

modelo_glm <- readRDS("resultados/unidade06/modelo_glm_dinizia.rds")
modelo_gam <- readRDS("resultados/unidade07/modelo_gam_dinizia.rds")
modelo_rf <- readRDS("resultados/unidade08/modelo_rf_dinizia.rds")
modelo_brt <- readRDS("resultados/unidade09/modelo_brt_dinizia.rds")
modelo_maxent <- readRDS("resultados/unidade10/modelo_maxent_dinizia.rds")

param_glm <- readr::read_csv(
  "resultados/unidade06/parametros_padronizacao_glm.csv",
  show_col_types = FALSE
)

param_gam <- readr::read_csv(
  "resultados/unidade07/parametros_padronizacao_gam.csv",
  show_col_types = FALSE
)

param_brt <- readr::read_csv(
  "tabelas/unidade09/parametros_brt.csv",
  show_col_types = FALSE
)

melhor_iter_brt <- as.numeric(
  param_brt$valor[param_brt$parametro == "best.trees"]
)

vars <- readr::read_csv(
  "resultados/unidade10/variaveis_maxent.csv",
  show_col_types = FALSE
)$variavel

# ------------------------------------------------------------
# 4. Funções auxiliares
# ------------------------------------------------------------

padronizar_df <- function(df, parametros) {
  
  for (i in seq_len(nrow(parametros))) {
    
    v <- parametros$variavel[i]
    vz <- parametros$variavel_z[i]
    
    if (v %in% names(df)) {
      df[[vz]] <- (
        df[[v]] - parametros$media_treino[i]
      ) / parametros$sd_treino[i]
    }
  }
  
  df
}

criar_raster_pred <- function(r_ref, df, coluna, nome) {
  
  r <- r_ref[[1]]
  terra::values(r) <- NA_real_
  
  cel <- terra::cellFromXY(
    r,
    as.matrix(df[, c("lon", "lat")])
  )
  
  r[cel] <- as.numeric(df[[coluna]])
  names(r) <- nome
  
  r
}

# ------------------------------------------------------------
# 5. Projetar modelos por período
# ------------------------------------------------------------

resumos <- list()

for (i in seq_len(nrow(periodos))) {
  
  periodo <- periodos$periodo[i]
  periodo_legenda <- periodos$periodo_legenda[i]
  
  message("Projetando modelos para: ", periodo_legenda)
  
  arquivo_paleo <- paste0(
    "dados/unidade15/processados/variaveis_paleoclimaticas_",
    periodo,
    ".tif"
  )
  
  if (!file.exists(arquivo_paleo)) {
    stop("Camadas paleoclimáticas não encontradas para ", periodo)
  }
  
  amb_paleo <- terra::rast(arquivo_paleo)
  
  vars_disp <- vars[vars %in% names(amb_paleo)]
  
  if (length(vars_disp) != length(vars)) {
    stop(
      "Variáveis ausentes no raster paleoclimático de ",
      periodo,
      ": ",
      paste(setdiff(vars, vars_disp), collapse = ", ")
    )
  }
  
  amb_paleo <- amb_paleo[[vars]]
  
  pred_df <- as.data.frame(
    amb_paleo,
    xy = TRUE,
    na.rm = TRUE
  ) |>
    dplyr::rename(
      lon = x,
      lat = y
    )
  
  for (v in vars) {
    pred_df[[v]] <- as.numeric(pred_df[[v]])
  }
  
  pred_df_glm <- padronizar_df(pred_df, param_glm)
  
  pred_df$GLM <- as.numeric(
    predict(
      modelo_glm,
      newdata = pred_df_glm,
      type = "response"
    )
  )
  
  pred_df_gam <- padronizar_df(pred_df, param_gam)
  
  pred_df$GAM <- as.numeric(
    predict(
      modelo_gam,
      newdata = pred_df_gam,
      type = "response"
    )
  )
  
  pred_df$Random_Forest <- as.numeric(
    predict(
      modelo_rf,
      data = pred_df[, vars, drop = FALSE]
    )$predictions[, "presenca"]
  )
  
  pred_df$BRT <- as.numeric(
    predict(
      modelo_brt,
      newdata = pred_df[, vars, drop = FALSE],
      n.trees = melhor_iter_brt,
      type = "response"
    )
  )
  
  pred_df$Maxent <- as.numeric(
    predict(
      modelo_maxent,
      pred_df[, vars, drop = FALSE],
      type = "cloglog",
      clamp = FALSE
    )
  )
  
  readr::write_csv(
    pred_df,
    paste0(
      "dados/unidade15/processados/predicoes_modelos_passado_",
      periodo,
      ".csv"
    )
  )
  
  rasters <- list(
    GLM = criar_raster_pred(amb_paleo, pred_df, "GLM", "GLM"),
    GAM = criar_raster_pred(amb_paleo, pred_df, "GAM", "GAM"),
    Random_Forest = criar_raster_pred(amb_paleo, pred_df, "Random_Forest", "Random_Forest"),
    BRT = criar_raster_pred(amb_paleo, pred_df, "BRT", "BRT"),
    Maxent = criar_raster_pred(amb_paleo, pred_df, "Maxent", "Maxent")
  )
  
  stack_pred <- terra::rast(rasters)
  
  terra::writeRaster(
    stack_pred,
    paste0(
      "resultados/unidade15/modelos_individuais/predicoes_modelos_passado_",
      periodo,
      ".tif"
    ),
    overwrite = TRUE
  )
  
  resumo_periodo <- pred_df |>
    tidyr::pivot_longer(
      cols = c("GLM", "GAM", "Random_Forest", "BRT", "Maxent"),
      names_to = "modelo",
      values_to = "adequabilidade"
    ) |>
    dplyr::group_by(modelo) |>
    dplyr::summarise(
      periodo = periodo,
      periodo_legenda = periodo_legenda,
      media = mean(adequabilidade, na.rm = TRUE),
      mediana = median(adequabilidade, na.rm = TRUE),
      min = min(adequabilidade, na.rm = TRUE),
      max = max(adequabilidade, na.rm = TRUE),
      sd = sd(adequabilidade, na.rm = TRUE),
      .groups = "drop"
    )
  
  resumos[[periodo]] <- resumo_periodo
}

resumos <- dplyr::bind_rows(resumos)

readr::write_csv(
  resumos,
  "tabelas/unidade15/resumo_modelos_passado.csv"
)

# ------------------------------------------------------------
# 6. Figura integrada dos modelos paleoclimáticos
# ------------------------------------------------------------

df_plot <- lapply(seq_len(nrow(periodos)), function(i) {
  
  periodo <- periodos$periodo[i]
  periodo_legenda <- periodos$periodo_legenda[i]
  
  r <- terra::rast(
    paste0(
      "resultados/unidade15/modelos_individuais/predicoes_modelos_passado_",
      periodo,
      ".tif"
    )
  )
  
  df <- as.data.frame(
    r,
    xy = TRUE,
    na.rm = TRUE
  )
  
  names(df)[1:2] <- c("lon", "lat")
  
  df |>
    tidyr::pivot_longer(
      cols = -c(lon, lat),
      names_to = "modelo",
      values_to = "adequabilidade"
    ) |>
    dplyr::mutate(
      periodo = periodo,
      periodo_legenda = periodo_legenda
    )
}) |>
  dplyr::bind_rows()

bioma_raw <- sf::st_read(arquivo_bioma, quiet = TRUE)

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
  area_m_plot <- sf::st_read(arquivo_m, quiet = TRUE) |>
    sf::st_make_valid() |>
    sf::st_transform(4326)
}

oc <- readr::read_csv(arquivo_oc, show_col_types = FALSE)

oc_sf <- sf::st_as_sf(
  oc,
  coords = c("lon", "lat"),
  crs = 4326,
  remove = FALSE
)

g <- ggplot() +
  geom_sf(
    data = bioma_plot,
    fill = "grey96",
    color = "grey35",
    linewidth = 0.25
  ) +
  geom_raster(
    data = df_plot,
    aes(
      x = lon,
      y = lat,
      fill = adequabilidade
    )
  ) +
  {
    if (!is.null(area_m_plot)) {
      geom_sf(
        data = area_m_plot,
        fill = NA,
        color = "grey20",
        linewidth = 0.20,
        linetype = "dashed"
      )
    }
  } +
  geom_sf(
    data = oc_sf,
    color = "black",
    fill = "red",
    shape = 21,
    size = 0.35,
    stroke = 0.10,
    alpha = 0.55
  ) +
  scale_fill_viridis_c(
    name = "Adequabilidade",
    option = "viridis",
    limits = c(0, 1),
    na.value = NA
  ) +
  coord_sf(
    xlim = c(bbox_bioma["xmin"], bbox_bioma["xmax"]),
    ylim = c(bbox_bioma["ymin"], bbox_bioma["ymax"]),
    expand = FALSE
  ) +
  facet_grid(
    periodo_legenda ~ modelo
  ) +
  labs(
    title = expression("Projeções paleoclimáticas de " * italic("Dinizia excelsa")),
    subtitle = "Modelos individuais projetados para períodos climáticos do passado",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_bw(base_size = 9) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10),
    strip.text = element_text(face = "bold", size = 8),
    panel.grid.major = element_line(color = "grey88", linewidth = 0.15),
    panel.grid.minor = element_blank()
  )

ggsave(
  "figuras/unidade15/unidade15_modelos_passado_integrado.png",
  plot = g,
  width = 15,
  height = 10,
  dpi = 600
)

# ------------------------------------------------------------
# 7. Figura específica do LGM
# ------------------------------------------------------------

df_lgm <- df_plot |>
  dplyr::filter(periodo == "lgm")

g_lgm <- ggplot() +
  geom_sf(
    data = bioma_plot,
    fill = "grey96",
    color = "grey35",
    linewidth = 0.30
  ) +
  geom_raster(
    data = df_lgm,
    aes(x = lon, y = lat, fill = adequabilidade)
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
    na.value = NA
  ) +
  coord_sf(
    xlim = c(bbox_bioma["xmin"], bbox_bioma["xmax"]),
    ylim = c(bbox_bioma["ymin"], bbox_bioma["ymax"]),
    expand = FALSE
  ) +
  facet_wrap(~ modelo, ncol = 3) +
  ggspatial::annotation_scale(
    location = "bl",
    width_hint = 0.22,
    text_cex = 0.55
  ) +
  labs(
    title = expression("Projeções para o Último Máximo Glacial de " * italic("Dinizia excelsa")),
    subtitle = "Modelos individuais projetados para o LGM",
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
  "figuras/unidade15/unidade15_modelos_passado_lgm.png",
  plot = g_lgm,
  width = 12,
  height = 8,
  dpi = 600
)

message("Projeções paleoclimáticas dos modelos concluídas com sucesso.")
print(resumos)
