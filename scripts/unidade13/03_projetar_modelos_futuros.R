source("scripts/_bootstrap.R")

# ============================================================
# Unidade 13 - Projeções climáticas futuras em SDM
# Script 03: Projetar modelos individuais para cenários futuros
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
  "tibble",
  "patchwork",
  "mgcv",
  "ranger",
  "gbm",
  "maxnet"
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
dir.create("resultados/unidade13/modelos_individuais", recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# 4. Configurações
# ------------------------------------------------------------

cenarios <- c("ssp126", "ssp245", "ssp370", "ssp585")

periodo <- "2061-2080"

modelo_gcm <- "MIROC6"

# ------------------------------------------------------------
# 5. Arquivos de entrada
# ------------------------------------------------------------

arquivos_modelos <- c(
  glm = "resultados/unidade06/modelo_glm_dinizia.rds",
  gam = "resultados/unidade07/modelo_gam_dinizia.rds",
  rf = "resultados/unidade08/modelo_rf_dinizia.rds",
  brt = "resultados/unidade09/modelo_brt_dinizia.rds",
  maxent = "resultados/unidade10/modelo_maxent_dinizia.rds"
)

arquivo_param_glm <- "resultados/unidade06/parametros_padronizacao_glm.csv"
arquivo_param_gam <- "resultados/unidade07/parametros_padronizacao_gam.csv"
arquivo_param_brt <- "tabelas/unidade09/parametros_brt.csv"
arquivo_vars <- "dados/unidade04/processados/variaveis_selecionadas_final.csv"

arquivo_bioma <- "dados/unidade01/brutos/amazon_biome_border.shp"
arquivo_m <- "dados/unidade05/processados/area_m_dinizia.gpkg"
arquivo_oc <- "dados/unidade05/processados/presencas_area_m_dinizia.csv"

faltantes <- c(
  arquivos_modelos,
  arquivo_param_glm,
  arquivo_param_gam,
  arquivo_param_brt,
  arquivo_vars,
  arquivo_bioma,
  arquivo_oc
)

faltantes <- faltantes[!file.exists(faltantes)]

if (length(faltantes) > 0) {
  stop(
    "Arquivos obrigatórios ausentes:\n",
    paste(faltantes, collapse = "\n")
  )
}

if (!file.exists(arquivo_m)) {
  warning("Área M não encontrada. Os mapas serão gerados sem contorno da área M.")
}

# ------------------------------------------------------------
# 6. Carregar modelos e parâmetros
# ------------------------------------------------------------

modelo_glm <- readRDS(arquivos_modelos["glm"])
modelo_gam <- readRDS(arquivos_modelos["gam"])
modelo_rf <- readRDS(arquivos_modelos["rf"])
modelo_brt <- readRDS(arquivos_modelos["brt"])
modelo_maxent <- readRDS(arquivos_modelos["maxent"])

param_glm <- read_csv(
  arquivo_param_glm,
  show_col_types = FALSE
)

param_gam <- read_csv(
  arquivo_param_gam,
  show_col_types = FALSE
)

param_brt <- read_csv(
  arquivo_param_brt,
  show_col_types = FALSE
)

melhor_iter_brt <- as.numeric(
  param_brt$valor[param_brt$parametro == "best.trees"]
)

if (length(melhor_iter_brt) == 0 || is.na(melhor_iter_brt)) {
  stop("Número ótimo de árvores do BRT não encontrado em parametros_brt.csv.")
}

vars <- read_csv(
  arquivo_vars,
  show_col_types = FALSE
)$variavel |>
  unique()

if (length(vars) < 2) {
  stop("Número insuficiente de variáveis selecionadas.")
}

# ------------------------------------------------------------
# 7. Bioma, área M e ocorrências para mapas
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
  stop("O arquivo de ocorrências precisa conter as colunas 'lon' e 'lat'.")
}

# ------------------------------------------------------------
# 8. Funções auxiliares
# ------------------------------------------------------------

padronizar_df <- function(df, parametros) {
  
  for (i in seq_len(nrow(parametros))) {
    
    v <- parametros$variavel[i]
    vz <- parametros$variavel_z[i]
    
    if (v %in% names(df)) {
      
      df[[vz]] <- (
        df[[v]] - parametros$media_treino[i]
      ) / parametros$sd_treino[i]
      
    } else {
      
      warning("Variável ausente para padronização: ", v)
      
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
  
  terra::clamp(
    r,
    lower = 0,
    upper = 1,
    values = TRUE
  )
}

mapa_modelos_futuros <- function(r_stack, ssp, bioma_plot, area_m_plot, oc_sf, bbox_bioma) {
  
  df_plot <- as.data.frame(
    r_stack,
    xy = TRUE,
    na.rm = TRUE
  )
  
  names(df_plot)[1:2] <- c("lon", "lat")
  
  df_long <- df_plot |>
    pivot_longer(
      cols = -c(lon, lat),
      names_to = "modelo",
      values_to = "adequabilidade"
    )
  
  ggplot() +
    geom_sf(
      data = bioma_plot,
      fill = "grey96",
      color = "grey35",
      linewidth = 0.30
    ) +
    geom_raster(
      data = df_long,
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
      size = 0.60,
      stroke = 0.15,
      alpha = 0.70
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
    facet_wrap(
      ~ modelo,
      ncol = 3
    ) +
    ggspatial::annotation_scale(
      location = "bl",
      width_hint = 0.22,
      text_cex = 0.55
    ) +
    labs(
      title = expression("Projeções futuras de " * italic("Dinizia excelsa")),
      subtitle = paste0(
        toupper(ssp),
        " | ",
        modelo_gcm,
        " | ",
        periodo
      ),
      x = "Longitude",
      y = "Latitude"
    ) +
    theme_bw(base_size = 9) +
    theme(
      plot.title = element_text(face = "bold", size = 13),
      plot.subtitle = element_text(size = 10),
      strip.text = element_text(face = "bold", size = 8.5),
      panel.grid.major = element_line(color = "grey88", linewidth = 0.20),
      panel.grid.minor = element_blank(),
      legend.position = "right"
    )
}

# ------------------------------------------------------------
# 9. Projetar modelos por cenário
# ------------------------------------------------------------

resumos <- list()
stacks_por_cenario <- list()

for (ssp in cenarios) {
  
  message("Projetando modelos para: ", ssp)
  
  arquivo_futuro <- paste0(
    "dados/unidade13/processados/variaveis_futuras_",
    ssp,
    "_",
    periodo,
    "_",
    modelo_gcm,
    ".tif"
  )
  
  if (!file.exists(arquivo_futuro)) {
    stop(
      "Variáveis futuras não encontradas para ",
      ssp,
      ". Execute o Script 02 da Unidade 13."
    )
  }
  
  amb_fut <- terra::rast(
    arquivo_futuro
  )
  
  vars_disponiveis <- vars[vars %in% names(amb_fut)]
  
  if (length(vars_disponiveis) != length(vars)) {
    stop(
      "Nem todas as variáveis selecionadas estão disponíveis no raster futuro de ",
      ssp,
      ". Ausentes: ",
      paste(setdiff(vars, vars_disponiveis), collapse = ", ")
    )
  }
  
  amb_fut <- amb_fut[[vars]]
  
  pred_df <- as.data.frame(
    amb_fut,
    xy = TRUE,
    na.rm = TRUE
  ) |>
    rename(
      lon = x,
      lat = y
    )
  
  for (v in vars) {
    pred_df[[v]] <- as.numeric(pred_df[[v]])
  }
  
  # GLM
  pred_df_glm <- padronizar_df(
    pred_df,
    param_glm
  )
  
  pred_df$GLM <- as.numeric(
    predict(
      modelo_glm,
      newdata = pred_df_glm,
      type = "response"
    )
  )
  
  # GAM
  pred_df_gam <- padronizar_df(
    pred_df,
    param_gam
  )
  
  pred_df$GAM <- as.numeric(
    predict(
      modelo_gam,
      newdata = pred_df_gam,
      type = "response"
    )
  )
  
  # Random Forest
  pred_df$RF <- as.numeric(
    predict(
      modelo_rf,
      data = pred_df[, vars, drop = FALSE]
    )$predictions[, "presenca"]
  )
  
  # BRT
  pred_df$BRT <- as.numeric(
    predict(
      modelo_brt,
      newdata = pred_df[, vars, drop = FALSE],
      n.trees = melhor_iter_brt,
      type = "response"
    )
  )
  
  # MaxEnt
  pred_df$MaxEnt <- as.numeric(
    predict(
      modelo_maxent,
      newdata = pred_df[, vars, drop = FALSE],
      type = "cloglog",
      clamp = FALSE
    )
  )
  
  # Garantir intervalo 0-1
  modelos_pred <- c("GLM", "GAM", "RF", "BRT", "MaxEnt")
  
  pred_df <- pred_df |>
    mutate(
      across(
        all_of(modelos_pred),
        ~ pmin(pmax(as.numeric(.x), 0), 1)
      )
    )
  
  # Salvar tabela
  arquivo_csv <- paste0(
    "dados/unidade13/processados/predicoes_futuras_modelos_",
    ssp,
    "_",
    periodo,
    "_",
    modelo_gcm,
    ".csv"
  )
  
  write_csv(
    pred_df,
    arquivo_csv
  )
  
  # Criar rasters individuais
  rasters <- list(
    GLM = criar_raster_pred(amb_fut, pred_df, "GLM", "GLM"),
    GAM = criar_raster_pred(amb_fut, pred_df, "GAM", "GAM"),
    RF = criar_raster_pred(amb_fut, pred_df, "RF", "RF"),
    BRT = criar_raster_pred(amb_fut, pred_df, "BRT", "BRT"),
    MaxEnt = criar_raster_pred(amb_fut, pred_df, "MaxEnt", "MaxEnt")
  )
  
  stack_pred <- terra::rast(
    rasters
  )
  
  names(stack_pred) <- modelos_pred
  
  arquivo_stack <- paste0(
    "resultados/unidade13/modelos_individuais/predicoes_modelos_",
    ssp,
    "_",
    periodo,
    "_",
    modelo_gcm,
    ".tif"
  )
  
  terra::writeRaster(
    stack_pred,
    arquivo_stack,
    overwrite = TRUE
  )
  
  stacks_por_cenario[[ssp]] <- stack_pred
  
  resumo_ssp <- pred_df |>
    select(
      all_of(modelos_pred)
    ) |>
    pivot_longer(
      cols = everything(),
      names_to = "modelo",
      values_to = "adequabilidade"
    ) |>
    group_by(modelo) |>
    summarise(
      cenario = ssp,
      periodo = periodo,
      gcm = modelo_gcm,
      n_pixels = sum(!is.na(adequabilidade)),
      minimo = min(adequabilidade, na.rm = TRUE),
      primeiro_quartil = quantile(adequabilidade, 0.25, na.rm = TRUE),
      mediana = median(adequabilidade, na.rm = TRUE),
      media = mean(adequabilidade, na.rm = TRUE),
      terceiro_quartil = quantile(adequabilidade, 0.75, na.rm = TRUE),
      maximo = max(adequabilidade, na.rm = TRUE),
      desvio_padrao = sd(adequabilidade, na.rm = TRUE),
      .groups = "drop"
    )
  
  resumos[[ssp]] <- resumo_ssp
}

resumos <- bind_rows(
  resumos
)

write_csv(
  resumos,
  "tabelas/unidade13/resumo_predicoes_futuras_modelos.csv"
)

# ------------------------------------------------------------
# 10. Preparar elementos cartográficos
# ------------------------------------------------------------

ref <- stacks_por_cenario[[1]]

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
# 11. Mapas por cenário
# ------------------------------------------------------------

for (ssp in cenarios) {
  
  g_ssp <- mapa_modelos_futuros(
    r_stack = stacks_por_cenario[[ssp]],
    ssp = ssp,
    bioma_plot = bioma_plot,
    area_m_plot = area_m_plot,
    oc_sf = oc_sf,
    bbox_bioma = bbox_bioma
  )
  
  ggsave(
    paste0(
      "figuras/unidade13/unidade13_modelos_futuros_",
      ssp,
      ".png"
    ),
    plot = g_ssp,
    width = 13,
    height = 8,
    dpi = 600
  )
}

# ------------------------------------------------------------
# 12. Figura integrada dos cenários para Ensemble visual simples
# ------------------------------------------------------------

df_integrado <- lapply(names(stacks_por_cenario), function(ssp) {
  
  r <- stacks_por_cenario[[ssp]]
  
  media_modelos <- terra::app(
    r,
    fun = mean,
    na.rm = TRUE
  )
  
  names(media_modelos) <- "media_modelos"
  
  as.data.frame(
    media_modelos,
    xy = TRUE,
    na.rm = TRUE
  ) |>
    rename(
      lon = x,
      lat = y,
      adequabilidade = media_modelos
    ) |>
    mutate(
      cenario = ssp
    )
}) |>
  bind_rows()

write_csv(
  df_integrado,
  "dados/unidade13/processados/media_modelos_futuros_por_cenario.csv"
)

g_integrado <- ggplot() +
  geom_sf(
    data = bioma_plot,
    fill = "grey96",
    color = "grey35",
    linewidth = 0.30
  ) +
  geom_raster(
    data = df_integrado,
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
  facet_wrap(
    ~ cenario,
    ncol = 2
  ) +
  labs(
    title = expression("Adequabilidade futura média entre modelos para " * italic("Dinizia excelsa")),
    subtitle = paste0(
      modelo_gcm,
      " | ",
      periodo,
      " | média de GLM, GAM, RF, BRT e MaxEnt"
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
  "figuras/unidade13/unidade13_media_modelos_futuros_cenarios.png",
  plot = g_integrado,
  width = 12,
  height = 9,
  dpi = 600
)

# ------------------------------------------------------------
# 13. Mensagem final
# ------------------------------------------------------------

message("Projeções futuras dos modelos individuais concluídas com sucesso.")
message("Cenários processados: ", paste(cenarios, collapse = ", "))
message("Período: ", periodo)
message("GCM: ", modelo_gcm)

print(resumos)
