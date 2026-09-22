source("scripts/_bootstrap.R")

# ============================================================
# Unidade 13 - Projeções climáticas futuras em SDM
# Script 04: Ensemble futuro por cenário
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
dir.create("resultados/unidade13/ensemble", recursive = TRUE, showWarnings = FALSE)

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
# 5. Preparar elementos cartográficos
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
# 6. Gerar ensembles futuros
# ------------------------------------------------------------

df_ensemble_all <- list()
df_incerteza_all <- list()
resumos <- list()
stacks_ensemble <- list()

for (ssp in cenarios) {
  
  message("Gerando ensemble futuro para: ", ssp)
  
  arquivo_stack <- paste0(
    "resultados/unidade13/modelos_individuais/predicoes_modelos_",
    ssp,
    "_",
    periodo,
    "_",
    modelo_gcm,
    ".tif"
  )
  
  if (!file.exists(arquivo_stack)) {
    stop(
      "Predições individuais não encontradas para ",
      ssp,
      ". Execute o Script 03 da Unidade 13."
    )
  }
  
  stack_modelos <- terra::rast(
    arquivo_stack
  )
  
  if (terra::nlyr(stack_modelos) < 2) {
    stop("O stack de modelos futuros para ", ssp, " possui menos de duas camadas.")
  }
  
  nomes_modelos <- names(stack_modelos)
  
  ensemble_media <- terra::app(
    stack_modelos,
    fun = mean,
    na.rm = TRUE
  )
  
  ensemble_media <- terra::clamp(
    ensemble_media,
    lower = 0,
    upper = 1,
    values = TRUE
  )
  
  names(ensemble_media) <- paste0("ensemble_", ssp)
  
  incerteza_sd <- terra::app(
    stack_modelos,
    fun = sd,
    na.rm = TRUE
  )
  
  names(incerteza_sd) <- paste0("incerteza_", ssp)
  
  amplitude <- terra::app(
    stack_modelos,
    fun = function(x) {
      max(x, na.rm = TRUE) - min(x, na.rm = TRUE)
    }
  )
  
  names(amplitude) <- paste0("amplitude_", ssp)
  
  # Salvar rasters
  terra::writeRaster(
    ensemble_media,
    paste0(
      "resultados/unidade13/ensemble/ensemble_futuro_",
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
    incerteza_sd,
    paste0(
      "resultados/unidade13/ensemble/incerteza_futura_",
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
    amplitude,
    paste0(
      "resultados/unidade13/ensemble/amplitude_futura_",
      ssp,
      "_",
      periodo,
      "_",
      modelo_gcm,
      ".tif"
    ),
    overwrite = TRUE
  )
  
  stack_produtos <- c(
    ensemble_media,
    incerteza_sd,
    amplitude
  )
  
  names(stack_produtos) <- c(
    "ensemble_media",
    "incerteza_sd",
    "amplitude"
  )
  
  terra::writeRaster(
    stack_produtos,
    paste0(
      "resultados/unidade13/ensemble/produtos_ensemble_futuro_",
      ssp,
      "_",
      periodo,
      "_",
      modelo_gcm,
      ".tif"
    ),
    overwrite = TRUE
  )
  
  stacks_ensemble[[ssp]] <- stack_produtos
  
  # Data frames
  df_ens <- as.data.frame(
    ensemble_media,
    xy = TRUE,
    na.rm = TRUE
  ) |>
    rename(
      lon = x,
      lat = y,
      adequabilidade = all_of(names(ensemble_media))
    ) |>
    mutate(
      cenario = ssp
    )
  
  df_unc <- as.data.frame(
    incerteza_sd,
    xy = TRUE,
    na.rm = TRUE
  ) |>
    rename(
      lon = x,
      lat = y,
      incerteza = all_of(names(incerteza_sd))
    ) |>
    mutate(
      cenario = ssp
    )
  
  df_ensemble_all[[ssp]] <- df_ens
  df_incerteza_all[[ssp]] <- df_unc
  
  write_csv(
    df_ens,
    paste0(
      "dados/unidade13/processados/ensemble_futuro_",
      ssp,
      "_",
      periodo,
      "_",
      modelo_gcm,
      ".csv"
    )
  )
  
  write_csv(
    df_unc,
    paste0(
      "dados/unidade13/processados/incerteza_futura_",
      ssp,
      "_",
      periodo,
      "_",
      modelo_gcm,
      ".csv"
    )
  )
  
  resumo_ssp <- bind_rows(
    tibble(
      produto = "ensemble_media",
      valores = list(df_ens$adequabilidade)
    ),
    tibble(
      produto = "incerteza_sd",
      valores = list(df_unc$incerteza)
    )
  ) |>
    rowwise() |>
    mutate(
      cenario = ssp,
      periodo = periodo,
      gcm = modelo_gcm,
      modelos_incluidos = paste(nomes_modelos, collapse = ", "),
      n_modelos = length(nomes_modelos),
      n_pixels = sum(!is.na(valores)),
      minimo = min(valores, na.rm = TRUE),
      primeiro_quartil = quantile(valores, 0.25, na.rm = TRUE),
      mediana = median(valores, na.rm = TRUE),
      media = mean(valores, na.rm = TRUE),
      terceiro_quartil = quantile(valores, 0.75, na.rm = TRUE),
      maximo = max(valores, na.rm = TRUE),
      desvio_padrao = sd(valores, na.rm = TRUE)
    ) |>
    ungroup() |>
    select(-valores)
  
  resumos[[ssp]] <- resumo_ssp
}

df_ensemble_all <- bind_rows(df_ensemble_all)
df_incerteza_all <- bind_rows(df_incerteza_all)
resumos <- bind_rows(resumos)

write_csv(
  df_ensemble_all,
  "dados/unidade13/processados/ensemble_futuro_todos_cenarios.csv"
)

write_csv(
  df_incerteza_all,
  "dados/unidade13/processados/incerteza_futura_todos_cenarios.csv"
)

write_csv(
  resumos,
  "tabelas/unidade13/resumo_ensemble_futuro_cenarios.csv"
)

# ------------------------------------------------------------
# 7. Preparar bioma, área M e ocorrências para mapas
# ------------------------------------------------------------

ref <- stacks_ensemble[[1]][[1]]

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
# 8. Função base para mapas
# ------------------------------------------------------------

mapa_futuro <- function(df, coluna, titulo, subtitulo, legenda, escala = "adequabilidade") {
  
  paleta <- if (escala == "adequabilidade") "viridis" else "magma"
  
  ggplot() +
    geom_sf(
      data = bioma_plot,
      fill = "grey96",
      color = "grey35",
      linewidth = 0.30
    ) +
    geom_raster(
      data = df,
      aes(
        x = lon,
        y = lat,
        fill = .data[[coluna]]
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
      name = legenda,
      option = paleta,
      limits = if (escala == "adequabilidade") c(0, 1) else NULL,
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
      title = titulo,
      subtitle = subtitulo,
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
}

# ------------------------------------------------------------
# 9. Mapas de ensemble e incerteza
# ------------------------------------------------------------

g_ensemble <- mapa_futuro(
  df = df_ensemble_all,
  coluna = "adequabilidade",
  titulo = expression("Ensemble futuro para " * italic("Dinizia excelsa")),
  subtitulo = paste0(
    "Média das predições futuras dos modelos | ",
    modelo_gcm,
    " | ",
    periodo
  ),
  legenda = "Adequabilidade",
  escala = "adequabilidade"
)

ggsave(
  "figuras/unidade13/unidade13_ensemble_futuro_cenarios.png",
  plot = g_ensemble,
  width = 12,
  height = 9,
  dpi = 600
)

g_incerteza <- mapa_futuro(
  df = df_incerteza_all,
  coluna = "incerteza",
  titulo = expression("Incerteza futura entre modelos para " * italic("Dinizia excelsa")),
  subtitulo = paste0(
    "Desvio-padrão das predições futuras | ",
    modelo_gcm,
    " | ",
    periodo
  ),
  legenda = "DP",
  escala = "incerteza"
)

ggsave(
  "figuras/unidade13/unidade13_incerteza_futura_cenarios.png",
  plot = g_incerteza,
  width = 12,
  height = 9,
  dpi = 600
)

# Figura composta
fig_composta <- g_ensemble / g_incerteza +
  patchwork::plot_annotation(
    title = expression("Síntese futura de ensemble SDM para " * italic("Dinizia excelsa")),
    subtitle = "Adequabilidade média e incerteza algorítmica por cenário SSP"
  ) &
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 10)
  )

ggsave(
  "figuras/unidade13/unidade13_ensemble_incerteza_futuro_patchwork.png",
  plot = fig_composta,
  width = 12,
  height = 16,
  dpi = 600
)

# ------------------------------------------------------------
# 10. Mensagem final
# ------------------------------------------------------------

message("Ensembles futuros gerados com sucesso.")
message("Cenários processados: ", paste(cenarios, collapse = ", "))
message("Período: ", periodo)
message("GCM: ", modelo_gcm)

print(resumos)
