source("scripts/_bootstrap.R")

# ============================================================
# Unidade 11 - Modelagem Ensemble em SDM
# Script 07: Comparação integrada
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
  "patchwork",
  "tibble",
  "tidyr"
)

require_packages(pacotes)
invisible(lapply(pacotes, library, character.only = TRUE))

sf::sf_use_s2(FALSE)

# ------------------------------------------------------------
# 2. Diretório raiz do projeto
# ------------------------------------------------------------
# ------------------------------------------------------------
# 3. Diretórios de saída
# ------------------------------------------------------------

dir.create("figuras/unidade11", recursive = TRUE, showWarnings = FALSE)
dir.create("tabelas/unidade11", recursive = TRUE, showWarnings = FALSE)
dir.create("dados/unidade11/processados", recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# 4. Arquivos de entrada
# ------------------------------------------------------------

arquivos_modelos <- c(
  GLM = "resultados/unidade06/adequabilidade_glm_dinizia.tif",
  GAM = "resultados/unidade07/adequabilidade_gam_dinizia.tif",
  RF = "resultados/unidade08/adequabilidade_rf_dinizia.tif",
  BRT = "resultados/unidade09/adequabilidade_brt_dinizia.tif",
  MaxEnt = "resultados/unidade10/adequabilidade_maxent_dinizia.tif"
)

arquivos_ensemble <- c(
  Ensemble_Media = "resultados/unidade11/ensemble_media_simples_dinizia.tif",
  Ensemble_Ponderado = "resultados/unidade11/ensemble_ponderado_dinizia.tif",
  Consenso_Binario = "resultados/unidade11/consenso_binario_dinizia.tif"
)

arquivos_incerteza <- c(
  Incerteza_SD = "resultados/unidade11/incerteza_algoritmica_dinizia.tif",
  Amplitude = "resultados/unidade11/amplitude_algoritmica_dinizia.tif"
)

arquivo_bioma <- "dados/unidade01/brutos/amazon_biome_border.shp"
arquivo_m <- "dados/unidade05/processados/area_m_dinizia.gpkg"
arquivo_oc <- "dados/unidade05/processados/presencas_area_m_dinizia.csv"

todos_arquivos <- c(
  arquivos_modelos,
  arquivos_ensemble,
  arquivos_incerteza
)

faltantes <- todos_arquivos[!file.exists(todos_arquivos)]

if (length(faltantes) > 0) {
  stop(
    "Arquivos faltantes:\n",
    paste(names(faltantes), faltantes, sep = " = ", collapse = "\n")
  )
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
# 5. Ler rasters e alinhar
# ------------------------------------------------------------

rasters <- lapply(
  todos_arquivos,
  terra::rast
)

ref <- rasters[[1]]

rasters_alinhados <- lapply(
  names(rasters),
  function(nm) {
    
    r <- rasters[[nm]]
    
    if (!terra::compareGeom(ref, r, stopOnError = FALSE)) {
      message("Reamostrando raster: ", nm)
      r <- terra::resample(
        r,
        ref,
        method = "bilinear"
      )
    }
    
    names(r) <- nm
    r
  }
)

names(rasters_alinhados) <- names(todos_arquivos)

# ------------------------------------------------------------
# 6. Preparar bioma, área M e ocorrências
# ------------------------------------------------------------

bioma_raw <- sf::st_read(
  arquivo_bioma,
  quiet = TRUE
)

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

bbox_bioma <- sf::st_bbox(bioma_plot)

area_m_plot <- NULL

if (file.exists(arquivo_m)) {
  area_m <- sf::st_read(
    arquivo_m,
    quiet = TRUE
  )
  
  area_m_plot <- area_m |>
    sf::st_make_valid() |>
    sf::st_transform(4326)
}

oc <- read_csv(
  arquivo_oc,
  show_col_types = FALSE
)

if (!all(c("lon", "lat") %in% names(oc))) {
  stop("O arquivo de ocorrências precisa conter as colunas 'lon' e 'lat'.")
}

oc_sf <- sf::st_as_sf(
  oc,
  coords = c("lon", "lat"),
  crs = 4326,
  remove = FALSE
)

# ------------------------------------------------------------
# 7. Funções auxiliares
# ------------------------------------------------------------

raster_para_df <- function(r, nome_valor) {
  
  as.data.frame(
    r,
    xy = TRUE,
    na.rm = TRUE
  ) |>
    rename(
      lon = x,
      lat = y,
      valor = all_of(nome_valor)
    )
}

mapa_base <- function(
    df,
    titulo,
    subtitulo = NULL,
    legenda = "Adequabilidade",
    escala = c("adequabilidade", "incerteza")
) {
  
  escala <- match.arg(escala)
  
  escala_fill <- if (escala == "adequabilidade") {
    scale_fill_viridis_c(
      name = legenda,
      option = "viridis",
      limits = c(0, 1),
      na.value = NA
    )
  } else {
    scale_fill_viridis_c(
      name = legenda,
      option = "magma",
      na.value = NA
    )
  }
  
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
        fill = valor
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
      size = 0.65,
      stroke = 0.16,
      alpha = 0.70
    ) +
    escala_fill +
    coord_sf(
      xlim = c(bbox_bioma["xmin"], bbox_bioma["xmax"]),
      ylim = c(bbox_bioma["ymin"], bbox_bioma["ymax"]),
      expand = FALSE
    ) +
    labs(
      title = titulo,
      subtitle = subtitulo,
      x = "Longitude",
      y = "Latitude"
    ) +
    theme_bw(base_size = 10) +
    theme(
      plot.title = element_text(face = "bold", size = 11),
      plot.subtitle = element_text(size = 8.5),
      legend.position = "right",
      panel.grid.major = element_line(color = "grey88", linewidth = 0.20),
      panel.grid.minor = element_blank()
    )
}

# ------------------------------------------------------------
# 8. Mapas dos modelos individuais
# ------------------------------------------------------------

p_glm <- mapa_base(
  raster_para_df(rasters_alinhados[["GLM"]], "GLM"),
  "GLM"
)

p_gam <- mapa_base(
  raster_para_df(rasters_alinhados[["GAM"]], "GAM"),
  "GAM"
)

p_rf <- mapa_base(
  raster_para_df(rasters_alinhados[["RF"]], "RF"),
  "Random Forest"
)

p_brt <- mapa_base(
  raster_para_df(rasters_alinhados[["BRT"]], "BRT"),
  "BRT"
)

p_maxent <- mapa_base(
  raster_para_df(rasters_alinhados[["MaxEnt"]], "MaxEnt"),
  "MaxEnt"
)

fig_modelos <- (p_glm + p_gam + p_rf) / (p_brt + p_maxent + patchwork::plot_spacer()) +
  patchwork::plot_layout(guides = "collect") +
  patchwork::plot_annotation(
    title = expression("Modelos individuais para " * italic("Dinizia excelsa")),
    subtitle = "Predições de adequabilidade ambiental calibradas com as mesmas variáveis ambientais"
  ) &
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 10),
    legend.position = "right"
  )

ggsave(
  "figuras/unidade11/unidade11_modelos_individuais.png",
  plot = fig_modelos,
  width = 15,
  height = 10,
  dpi = 600
)

# ------------------------------------------------------------
# 9. Mapas dos produtos ensemble
# ------------------------------------------------------------

p_media <- mapa_base(
  raster_para_df(rasters_alinhados[["Ensemble_Media"]], "Ensemble_Media"),
  "Ensemble média simples"
)

p_pond <- mapa_base(
  raster_para_df(rasters_alinhados[["Ensemble_Ponderado"]], "Ensemble_Ponderado"),
  "Ensemble ponderado"
)

p_consenso <- mapa_base(
  raster_para_df(rasters_alinhados[["Consenso_Binario"]], "Consenso_Binario"),
  "Consenso binário",
  subtitulo = "Proporção de modelos classificando a célula como adequada",
  legenda = "Consenso"
)

fig_ensembles <- (p_media + p_pond + p_consenso) +
  patchwork::plot_layout(guides = "collect") +
  patchwork::plot_annotation(
    title = expression("Produtos ensemble para " * italic("Dinizia excelsa")),
    subtitle = "Média simples, média ponderada por desempenho e consenso binário"
  ) &
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 10),
    legend.position = "right"
  )

ggsave(
  "figuras/unidade11/unidade11_produtos_ensemble.png",
  plot = fig_ensembles,
  width = 15,
  height = 5.5,
  dpi = 600
)

# ------------------------------------------------------------
# 10. Mapas de incerteza
# ------------------------------------------------------------

p_sd <- mapa_base(
  raster_para_df(rasters_alinhados[["Incerteza_SD"]], "Incerteza_SD"),
  "Incerteza algorítmica",
  subtitulo = "Desvio-padrão das predições entre modelos",
  legenda = "DP",
  escala = "incerteza"
)

p_amp <- mapa_base(
  raster_para_df(rasters_alinhados[["Amplitude"]], "Amplitude"),
  "Amplitude entre modelos",
  subtitulo = "Diferença entre a maior e a menor adequabilidade predita",
  legenda = "Amplitude",
  escala = "incerteza"
)

fig_incerteza <- p_sd + p_amp +
  patchwork::plot_annotation(
    title = expression("Incerteza em ensemble SDM para " * italic("Dinizia excelsa")),
    subtitle = "Desvio-padrão e amplitude das predições entre algoritmos"
  ) &
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 10)
  )

ggsave(
  "figuras/unidade11/unidade11_produtos_incerteza.png",
  plot = fig_incerteza,
  width = 14,
  height = 7,
  dpi = 600
)

# ------------------------------------------------------------
# 11. Síntese integrada
# ------------------------------------------------------------

fig_sintese <- (p_media + p_pond) / (p_consenso + p_sd) +
  patchwork::plot_layout(guides = "collect") +
  patchwork::plot_annotation(
    title = expression("Síntese integrada do ensemble para " * italic("Dinizia excelsa")),
    subtitle = "Adequabilidade média, ensemble ponderado, consenso binário e incerteza algorítmica"
  ) &
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 10),
    legend.position = "right"
  )

ggsave(
  "figuras/unidade11/unidade11_comparacao_ensemble.png",
  plot = fig_sintese,
  width = 14,
  height = 10,
  dpi = 600
)

# ------------------------------------------------------------
# 12. Exportar tabela integrada pixel a pixel
# ------------------------------------------------------------

stack_integrado <- terra::rast(rasters_alinhados)

names(stack_integrado) <- names(todos_arquivos)

df_integrado <- as.data.frame(
  stack_integrado,
  xy = TRUE,
  na.rm = TRUE
) |>
  rename(
    lon = x,
    lat = y
  )

write_csv(
  df_integrado,
  "dados/unidade11/processados/comparacao_integrada_ensemble.csv"
)

# ------------------------------------------------------------
# 13. Resumo estatístico integrado
# ------------------------------------------------------------

resumo_integrado <- df_integrado |>
  pivot_longer(
    cols = -c(lon, lat),
    names_to = "produto",
    values_to = "valor"
  ) |>
  group_by(produto) |>
  summarise(
    n_pixels = sum(!is.na(valor)),
    minimo = min(valor, na.rm = TRUE),
    primeiro_quartil = quantile(valor, 0.25, na.rm = TRUE),
    mediana = median(valor, na.rm = TRUE),
    media = mean(valor, na.rm = TRUE),
    terceiro_quartil = quantile(valor, 0.75, na.rm = TRUE),
    maximo = max(valor, na.rm = TRUE),
    desvio_padrao = sd(valor, na.rm = TRUE),
    .groups = "drop"
  )

write_csv(
  resumo_integrado,
  "tabelas/unidade11/resumo_comparacao_integrada_ensemble.csv"
)

# ------------------------------------------------------------
# 14. Mensagem final
# ------------------------------------------------------------

message("Comparação integrada do ensemble concluída.")
message("Figura dos modelos individuais salva em: figuras/unidade11/unidade11_modelos_individuais.png")
message("Figura dos produtos ensemble salva em: figuras/unidade11/unidade11_produtos_ensemble.png")
message("Figura de incerteza salva em: figuras/unidade11/unidade11_produtos_incerteza.png")
message("Figura síntese salva em: figuras/unidade11/unidade11_comparacao_ensemble.png")

print(resumo_integrado)
