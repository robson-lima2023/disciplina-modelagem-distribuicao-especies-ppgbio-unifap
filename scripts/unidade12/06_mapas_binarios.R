source("scripts/_bootstrap.R")

# ============================================================
# Unidade 12 - Avaliação de modelos em SDM
# Script 06: Mapas binários por limiar TSS
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
# 2. Diretório raiz do projeto
# ------------------------------------------------------------
# ------------------------------------------------------------
# 3. Diretórios de saída
# ------------------------------------------------------------

dir.create("dados/unidade12/processados", recursive = TRUE, showWarnings = FALSE)
dir.create("figuras/unidade12", recursive = TRUE, showWarnings = FALSE)
dir.create("resultados/unidade12", recursive = TRUE, showWarnings = FALSE)
dir.create("tabelas/unidade12", recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# 4. Entradas
# ------------------------------------------------------------

arquivo_metricas <- "tabelas/unidade12/metricas_integradas_modelos.csv"
arquivo_bioma <- "dados/unidade01/brutos/amazon_biome_border.shp"
arquivo_m <- "dados/unidade05/processados/area_m_dinizia.gpkg"
arquivo_oc <- "dados/unidade05/processados/presencas_area_m_dinizia.csv"

if (!file.exists(arquivo_metricas)) {
  stop("Tabela de métricas não encontrada. Execute primeiro o Script 03 da Unidade 12.")
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

metricas <- read_csv(
  arquivo_metricas,
  show_col_types = FALSE
) |>
  mutate(
    modelo = as.character(modelo),
    limiar_TSS = as.numeric(limiar_TSS)
  )

# Nomes precisam coincidir com a coluna modelo gerada na Unidade 12.
arquivos <- c(
  GLM = "resultados/unidade06/adequabilidade_glm_dinizia.tif",
  GAM = "resultados/unidade07/adequabilidade_gam_dinizia.tif",
  RF = "resultados/unidade08/adequabilidade_rf_dinizia.tif",
  BRT = "resultados/unidade09/adequabilidade_brt_dinizia.tif",
  MaxEnt = "resultados/unidade10/adequabilidade_maxent_dinizia.tif",
  Ensemble_media = "resultados/unidade11/ensemble_media_simples_dinizia.tif",
  Ensemble_ponderado = "resultados/unidade11/ensemble_ponderado_dinizia.tif"
)

faltantes <- arquivos[!file.exists(arquivos)]

if (length(faltantes) > 0) {
  stop(
    "Mapas faltantes:\n",
    paste(names(faltantes), faltantes, sep = " = ", collapse = "\n")
  )
}

# Manter apenas modelos com métrica disponível
arquivos <- arquivos[names(arquivos) %in% metricas$modelo]

if (length(arquivos) < 2) {
  stop("Menos de dois modelos possuem métricas e mapas disponíveis para binarização.")
}

# ------------------------------------------------------------
# 5. Ler bioma, área M e ocorrências
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
# 6. Binarizar mapas
# ------------------------------------------------------------

binarios <- list()
diagnostico <- list()

for (modelo_nome in names(arquivos)) {
  
  message("Binarizando modelo: ", modelo_nome)
  
  r <- terra::rast(arquivos[[modelo_nome]])
  
  limiar <- metricas |>
    filter(modelo == modelo_nome) |>
    pull(limiar_TSS)
  
  if (length(limiar) == 0 || is.na(limiar[1]) || !is.finite(limiar[1])) {
    warning("Limiar ausente para ", modelo_nome, ". Usando limiar padrão 0.5.")
    limiar <- 0.5
  }
  
  limiar <- as.numeric(limiar[1])
  
  if (limiar <= 0 || limiar >= 1) {
    warning("Limiar inválido para ", modelo_nome, ". Usando limiar padrão 0.5.")
    limiar <- 0.5
  }
  
  valores <- terra::values(r, mat = FALSE)
  valores <- valores[!is.na(valores)]
  
  if (length(valores) == 0) {
    warning("Raster sem valores válidos para ", modelo_nome)
    next
  }
  
  min_pred <- min(valores, na.rm = TRUE)
  max_pred <- max(valores, na.rm = TRUE)
  media_pred <- mean(valores, na.rm = TRUE)
  sd_pred <- sd(valores, na.rm = TRUE)
  
  r_bin <- terra::ifel(r >= limiar, 1, 0)
  names(r_bin) <- modelo_nome
  
  freq_bin <- as.data.frame(
    terra::freq(r_bin)
  )
  
  n_inadequado <- ifelse(
    0 %in% freq_bin$value,
    freq_bin$count[freq_bin$value == 0],
    0
  )
  
  n_adequado <- ifelse(
    1 %in% freq_bin$value,
    freq_bin$count[freq_bin$value == 1],
    0
  )
  
  total_validos <- n_inadequado + n_adequado
  
  prop_adequado <- ifelse(
    total_validos == 0,
    NA_real_,
    n_adequado / total_validos
  )
  
  diagnostico[[modelo_nome]] <- tibble(
    modelo = modelo_nome,
    arquivo = arquivos[[modelo_nome]],
    limiar_TSS = limiar,
    min_pred = min_pred,
    media_pred = media_pred,
    max_pred = max_pred,
    sd_pred = sd_pred,
    n_inadequado = n_inadequado,
    n_adequado = n_adequado,
    total_validos = total_validos,
    prop_adequado = prop_adequado
  )
  
  binarios[[modelo_nome]] <- r_bin
}

if (length(binarios) == 0) {
  stop("Nenhum mapa binário foi gerado.")
}

diagnostico <- bind_rows(diagnostico)

write_csv(
  diagnostico,
  "tabelas/unidade12/diagnostico_mapas_binarios.csv"
)

# ------------------------------------------------------------
# 7. Alinhar mapas binários e salvar stack
# ------------------------------------------------------------

ref <- binarios[[1]]

binarios_alinhados <- lapply(
  names(binarios),
  function(nm) {
    
    r <- binarios[[nm]]
    
    if (!terra::compareGeom(ref, r, stopOnError = FALSE)) {
      message("Reamostrando mapa binário: ", nm)
      r <- terra::resample(
        r,
        ref,
        method = "near"
      )
    }
    
    names(r) <- nm
    r
  }
)

names(binarios_alinhados) <- names(binarios)

stack_bin <- terra::rast(binarios_alinhados)
names(stack_bin) <- names(binarios_alinhados)

terra::writeRaster(
  stack_bin,
  "resultados/unidade12/mapas_binarios_modelos_dinizia.tif",
  overwrite = TRUE
)

# Consenso binário avaliativo: proporção de mapas classificados como adequados
consenso_binario <- terra::app(
  stack_bin,
  fun = mean,
  na.rm = TRUE
)

names(consenso_binario) <- "consenso_binario_avaliacao"

terra::writeRaster(
  consenso_binario,
  "resultados/unidade12/consenso_binario_avaliacao_dinizia.tif",
  overwrite = TRUE
)

# ------------------------------------------------------------
# 8. Converter mapas para data.frame
# ------------------------------------------------------------

df_list <- lapply(names(stack_bin), function(nm) {
  
  df_nm <- as.data.frame(
    stack_bin[[nm]],
    xy = TRUE,
    na.rm = TRUE
  )
  
  names(df_nm) <- c("lon", "lat", "adequado")
  
  df_nm |>
    mutate(
      adequado = as.integer(adequado),
      classe = case_when(
        adequado == 1 ~ "Adequado",
        adequado == 0 ~ "Inadequado",
        TRUE ~ NA_character_
      ),
      modelo = nm
    ) |>
    filter(!is.na(classe))
})

df <- bind_rows(df_list)

write_csv(
  df,
  "dados/unidade12/processados/mapas_binarios_modelos_long.csv"
)

df_consenso <- as.data.frame(
  consenso_binario,
  xy = TRUE,
  na.rm = TRUE
) |>
  rename(
    lon = x,
    lat = y,
    consenso = consenso_binario_avaliacao
  )

write_csv(
  df_consenso,
  "dados/unidade12/processados/consenso_binario_avaliacao_dinizia.csv"
)

# ------------------------------------------------------------
# 9. Preparar bioma, área M e ocorrências para mapa
# ------------------------------------------------------------

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
# 10. Função auxiliar para mapa binário
# ------------------------------------------------------------

mapa_binario <- function(df_mapa, titulo) {
  
  ggplot() +
    geom_sf(
      data = bioma_plot,
      fill = "grey96",
      color = "grey35",
      linewidth = 0.30
    ) +
    geom_raster(
      data = df_mapa,
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
      size = 0.60,
      stroke = 0.15,
      alpha = 0.70
    ) +
    scale_fill_manual(
      values = c(
        "Inadequado" = "grey88",
        "Adequado" = "#1B7837"
      ),
      limits = c("Inadequado", "Adequado"),
      drop = FALSE,
      name = "Classe"
    ) +
    coord_sf(
      xlim = c(bbox_bioma["xmin"], bbox_bioma["xmax"]),
      ylim = c(bbox_bioma["ymin"], bbox_bioma["ymax"]),
      expand = FALSE
    ) +
    labs(
      title = titulo,
      x = "Longitude",
      y = "Latitude"
    ) +
    theme_bw(base_size = 10) +
    theme(
      plot.title = element_text(face = "bold", size = 11),
      legend.position = "bottom",
      panel.grid.major = element_line(color = "grey88", linewidth = 0.20),
      panel.grid.minor = element_blank()
    )
}

# ------------------------------------------------------------
# 11. Mapas binários individuais com patchwork
# ------------------------------------------------------------

lista_mapas <- df |>
  group_split(modelo) |>
  lapply(function(df_m) {
    
    modelo_nome <- unique(df_m$modelo)
    
    mapa_binario(
      df_mapa = df_m,
      titulo = modelo_nome
    )
  })

n_mapas <- length(lista_mapas)
n_col <- ifelse(n_mapas <= 4, 2, 3)

fig_binarios <- patchwork::wrap_plots(
  lista_mapas,
  ncol = n_col,
  guides = "collect"
) +
  patchwork::plot_annotation(
    title = expression("Mapas binários de adequabilidade para " * italic("Dinizia excelsa")),
    subtitle = "Classificação por limiar ótimo TSS; verde = adequado, cinza = inadequado"
  ) &
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 10),
    legend.position = "bottom"
  )

ggsave(
  "figuras/unidade12/unidade12_mapas_binarios.png",
  plot = fig_binarios,
  width = 14,
  height = ifelse(n_mapas <= 6, 9, 11),
  dpi = 600
)

# ------------------------------------------------------------
# 12. Mapa de consenso binário avaliativo
# ------------------------------------------------------------

g_consenso <- ggplot() +
  geom_sf(
    data = bioma_plot,
    fill = "grey96",
    color = "grey35",
    linewidth = 0.30
  ) +
  geom_raster(
    data = df_consenso,
    aes(
      x = lon,
      y = lat,
      fill = consenso
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
    size = 0.75,
    stroke = 0.16,
    alpha = 0.75
  ) +
  scale_fill_viridis_c(
    name = "Consenso",
    option = "viridis",
    limits = c(0, 1),
    breaks = seq(0, 1, 0.25),
    na.value = NA
  ) +
  coord_sf(
    xlim = c(bbox_bioma["xmin"], bbox_bioma["xmax"]),
    ylim = c(bbox_bioma["ymin"], bbox_bioma["ymax"]),
    expand = FALSE
  ) +
  ggspatial::annotation_scale(
    location = "bl",
    width_hint = 0.28,
    text_cex = 0.65
  ) +
  ggspatial::annotation_north_arrow(
    location = "tr",
    which_north = "true",
    style = ggspatial::north_arrow_fancy_orienteering
  ) +
  labs(
    title = expression("Consenso binário avaliativo para " * italic("Dinizia excelsa")),
    subtitle = "Proporção de modelos classificados como adequados após aplicação dos limiares TSS",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_bw(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10),
    panel.grid.major = element_line(color = "grey88", linewidth = 0.25),
    panel.grid.minor = element_blank(),
    legend.position = "right"
  )

ggsave(
  "figuras/unidade12/unidade12_consenso_binario_avaliacao.png",
  plot = g_consenso,
  width = 9,
  height = 7,
  dpi = 600
)

# ------------------------------------------------------------
# 13. Mensagem final
# ------------------------------------------------------------

message("Mapas binários por limiar TSS gerados com sucesso.")
message("Modelos binarizados: ", paste(names(stack_bin), collapse = ", "))

print(diagnostico)
