source("scripts/_bootstrap.R")

# ============================================================
# Unidade 11 - Modelagem Ensemble em SDM
# Script 05: Consenso binário entre modelos
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

dir.create("dados/unidade11/processados", recursive = TRUE, showWarnings = FALSE)
dir.create("figuras/unidade11", recursive = TRUE, showWarnings = FALSE)
dir.create("resultados/unidade11", recursive = TRUE, showWarnings = FALSE)
dir.create("tabelas/unidade11", recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# 4. Arquivos de entrada
# ------------------------------------------------------------

arquivo_stack <- "dados/unidade11/processados/stack_predicoes_modelos.tif"

arquivo_bioma <- "dados/unidade01/brutos/amazon_biome_border.shp"

arquivo_m <- "dados/unidade05/processados/area_m_dinizia.gpkg"

arquivo_oc <- "dados/unidade05/processados/presencas_area_m_dinizia.csv"

if (!file.exists(arquivo_stack)) {
  stop("Stack de predições não encontrado. Execute o Script 02 da Unidade 11.")
}

if (!file.exists(arquivo_bioma)) {
  stop("Limite do bioma Amazônia não encontrado: ", arquivo_bioma)
}

if (!file.exists(arquivo_oc)) {
  stop("Arquivo de ocorrências não encontrado: ", arquivo_oc)
}

if (!file.exists(arquivo_m)) {
  warning("Área M não encontrada. O mapa será gerado sem contorno da área M.")
}

# ------------------------------------------------------------
# 5. Leitura dos dados
# ------------------------------------------------------------

stack_modelos <- terra::rast(
  arquivo_stack
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

if (terra::nlyr(stack_modelos) < 2) {
  stop("O stack deve conter pelo menos dois modelos para consenso binário.")
}

nomes_modelos <- names(stack_modelos)

# ------------------------------------------------------------
# 6. Função para ler limiares TSS
# ------------------------------------------------------------

ler_limiar <- function(arquivo, nome_modelo) {
  
  if (!file.exists(arquivo)) {
    warning("Arquivo de avaliação não encontrado para ", nome_modelo, ": ", arquivo)
    return(
      tibble(
        modelo = nome_modelo,
        limiar = NA_real_,
        origem_limiar = NA_character_
      )
    )
  }
  
  tab <- read_csv(
    arquivo,
    show_col_types = FALSE
  )
  
  candidatos_limiar <- c(
    "limiar_TSS",
    "limiar",
    "threshold",
    "limiar_tss"
  )
  
  col_limiar <- candidatos_limiar[candidatos_limiar %in% names(tab)][1]
  
  if (is.na(col_limiar)) {
    warning("Nenhuma coluna de limiar encontrada em: ", arquivo)
    return(
      tibble(
        modelo = nome_modelo,
        limiar = NA_real_,
        origem_limiar = NA_character_
      )
    )
  }
  
  tibble(
    modelo = nome_modelo,
    limiar = as.numeric(tab[[col_limiar]][1]),
    origem_limiar = col_limiar
  )
}

# ------------------------------------------------------------
# 7. Ler limiares dos modelos
# ------------------------------------------------------------

limiares <- bind_rows(
  ler_limiar("tabelas/unidade06/avaliacao_glm_dinizia.csv", "GLM"),
  ler_limiar("tabelas/unidade07/avaliacao_gam_dinizia.csv", "GAM"),
  ler_limiar("tabelas/unidade08/avaliacao_rf_dinizia.csv", "RF"),
  ler_limiar("tabelas/unidade09/avaliacao_brt_dinizia.csv", "BRT"),
  ler_limiar("tabelas/unidade10/avaliacao_maxent_dinizia.csv", "MaxEnt")
) |>
  filter(modelo %in% nomes_modelos)

if (nrow(limiares) == 0) {
  stop("Nenhum limiar foi encontrado para os modelos do stack.")
}

# Substituir limiares ausentes ou inválidos por 0.5
limiares <- limiares |>
  mutate(
    limiar = ifelse(
      is.na(limiar) | !is.finite(limiar) | limiar <= 0 | limiar >= 1,
      0.5,
      limiar
    ),
    origem_limiar = ifelse(
      is.na(origem_limiar),
      "limiar_padrao_0.5",
      origem_limiar
    )
  ) |>
  arrange(match(modelo, nomes_modelos))

write_csv(
  limiares,
  "tabelas/unidade11/limiares_consenso_binario.csv"
)

# ------------------------------------------------------------
# 8. Gerar rasters binários por modelo
# ------------------------------------------------------------

stack_ordenado <- stack_modelos[[limiares$modelo]]

binarios <- stack_ordenado

for (i in seq_len(terra::nlyr(stack_ordenado))) {
  
  binarios[[i]] <- stack_ordenado[[i]] >= limiares$limiar[i]
  
}

names(binarios) <- paste0(limiares$modelo, "_binario")

terra::writeRaster(
  binarios,
  "resultados/unidade11/stack_binario_modelos_dinizia.tif",
  overwrite = TRUE
)

# ------------------------------------------------------------
# 9. Calcular consenso binário
# ------------------------------------------------------------

consenso <- terra::app(
  binarios,
  fun = mean,
  na.rm = TRUE
)

names(consenso) <- "consenso_binario"

terra::writeRaster(
  consenso,
  "resultados/unidade11/consenso_binario_dinizia.tif",
  overwrite = TRUE
)

# Consenso forte: pelo menos 70% dos modelos classificam como adequado
consenso_forte <- consenso >= 0.70
names(consenso_forte) <- "consenso_forte"

terra::writeRaster(
  consenso_forte,
  "resultados/unidade11/consenso_binario_forte_dinizia.tif",
  overwrite = TRUE
)

# ------------------------------------------------------------
# 10. Converter consenso para tabela
# ------------------------------------------------------------

df <- as.data.frame(
  consenso,
  xy = TRUE,
  na.rm = TRUE
) |>
  rename(
    lon = x,
    lat = y,
    consenso = consenso_binario
  )

write_csv(
  df,
  "dados/unidade11/processados/consenso_binario_dinizia.csv"
)

df_forte <- as.data.frame(
  consenso_forte,
  xy = TRUE,
  na.rm = TRUE
) |>
  rename(
    lon = x,
    lat = y,
    consenso_forte = consenso_forte
  )

write_csv(
  df_forte,
  "dados/unidade11/processados/consenso_binario_forte_dinizia.csv"
)

# ------------------------------------------------------------
# 11. Resumo estatístico do consenso binário
# ------------------------------------------------------------

resumo_consenso <- tibble(
  produto = c(
    "Consenso binário médio",
    "Consenso forte >= 0.70"
  ),
  n_modelos = terra::nlyr(binarios),
  n_pixels = c(
    nrow(df),
    nrow(df_forte)
  ),
  minimo = c(
    min(df$consenso, na.rm = TRUE),
    min(df_forte$consenso_forte, na.rm = TRUE)
  ),
  media = c(
    mean(df$consenso, na.rm = TRUE),
    mean(as.numeric(df_forte$consenso_forte), na.rm = TRUE)
  ),
  mediana = c(
    median(df$consenso, na.rm = TRUE),
    median(as.numeric(df_forte$consenso_forte), na.rm = TRUE)
  ),
  maximo = c(
    max(df$consenso, na.rm = TRUE),
    max(df_forte$consenso_forte, na.rm = TRUE)
  )
)

write_csv(
  resumo_consenso,
  "tabelas/unidade11/resumo_consenso_binario.csv"
)

# ------------------------------------------------------------
# 12. Preparar bioma, área M e ocorrências para mapa
# ------------------------------------------------------------

bioma_proj <- bioma_raw |>
  sf::st_transform(5880) |>
  sf::st_make_valid() |>
  sf::st_union() |>
  sf::st_as_sf() |>
  sf::st_make_valid()

bioma <- bioma_proj |>
  sf::st_transform(terra::crs(consenso)) |>
  sf::st_make_valid()

bioma_plot <- sf::st_transform(
  bioma,
  4326
)

area_m_plot <- NULL

if (!is.null(area_m)) {
  area_m_plot <- area_m |>
    sf::st_make_valid() |>
    sf::st_transform(4326)
}

if (!all(c("lon", "lat") %in% names(oc))) {
  stop("O arquivo de ocorrências precisa conter as colunas 'lon' e 'lat'.")
}

oc_sf <- sf::st_as_sf(
  oc,
  coords = c("lon", "lat"),
  crs = 4326,
  remove = FALSE
)

bbox_bioma <- sf::st_bbox(bioma_plot)

# ------------------------------------------------------------
# 13. Mapa do consenso binário
# ------------------------------------------------------------

g <- ggplot() +
  geom_sf(
    data = bioma_plot,
    fill = "grey96",
    color = "grey35",
    linewidth = 0.35
  ) +
  geom_raster(
    data = df,
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
        linewidth = 0.30,
        linetype = "dashed"
      )
    }
  } +
  geom_sf(
    data = oc_sf,
    color = "black",
    fill = "red",
    shape = 21,
    size = 1.05,
    stroke = 0.20,
    alpha = 0.80
  ) +
  scale_fill_viridis_c(
    name = "Consenso",
    option = "viridis",
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.25),
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
    title = expression("Consenso binário para " * italic("Dinizia excelsa")),
    subtitle = "Proporção de modelos classificando cada célula como ambientalmente adequada",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_bw(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 9),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    legend.position = "right",
    panel.grid.major = element_line(color = "grey88", linewidth = 0.25),
    panel.grid.minor = element_blank()
  )

ggsave(
  "figuras/unidade11/unidade11_consenso_binario.png",
  plot = g,
  width = 9,
  height = 7,
  dpi = 600
)

# ------------------------------------------------------------
# 14. Mensagem final
# ------------------------------------------------------------

message("Consenso binário gerado com sucesso.")
message("Modelos incluídos: ", paste(limiares$modelo, collapse = ", "))
message("Número de modelos: ", terra::nlyr(binarios))
message("Raster salvo em: resultados/unidade11/consenso_binario_dinizia.tif")
message("Mapa salvo em: figuras/unidade11/unidade11_consenso_binario.png")

print(limiares)
