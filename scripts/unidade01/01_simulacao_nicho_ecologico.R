source("scripts/_bootstrap.R")

# ============================================================
# Unidade 1 - Nicho ecológico real de Dinizia excelsa
# Script: 01_nicho_ecologico_dinizia.R
# ============================================================
pacotes <- c(
  "dplyr", "readr", "readxl", "ggplot2", "sf", "terra",
  "geodata", "tidyr", "stringr", "ggspatial", "viridis",
  "patchwork", "tibble"
)

require_packages(pacotes)
invisible(lapply(pacotes, library, character.only = TRUE))

# ------------------------------------------------------------
# 1. Pastas
# ------------------------------------------------------------

pastas <- c(
  "dados/unidade01/brutos",
  "dados/unidade01/processados",
  "figuras/unidade01",
  "tabelas/unidade01",
  "resultados/unidade01"
)

for (p in pastas) dir.create(p, recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# 2. Arquivos de entrada
# ------------------------------------------------------------

arquivo_ocorrencias <- "dados/unidade01/brutos/coord-Dinizia.xlsx"

possiveis_shp <- c(
  "dados/unidade01/brutos/amazon_biome_border.shp",
  "dados/unidade01/brutos/amazon_biome_border(1).shp"
)

arquivo_bioma <- possiveis_shp[file.exists(possiveis_shp)][1]

if (!file.exists(arquivo_ocorrencias)) {
  stop("Coloque coord-Dinizia.xlsx em dados/unidade01/brutos/")
}

if (is.na(arquivo_bioma) || !file.exists(arquivo_bioma)) {
  stop("Shapefile do bioma Amazônia não encontrado em dados/unidade01/brutos/")
}

# ------------------------------------------------------------
# 3. Ler ocorrências corretas
# ------------------------------------------------------------

oc_raw <- readxl::read_excel(arquivo_ocorrencias, sheet = 1)

oc <- oc_raw %>%
  transmute(
    especie = as.character(species),
    lon = as.numeric(lon),
    lat = as.numeric(lat)
  ) %>%
  filter(
    !is.na(lon),
    !is.na(lat),
    lon >= -180,
    lon <= 180,
    lat >= -90,
    lat <= 90
  ) %>%
  distinct(lon, lat, .keep_all = TRUE)

write_csv(
  oc,
  "dados/unidade01/processados/dinizia_ocorrencias_01_limpas.csv"
)

# ------------------------------------------------------------
# 4. Ler e corrigir limite do bioma Amazônia
# ------------------------------------------------------------

sf::sf_use_s2(FALSE)

amazonia <- sf::st_read(arquivo_bioma, quiet = TRUE) %>%
  sf::st_transform(4326) %>%
  sf::st_make_valid() %>%
  sf::st_buffer(0) %>%
  sf::st_collection_extract("POLYGON") %>%
  sf::st_make_valid()

amazonia_union <- amazonia
amazonia_vect <- terra::makeValid(terra::vect(amazonia_union))

# ------------------------------------------------------------
# 5. Filtrar ocorrências dentro do bioma
# ------------------------------------------------------------

oc_sf <- sf::st_as_sf(
  oc,
  coords = c("lon", "lat"),
  crs = 4326,
  remove = FALSE
)

oc_vect <- terra::vect(oc_sf)

oc_amazonia_vect <- terra::intersect(
  oc_vect,
  amazonia_vect
)

oc_amazonia <- sf::st_as_sf(oc_amazonia_vect)

oc_amazonia_df <- oc_amazonia %>%
  sf::st_drop_geometry() %>%
  dplyr::select(especie, lon, lat) %>%
  distinct(lon, lat, .keep_all = TRUE)

oc_amazonia <- sf::st_as_sf(
  oc_amazonia_df,
  coords = c("lon", "lat"),
  crs = 4326,
  remove = FALSE
)

sf::sf_use_s2(TRUE)

write_csv(
  oc_amazonia_df,
  "dados/unidade01/processados/dinizia_ocorrencias_02_amazonia.csv"
)

sf::st_write(
  oc_amazonia,
  "dados/unidade01/processados/dinizia_ocorrencias_amazonia.gpkg",
  delete_dsn = TRUE,
  quiet = TRUE
)

# ------------------------------------------------------------
# 6. Variáveis bioclimáticas atuais - WorldClim
# ------------------------------------------------------------

wc_dir <- "dados/unidade01/brutos/worldclim"
dir.create(wc_dir, recursive = TRUE, showWarnings = FALSE)

bio <- geodata::worldclim_global(
  var = "bio",
  res = 10,
  path = wc_dir
)

names(bio) <- paste0("bio", 1:19)

bio_sel <- bio[[c("bio1", "bio4", "bio12", "bio15")]]

names(bio_sel) <- c(
  "bio1_temp_media_anual",
  "bio4_sazonalidade_temp",
  "bio12_precipitacao_anual",
  "bio15_sazonalidade_prec"
)

# Corrigir temperatura se vier multiplicada por 10
if (terra::global(bio_sel[["bio1_temp_media_anual"]], "max", na.rm = TRUE)[1, 1] > 100) {
  bio_sel[["bio1_temp_media_anual"]] <- bio_sel[["bio1_temp_media_anual"]] / 10
}

bio_amazonia <- bio_sel %>%
  terra::crop(amazonia_vect) %>%
  terra::mask(amazonia_vect)

terra::writeRaster(
  bio_amazonia,
  "dados/unidade01/processados/worldclim_bioclim_amazonia_unidade01.tif",
  overwrite = TRUE
)

# ------------------------------------------------------------
# 7. Extrair valores ambientais nas ocorrências
# ------------------------------------------------------------

valores_oc <- terra::extract(
  bio_amazonia,
  terra::vect(oc_amazonia)
)

oc_env <- bind_cols(
  oc_amazonia_df,
  valores_oc %>% dplyr::select(-ID)
) %>%
  filter(if_all(starts_with("bio"), ~ !is.na(.x)))

write_csv(
  oc_env,
  "dados/unidade01/processados/dinizia_ocorrencias_03_ambiente.csv"
)

# ------------------------------------------------------------
# 8. Background ambiental amazônico
# ------------------------------------------------------------

set.seed(123)

background <- terra::spatSample(
  bio_amazonia,
  size = 10000,
  method = "random",
  na.rm = TRUE,
  xy = TRUE,
  values = TRUE
) %>%
  as.data.frame() %>%
  rename(lon = x, lat = y) %>%
  mutate(tipo = "Background amazônico")

write_csv(
  background,
  "dados/unidade01/processados/background_ambiental_amazonia_unidade01.csv"
)

# ------------------------------------------------------------
# 9. Função cartográfica para mapas individuais
# ------------------------------------------------------------

plotar_variavel_bioma <- function(raster_layer, nome_variavel, arquivo_saida) {
  
  raster_df <- as.data.frame(
    raster_layer,
    xy = TRUE,
    na.rm = TRUE
  )
  
  names(raster_df) <- c("lon", "lat", "valor")
  
  g <- ggplot() +
    geom_raster(
      data = raster_df,
      aes(x = lon, y = lat, fill = valor)
    ) +
    geom_sf(
      data = amazonia_union,
      fill = NA,
      color = "grey20",
      linewidth = 0.35
    ) +
    geom_sf(
      data = oc_amazonia,
      color = "black",
      fill = "red",
      shape = 21,
      size = 1.7,
      stroke = 0.25,
      alpha = 0.85
    ) +
    scale_fill_viridis_c(name = nome_variavel) +
    coord_sf(expand = FALSE) +
    ggspatial::annotation_scale(location = "bl", width_hint = 0.35) +
    ggspatial::annotation_north_arrow(
      location = "tr",
      which_north = "true",
      style = ggspatial::north_arrow_fancy_orienteering
    ) +
    labs(
      title = expression("Ocorrências de " * italic("Dinizia excelsa") * " no bioma Amazônia"),
      subtitle = nome_variavel,
      x = "Longitude",
      y = "Latitude"
    ) +
    theme_bw(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold"),
      panel.grid = element_line(color = "grey88"),
      legend.position = "right"
    )
  
  ggsave(
    arquivo_saida,
    plot = g,
    width = 8.5,
    height = 7,
    dpi = 600
  )
  
  return(g)
}

# ------------------------------------------------------------
# 10. Mapas ambientais individuais
# ------------------------------------------------------------

mapa_bio1 <- plotar_variavel_bioma(
  bio_amazonia[["bio1_temp_media_anual"]],
  "BIO1 - Temperatura média anual (°C)",
  "figuras/unidade01/unidade01_mapa_bio1_dinizia.png"
)

mapa_bio4 <- plotar_variavel_bioma(
  bio_amazonia[["bio4_sazonalidade_temp"]],
  "BIO4 - Sazonalidade da temperatura",
  "figuras/unidade01/unidade01_mapa_bio4_dinizia.png"
)

mapa_bio12 <- plotar_variavel_bioma(
  bio_amazonia[["bio12_precipitacao_anual"]],
  "BIO12 - Precipitação anual (mm)",
  "figuras/unidade01/unidade01_mapa_bio12_dinizia.png"
)

mapa_bio15 <- plotar_variavel_bioma(
  bio_amazonia[["bio15_sazonalidade_prec"]],
  "BIO15 - Sazonalidade da precipitação",
  "figuras/unidade01/unidade01_mapa_bio15_dinizia.png"
)

# ------------------------------------------------------------
# 11. Figura integrada com escalas próprias por variável
# ------------------------------------------------------------
# Importante:
# ggplot2 não permite uma legenda independente por painel em facet_wrap
# usando a mesma estética fill. Por isso, a solução correta é criar
# mapas independentes e combiná-los com patchwork.

criar_mapa_patch <- function(raster_layer, titulo_painel, nome_legenda) {
  
  raster_df <- as.data.frame(
    raster_layer,
    xy = TRUE,
    na.rm = TRUE
  )
  
  names(raster_df) <- c("lon", "lat", "valor")
  
  ggplot() +
    geom_raster(
      data = raster_df,
      aes(x = lon, y = lat, fill = valor)
    ) +
    geom_sf(
      data = amazonia_union,
      fill = NA,
      color = "grey20",
      linewidth = 0.25
    ) +
    geom_sf(
      data = oc_amazonia,
      color = "black",
      fill = "red",
      shape = 21,
      size = 0.75,
      stroke = 0.15,
      alpha = 0.85
    ) +
    scale_fill_viridis_c(name = nome_legenda) +
    coord_sf(expand = FALSE) +
    labs(
      title = titulo_painel,
      x = "Longitude",
      y = "Latitude"
    ) +
    theme_bw(base_size = 9) +
    theme(
      plot.title = element_text(face = "bold", size = 10),
      panel.grid = element_line(color = "grey90"),
      legend.position = "right",
      legend.title = element_text(size = 8),
      legend.text = element_text(size = 7),
      axis.title = element_text(size = 8),
      axis.text = element_text(size = 7)
    )
}

p_bio1 <- criar_mapa_patch(
  bio_amazonia[["bio1_temp_media_anual"]],
  "BIO1 - Temperatura média anual",
  "°C"
)

p_bio4 <- criar_mapa_patch(
  bio_amazonia[["bio4_sazonalidade_temp"]],
  "BIO4 - Sazonalidade da temperatura",
  "Índice"
)

p_bio12 <- criar_mapa_patch(
  bio_amazonia[["bio12_precipitacao_anual"]],
  "BIO12 - Precipitação anual",
  "mm"
)

p_bio15 <- criar_mapa_patch(
  bio_amazonia[["bio15_sazonalidade_prec"]],
  "BIO15 - Sazonalidade da precipitação",
  "Índice"
)

mapa_patchwork <- (p_bio1 | p_bio12) / (p_bio4 | p_bio15) +
  patchwork::plot_annotation(
    title = expression("Variáveis ambientais selecionadas e ocorrências de " * italic("Dinizia excelsa")),
    subtitle = "Bioma Amazônia | WorldClim atual | Pontos vermelhos = ocorrências corrigidas"
  )

ggsave(
  "figuras/unidade01/unidade01_mapas_variaveis_patchwork_dinizia.png",
  plot = mapa_patchwork,
  width = 13,
  height = 9,
  dpi = 600
)

# ------------------------------------------------------------
# 12. Mapa geral das ocorrências
# ------------------------------------------------------------

mapa_oc <- ggplot() +
  geom_sf(
    data = amazonia_union,
    fill = "grey95",
    color = "grey30",
    linewidth = 0.35
  ) +
  geom_sf(
    data = oc_amazonia,
    color = "black",
    fill = "red",
    shape = 21,
    size = 1.8,
    stroke = 0.25,
    alpha = 0.85
  ) +
  coord_sf(expand = FALSE) +
  ggspatial::annotation_scale(location = "bl", width_hint = 0.35) +
  ggspatial::annotation_north_arrow(
    location = "tr",
    which_north = "true",
    style = ggspatial::north_arrow_fancy_orienteering
  ) +
  labs(
    title = expression("Registros corrigidos de " * italic("Dinizia excelsa") * " no bioma Amazônia"),
    subtitle = "Pontos de ocorrência filtrados espacialmente pelo limite do bioma",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid = element_line(color = "grey88")
  )

ggsave(
  "figuras/unidade01/unidade01_mapa_ocorrencias_dinizia.png",
  plot = mapa_oc,
  width = 8.5,
  height = 7,
  dpi = 600
)

# ------------------------------------------------------------
# 13. PCA do espaço ambiental amazônico
# ------------------------------------------------------------

vars_amb <- c(
  "bio1_temp_media_anual",
  "bio4_sazonalidade_temp",
  "bio12_precipitacao_anual",
  "bio15_sazonalidade_prec"
)

pca <- prcomp(
  background %>% dplyr::select(all_of(vars_amb)) %>% tidyr::drop_na(),
  center = TRUE,
  scale. = TRUE
)

scores_bg <- as.data.frame(
  predict(pca, newdata = background[, vars_amb])
) %>%
  mutate(tipo = "Background amazônico")

scores_oc <- as.data.frame(
  predict(pca, newdata = oc_env[, vars_amb])
) %>%
  mutate(tipo = "Presença de Dinizia excelsa")

scores <- bind_rows(scores_bg, scores_oc)

variancia <- data.frame(
  componente = paste0("PC", seq_along(pca$sdev)),
  proporcao = (pca$sdev^2) / sum(pca$sdev^2),
  acumulada = cumsum((pca$sdev^2) / sum(pca$sdev^2))
)

write_csv(
  variancia,
  "tabelas/unidade01/pca_variancia_explicada_nicho_dinizia.csv"
)

loadings <- as.data.frame(pca$rotation) %>%
  tibble::rownames_to_column("variavel")

write_csv(
  loadings,
  "tabelas/unidade01/pca_loadings_nicho_dinizia.csv"
)

pc1_lab <- paste0("PC1 (", round(100 * variancia$proporcao[1], 1), "%)")
pc2_lab <- paste0("PC2 (", round(100 * variancia$proporcao[2], 1), "%)")

grafico_pca <- ggplot() +
  geom_point(
    data = scores %>% filter(tipo == "Background amazônico"),
    aes(x = PC1, y = PC2),
    color = "grey75",
    alpha = 0.35,
    size = 0.7
  ) +
  geom_point(
    data = scores %>% filter(tipo == "Presença de Dinizia excelsa"),
    aes(x = PC1, y = PC2),
    color = "red",
    alpha = 0.90,
    size = 1.8
  ) +
  stat_ellipse(
    data = scores %>% filter(tipo == "Presença de Dinizia excelsa"),
    aes(x = PC1, y = PC2),
    color = "red",
    linewidth = 0.7,
    linetype = "dashed",
    level = 0.80
  ) +
  labs(
    title = expression("Nicho climático multivariado de " * italic("Dinizia excelsa")),
    subtitle = "PCA das variáveis bioclimáticas no bioma Amazônia",
    x = pc1_lab,
    y = pc2_lab
  ) +
  theme_bw(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

ggsave(
  "figuras/unidade01/unidade01_pca_nicho_climatico_dinizia.png",
  plot = grafico_pca,
  width = 8,
  height = 6,
  dpi = 600
)

# ------------------------------------------------------------
# 14. Nicho climático bivariado
# ------------------------------------------------------------

grafico_nicho_tp <- ggplot() +
  geom_point(
    data = background,
    aes(
      x = bio1_temp_media_anual,
      y = bio12_precipitacao_anual
    ),
    color = "grey70",
    alpha = 0.35,
    size = 0.7
  ) +
  geom_point(
    data = oc_env,
    aes(
      x = bio1_temp_media_anual,
      y = bio12_precipitacao_anual
    ),
    color = "red",
    alpha = 0.85,
    size = 1.8
  ) +
  labs(
    title = expression("Espaço climático ocupado por " * italic("Dinizia excelsa")),
    subtitle = "Ocorrências corrigidas versus background climático do bioma Amazônia",
    x = "BIO1 - Temperatura média anual (°C)",
    y = "BIO12 - Precipitação anual (mm)"
  ) +
  theme_bw(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

ggsave(
  "figuras/unidade01/unidade01_nicho_temperatura_precipitacao_dinizia.png",
  plot = grafico_nicho_tp,
  width = 8,
  height = 6,
  dpi = 600
)

# ------------------------------------------------------------
# 15. Tabelas de resumo
# ------------------------------------------------------------

resumo_ocorrencias <- data.frame(
  etapa = c(
    "Registros brutos",
    "Registros únicos válidos",
    "Registros dentro do bioma Amazônia",
    "Registros com ambiente extraído"
  ),
  n = c(
    nrow(oc_raw),
    nrow(oc),
    nrow(oc_amazonia_df),
    nrow(oc_env)
  )
)

write_csv(
  resumo_ocorrencias,
  "tabelas/unidade01/resumo_ocorrencias_dinizia.csv"
)

resumo_ambiente_oc <- oc_env %>%
  summarise(
    n = n(),
    temp_media = mean(bio1_temp_media_anual, na.rm = TRUE),
    temp_min = min(bio1_temp_media_anual, na.rm = TRUE),
    temp_max = max(bio1_temp_media_anual, na.rm = TRUE),
    precip_media = mean(bio12_precipitacao_anual, na.rm = TRUE),
    precip_min = min(bio12_precipitacao_anual, na.rm = TRUE),
    precip_max = max(bio12_precipitacao_anual, na.rm = TRUE),
    saz_temp_media = mean(bio4_sazonalidade_temp, na.rm = TRUE),
    saz_prec_media = mean(bio15_sazonalidade_prec, na.rm = TRUE)
  )

write_csv(
  resumo_ambiente_oc,
  "tabelas/unidade01/resumo_nicho_climatico_dinizia.csv"
)

# ------------------------------------------------------------
# 16. Salvar objetos
# ------------------------------------------------------------

saveRDS(
  list(
    ocorrencias_limpas = oc,
    ocorrencias_amazonia = oc_amazonia_df,
    ocorrencias_ambiente = oc_env,
    background = background,
    bio_amazonia = bio_amazonia,
    pca = pca,
    variancia_pca = variancia,
    loadings_pca = loadings
  ),
  "resultados/unidade01/objetos_nicho_dinizia_unidade01.rds"
)

message("Unidade 1 concluída com mapas independentes e figura integrada com escalas próprias.")
