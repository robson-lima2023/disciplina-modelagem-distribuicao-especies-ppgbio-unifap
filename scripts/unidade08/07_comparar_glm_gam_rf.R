source("scripts/_bootstrap.R")

# ============================================================
# Unidade 8 - Random Forest em SDM
# Script 07: Comparar GLM, GAM e Random Forest
# ============================================================

# ------------------------------------------------------------
# 1. Pacotes
# ------------------------------------------------------------

pacotes <- c(
  "dplyr", "readr", "terra", "sf",
  "ggplot2", "ggspatial", "viridis",
  "tidyr", "patchwork", "tibble"
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

dir.create("figuras/unidade08", recursive = TRUE, showWarnings = FALSE)
dir.create("tabelas/unidade08", recursive = TRUE, showWarnings = FALSE)
dir.create("resultados/unidade08", recursive = TRUE, showWarnings = FALSE)
dir.create("dados/unidade08/processados", recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# 4. Arquivos de entrada
# ------------------------------------------------------------

arquivos <- c(
  GLM = "resultados/unidade06/adequabilidade_glm_dinizia.tif",
  GAM = "resultados/unidade07/adequabilidade_gam_dinizia.tif",
  Random_Forest = "resultados/unidade08/adequabilidade_rf_dinizia.tif"
)

arquivo_bioma <- "dados/unidade01/brutos/amazon_biome_border.shp"
arquivo_m <- "dados/unidade05/processados/area_m_dinizia.gpkg"
arquivo_oc <- "dados/unidade05/processados/presencas_area_m_dinizia.csv"

faltantes <- arquivos[!file.exists(arquivos)]

if (length(faltantes) > 0) {
  stop("Arquivos de raster faltantes: ", paste(faltantes, collapse = ", "))
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
# 5. Ler e alinhar rasters
# ------------------------------------------------------------

r_glm <- terra::rast(arquivos["GLM"])
r_gam <- terra::rast(arquivos["GAM"])
r_rf  <- terra::rast(arquivos["Random_Forest"])

ref <- r_glm

if (!terra::compareGeom(ref, r_gam, stopOnError = FALSE)) {
  r_gam <- terra::resample(r_gam, ref, method = "bilinear")
}

if (!terra::compareGeom(ref, r_rf, stopOnError = FALSE)) {
  r_rf <- terra::resample(r_rf, ref, method = "bilinear")
}

names(r_glm) <- "GLM"
names(r_gam) <- "GAM"
names(r_rf)  <- "Random_Forest"

r_stack <- c(r_glm, r_gam, r_rf)

# ------------------------------------------------------------
# 6. Consenso entre modelos
# ------------------------------------------------------------

r_media <- mean(r_stack, na.rm = TRUE)
names(r_media) <- "consenso_media"

r_sd <- app(r_stack, fun = sd, na.rm = TRUE)
names(r_sd) <- "incerteza_sd"

terra::writeRaster(
  r_media,
  "resultados/unidade08/consenso_media_glm_gam_rf.tif",
  overwrite = TRUE
)

terra::writeRaster(
  r_sd,
  "resultados/unidade08/incerteza_sd_glm_gam_rf.tif",
  overwrite = TRUE
)

# ------------------------------------------------------------
# 7. Dados pixel a pixel
# ------------------------------------------------------------

df_pixel <- as.data.frame(
  c(r_stack, r_media, r_sd),
  xy = TRUE,
  na.rm = TRUE
) |>
  rename(
    lon = x,
    lat = y
  )

write_csv(
  df_pixel,
  "dados/unidade08/processados/comparacao_pixel_glm_gam_rf.csv"
)

# ------------------------------------------------------------
# 8. Correlação entre modelos
# ------------------------------------------------------------

cor_modelos <- stats::cor(
  df_pixel[, c("GLM", "GAM", "Random_Forest")],
  use = "complete.obs",
  method = "pearson"
)

cor_modelos_tab <- as.data.frame(cor_modelos) |>
  tibble::rownames_to_column("modelo_1") |>
  tidyr::pivot_longer(
    cols = -modelo_1,
    names_to = "modelo_2",
    values_to = "correlacao_pearson"
  )

write_csv(
  cor_modelos_tab,
  "tabelas/unidade08/correlacao_espacial_glm_gam_rf.csv"
)

# ------------------------------------------------------------
# 9. Resumo estatístico dos rasters
# ------------------------------------------------------------

resumo_modelos <- df_pixel |>
  tidyr::pivot_longer(
    cols = c(GLM, GAM, Random_Forest, consenso_media, incerteza_sd),
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
  resumo_modelos,
  "tabelas/unidade08/resumo_espacial_glm_gam_rf.csv"
)

# ------------------------------------------------------------
# 10. Ler bioma, área M e ocorrências
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

bioma_plot <- sf::st_transform(bioma, 4326)
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

oc_plot <- sf::st_as_sf(
  oc,
  coords = c("lon", "lat"),
  crs = 4326,
  remove = FALSE
)

# ------------------------------------------------------------
# 11. Data frames para mapas
# ------------------------------------------------------------

df_modelos <- df_pixel |>
  select(lon, lat, GLM, GAM, Random_Forest) |>
  pivot_longer(
    cols = c(GLM, GAM, Random_Forest),
    names_to = "modelo",
    values_to = "adequabilidade"
  ) |>
  mutate(
    modelo = recode(
      modelo,
      Random_Forest = "Random Forest"
    )
  )

df_consenso <- as.data.frame(
  r_media,
  xy = TRUE,
  na.rm = TRUE
) |>
  rename(
    lon = x,
    lat = y,
    consenso_media = consenso_media
  )

df_incerteza <- as.data.frame(
  r_sd,
  xy = TRUE,
  na.rm = TRUE
) |>
  rename(
    lon = x,
    lat = y,
    incerteza_sd = incerteza_sd
  )

# ------------------------------------------------------------
# 12. Mapa facetado dos três modelos
# ------------------------------------------------------------

g_modelos <- ggplot() +
  geom_sf(
    data = bioma_plot,
    fill = "grey96",
    color = "grey35",
    linewidth = 0.30
  ) +
  geom_raster(
    data = df_modelos,
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
    data = oc_plot,
    color = "black",
    fill = "red",
    shape = 21,
    size = 0.65,
    stroke = 0.16,
    alpha = 0.70
  ) +
  facet_wrap(~ modelo, ncol = 3) +
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
  labs(
    title = expression("Comparação de adequabilidade para " * italic("Dinizia excelsa")),
    subtitle = "GLM, GAM e Random Forest calibrados com as mesmas variáveis selecionadas por VIF",
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
  "figuras/unidade08/unidade08_comparacao_glm_gam_rf.png",
  plot = g_modelos,
  width = 13,
  height = 5.8,
  dpi = 600
)

# ------------------------------------------------------------
# 13. Mapa de consenso
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
    aes(x = lon, y = lat, fill = consenso_media)
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
    data = oc_plot,
    color = "black",
    fill = "red",
    shape = 21,
    size = 0.75,
    stroke = 0.18,
    alpha = 0.75
  ) +
  scale_fill_viridis_c(
    name = "Consenso",
    option = "viridis",
    limits = c(0, 1),
    na.value = NA
  ) +
  coord_sf(
    xlim = c(bbox_bioma["xmin"], bbox_bioma["xmax"]),
    ylim = c(bbox_bioma["ymin"], bbox_bioma["ymax"]),
    expand = FALSE
  ) +
  labs(
    title = expression("Consenso médio de adequabilidade para " * italic("Dinizia excelsa")),
    subtitle = "Média das predições GLM, GAM e Random Forest",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_bw(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10),
    panel.grid.minor = element_blank()
  )

ggsave(
  "figuras/unidade08/unidade08_consenso_media_glm_gam_rf.png",
  plot = g_consenso,
  width = 9,
  height = 7,
  dpi = 600
)

# ------------------------------------------------------------
# 14. Mapa de incerteza
# ------------------------------------------------------------

g_incerteza <- ggplot() +
  geom_sf(
    data = bioma_plot,
    fill = "grey96",
    color = "grey35",
    linewidth = 0.30
  ) +
  geom_raster(
    data = df_incerteza,
    aes(x = lon, y = lat, fill = incerteza_sd)
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
  scale_fill_viridis_c(
    name = "DP entre modelos",
    option = "magma",
    na.value = NA
  ) +
  coord_sf(
    xlim = c(bbox_bioma["xmin"], bbox_bioma["xmax"]),
    ylim = c(bbox_bioma["ymin"], bbox_bioma["ymax"]),
    expand = FALSE
  ) +
  labs(
    title = "Incerteza espacial entre modelos",
    subtitle = "Desvio-padrão das predições GLM, GAM e Random Forest",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_bw(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10),
    panel.grid.minor = element_blank()
  )

ggsave(
  "figuras/unidade08/unidade08_incerteza_sd_glm_gam_rf.png",
  plot = g_incerteza,
  width = 9,
  height = 7,
  dpi = 600
)

# ------------------------------------------------------------
# 15. Figura composta consenso + incerteza
# ------------------------------------------------------------

g_patch <- g_consenso + g_incerteza +
  patchwork::plot_annotation(
    title = expression("Síntese espacial multi-modelo para " * italic("Dinizia excelsa")),
    subtitle = "Consenso médio e incerteza entre GLM, GAM e Random Forest"
  ) &
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10)
  )

ggsave(
  "figuras/unidade08/unidade08_sintese_consenso_incerteza_glm_gam_rf.png",
  plot = g_patch,
  width = 14,
  height = 7,
  dpi = 600
)

# ------------------------------------------------------------
# 16. Mensagem final
# ------------------------------------------------------------

message("Comparação GLM-GAM-RF concluída.")
message("Raster de consenso salvo em: resultados/unidade08/consenso_media_glm_gam_rf.tif")
message("Raster de incerteza salvo em: resultados/unidade08/incerteza_sd_glm_gam_rf.tif")
message("Figura principal salva em: figuras/unidade08/unidade08_comparacao_glm_gam_rf.png")

print(resumo_modelos)
