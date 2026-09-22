source("scripts/_bootstrap.R")

# ============================================================
# Unidade 2 - Dados de ocorrência
# Script 04: Rarefação espacial
# ============================================================

pacotes <- c("dplyr", "readr", "sf", "ggplot2", "patchwork")

require_packages(pacotes)
invisible(lapply(pacotes, library, character.only = TRUE))

arquivo_oc <- "dados/unidade02/processados/03_ocorrencias_ambiente_sem_duplicata_raster.csv"

possiveis_shp <- c(
  "dados/unidade01/brutos/amazon_biome_border.shp",
  "dados/unidade01/brutos/amazon_biome_border(1).shp",
  "dados/unidade02/brutos/amazon_biome_border.shp",
  "dados/unidade02/brutos/amazon_biome_border(1).shp"
)

arquivo_bioma <- possiveis_shp[file.exists(possiveis_shp)][1]

if (!file.exists(arquivo_oc)) {
  stop("Execute primeiro o script 03.")
}

oc_env <- read_csv(arquivo_oc, show_col_types = FALSE)

sf::sf_use_s2(FALSE)

amazonia <- sf::st_read(arquivo_bioma, quiet = TRUE) %>%
  sf::st_transform(4326) %>%
  sf::st_make_valid() %>%
  sf::st_buffer(0) %>%
  sf::st_collection_extract("POLYGON") %>%
  sf::st_make_valid()

sf::sf_use_s2(TRUE)

# ------------------------------------------------------------
# 1. Rarefação com distância mínima
# ------------------------------------------------------------

dist_km <- 10

dados_thin <- oc_env %>%
  transmute(
    SPEC = "Dinizia_excelsa",
    LAT = as.numeric(lat),
    LONG = as.numeric(lon)
  ) %>%
  filter(!is.na(LAT), !is.na(LONG))

usar_spthin <- TRUE

if (usar_spthin && !requireNamespace("spThin", quietly = TRUE)) {
  message(
    "Pacote opcional 'spThin' ausente; será usado o método alternativo ",
    "determinístico implementado neste script."
  )
}

if (usar_spthin && requireNamespace("spThin", quietly = TRUE)) {

  book_seed(20260204L)
  library(spThin)
  
  thin_out <- spThin::thin(
    loc.data = dados_thin,
    lat.col = "LAT",
    long.col = "LONG",
    spec.col = "SPEC",
    thin.par = dist_km,
    reps = 1,
    locs.thinned.list.return = TRUE,
    write.files = FALSE,
    verbose = FALSE
  )
  
  oc_thin_coord <- as.data.frame(thin_out[[1]])
  
  # Padronizar nomes retornados pelo spThin
  names(oc_thin_coord) <- tolower(names(oc_thin_coord))
  
  col_lat <- names(oc_thin_coord)[names(oc_thin_coord) %in% c("lat", "latitude", "lat.thinned", "lat_thinned")][1]
  col_lon <- names(oc_thin_coord)[names(oc_thin_coord) %in% c("long", "lon", "longitude", "long.thinned", "long_thinned")][1]
  
  if (is.na(col_lat) || is.na(col_lon)) {
    stop(
      paste0(
        "Não foi possível identificar as colunas de latitude/longitude retornadas pelo spThin. ",
        "Colunas retornadas: ",
        paste(names(oc_thin_coord), collapse = ", ")
      )
    )
  }
  
  oc_thin_coord <- oc_thin_coord %>%
    transmute(
      lat = as.numeric(.data[[col_lat]]),
      lon = as.numeric(.data[[col_lon]])
    ) %>%
    distinct(lon, lat, .keep_all = TRUE)
  
  oc_thin <- oc_env %>%
    mutate(
      lon_join = round(as.numeric(lon), 6),
      lat_join = round(as.numeric(lat), 6)
    ) %>%
    inner_join(
      oc_thin_coord %>%
        mutate(
          lon_join = round(as.numeric(lon), 6),
          lat_join = round(as.numeric(lat), 6)
        ) %>%
        select(lon_join, lat_join),
      by = c("lon_join", "lat_join")
    ) %>%
    select(-lon_join, -lat_join) %>%
    distinct(lon, lat, .keep_all = TRUE)
  
} else {
  
  # Alternativa sem spThin: algoritmo guloso em projeção métrica.
  oc_sf <- sf::st_as_sf(
    oc_env,
    coords = c("lon", "lat"),
    crs = 4326,
    remove = FALSE
  ) %>%
    sf::st_transform(6933)
  
  manter <- rep(FALSE, nrow(oc_sf))
  
  for (i in seq_len(nrow(oc_sf))) {
    if (!any(manter)) {
      manter[i] <- TRUE
    } else {
      distancias <- sf::st_distance(oc_sf[i, ], oc_sf[manter, ])
      if (min(as.numeric(distancias)) >= dist_km * 1000) {
        manter[i] <- TRUE
      }
    }
  }
  
  oc_thin <- oc_env[manter, ]
}

readr::write_csv(
  oc_thin,
  "dados/unidade02/processados/04_ocorrencias_rarefeitas_dinizia.csv"
)

# ------------------------------------------------------------
# 2. Mapa antes e depois
# ------------------------------------------------------------

oc_antes_sf <- sf::st_as_sf(
  oc_env,
  coords = c("lon", "lat"),
  crs = 4326,
  remove = FALSE
)

oc_depois_sf <- sf::st_as_sf(
  oc_thin,
  coords = c("lon", "lat"),
  crs = 4326,
  remove = FALSE
)

p1 <- ggplot() +
  geom_sf(data = amazonia, fill = "grey95", color = "grey35", linewidth = 0.25) +
  geom_sf(data = oc_antes_sf, color = "black", fill = "orange", shape = 21, size = 1.3, alpha = 0.80) +
  coord_sf(expand = FALSE) +
  labs(title = "A) Antes da rarefação", x = "Longitude", y = "Latitude") +
  theme_bw(base_size = 10) +
  theme(plot.title = element_text(face = "bold"))

p2 <- ggplot() +
  geom_sf(data = amazonia, fill = "grey95", color = "grey35", linewidth = 0.25) +
  geom_sf(data = oc_depois_sf, color = "black", fill = "red", shape = 21, size = 1.3, alpha = 0.80) +
  coord_sf(expand = FALSE) +
  labs(title = paste0("B) Depois da rarefação - ", dist_km, " km"), x = "Longitude", y = "Latitude") +
  theme_bw(base_size = 10) +
  theme(plot.title = element_text(face = "bold"))

fig <- p1 | p2

ggsave(
  "figuras/unidade02/unidade02_rarefacao_dinizia.png",
  plot = fig,
  width = 12,
  height = 6,
  dpi = 600
)

resumo <- tibble::tibble(
  etapa = c(
    "Antes da rarefação",
    paste0("Após rarefação espacial de ", dist_km, " km")
  ),
  n = c(nrow(oc_env), nrow(oc_thin))
)

readr::write_csv(
  resumo,
  "tabelas/unidade02/resumo_rarefacao_dinizia.csv"
)

message("Script 04 concluído: rarefação espacial aplicada.")
print(resumo)
