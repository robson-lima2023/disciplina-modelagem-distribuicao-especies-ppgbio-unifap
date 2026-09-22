source("scripts/_bootstrap.R")

# ============================================================
# Unidade 15 - Paleoclima e nicho climático passado
# Script 02: Preparar variáveis paleoclimáticas
# ============================================================

pacotes <- c(
  "dplyr", "readr", "terra", "sf", "ggplot2",
  "ggspatial", "viridis", "tibble", "stringr",
  "tidyr", "patchwork"
)

require_packages(pacotes)
invisible(lapply(pacotes, library, character.only = TRUE))

sf::sf_use_s2(FALSE)
dir.create("dados/unidade15/brutos/paleoclim", recursive = TRUE, showWarnings = FALSE)
dir.create("dados/unidade15/processados", recursive = TRUE, showWarnings = FALSE)
dir.create("figuras/unidade15", recursive = TRUE, showWarnings = FALSE)
dir.create("tabelas/unidade15", recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# 1. Arquivos de entrada
# ------------------------------------------------------------

arquivo_atual <- "dados/unidade04/processados/variaveis_ambientais_selecionadas_bioma.tif"

if (!file.exists(arquivo_atual)) {
  candidatos <- list.files(
    "dados",
    pattern = "variaveis.*bioma.*\\.tif$|ambientais.*bioma.*\\.tif$",
    recursive = TRUE,
    full.names = TRUE,
    ignore.case = TRUE
  )
  if (length(candidatos) > 0) arquivo_atual <- candidatos[1]
}

arquivo_vars <- "resultados/unidade04/variaveis_selecionadas_final.csv"

if (!file.exists(arquivo_vars)) {
  arquivo_vars <- "resultados/unidade10/variaveis_maxent.csv"
}

arquivo_bioma <- "dados/unidade01/brutos/amazon_biome_border.shp"
arquivo_m <- "dados/unidade05/processados/area_m_dinizia.gpkg"
arquivo_oc <- "dados/unidade05/processados/presencas_area_m_dinizia.csv"

if (!file.exists(arquivo_atual)) stop("Raster ambiental atual do bioma não encontrado.")
if (!file.exists(arquivo_vars)) stop("Lista de variáveis selecionadas não encontrada.")
if (!file.exists(arquivo_bioma)) stop("Limite do bioma Amazônia não encontrado.")
if (!file.exists(arquivo_oc)) stop("Ocorrências não encontradas.")

# ------------------------------------------------------------
# 2. Ler dados atuais
# ------------------------------------------------------------

amb_atual <- terra::rast(arquivo_atual)

vars <- readr::read_csv(
  arquivo_vars,
  show_col_types = FALSE
)$variavel

vars <- vars[vars %in% names(amb_atual)]

if (length(vars) < 2) {
  stop("Poucas variáveis selecionadas foram encontradas no raster atual.")
}

amb_atual <- amb_atual[[vars]]

bioma_raw <- sf::st_read(arquivo_bioma, quiet = TRUE)

bioma_proj <- bioma_raw |>
  sf::st_transform(5880) |>
  sf::st_make_valid() |>
  sf::st_union() |>
  sf::st_as_sf() |>
  sf::st_make_valid()

bioma <- bioma_proj |>
  sf::st_transform(terra::crs(amb_atual)) |>
  sf::st_make_valid()

bioma_vect <- terra::vect(bioma)

amb_atual <- terra::crop(amb_atual, bioma_vect)
amb_atual <- terra::mask(amb_atual, bioma_vect)

# ------------------------------------------------------------
# 3. Períodos paleoclimáticos
# ------------------------------------------------------------

periodos <- tibble::tibble(
  periodo = c("holoceno_medio", "lgm", "lig"),
  periodo_legenda = c(
    "Holoceno Médio",
    "Último Máximo Glacial",
    "Último Interglacial"
  ),
  pasta_local = c("holoceno_medio", "lgm", "lig")
)

# ------------------------------------------------------------
# 4. Funções auxiliares
# ------------------------------------------------------------

carregar_local_paleoclim <- function(periodo_chave) {
  
  pasta <- file.path("dados/unidade15/brutos/paleoclim", periodo_chave)
  
  if (!dir.exists(pasta)) return(NULL)
  
  arquivos <- list.files(
    pasta,
    pattern = "\\.(tif|tiff|grd)$",
    full.names = TRUE,
    recursive = TRUE
  )
  
  if (length(arquivos) == 0) return(NULL)
  
  tryCatch(
    terra::rast(arquivos),
    error = function(e) NULL
  )
}

gerar_fallback_paleo <- function(amb_atual, periodo_chave) {
  
  ajuste_temp <- dplyr::case_when(
    periodo_chave == "holoceno_medio" ~ -0.3,
    periodo_chave == "lgm" ~ -4.5,
    periodo_chave == "lig" ~ 1.0,
    TRUE ~ 0
  )
  
  fator_prec <- dplyr::case_when(
    periodo_chave == "holoceno_medio" ~ 0.98,
    periodo_chave == "lgm" ~ 0.80,
    periodo_chave == "lig" ~ 1.05,
    TRUE ~ 1
  )
  
  paleo <- amb_atual
  
  for (nm in names(paleo)) {
    
    if (grepl("temp|temperatura|bio1|bio5|bio8", nm, ignore.case = TRUE)) {
      paleo[[nm]] <- paleo[[nm]] + ajuste_temp
    }
    
    if (grepl("prec|precipitacao|bio12|bio13|bio14|bio18|bio19", nm, ignore.case = TRUE)) {
      paleo[[nm]] <- paleo[[nm]] * fator_prec
    }
    
    if (grepl("sazonalidade|bio4|bio15", nm, ignore.case = TRUE)) {
      paleo[[nm]] <- paleo[[nm]] * (1 + abs(ajuste_temp) * 0.04)
    }
  }
  
  paleo
}

harmonizar_paleo <- function(r_paleo, vars) {
  
  nomes <- names(r_paleo)
  nomes_lower <- tolower(nomes)
  
  bio_ids <- stringr::str_extract(vars, "bio[0-9]+")
  
  if (all(!is.na(bio_ids))) {
    
    idx <- sapply(bio_ids, function(b) {
      which(
        stringr::str_detect(
          nomes_lower,
          paste0("\\b", b, "\\b|_", b, "_|", b, "$")
        )
      )[1]
    })
    
    if (all(!is.na(idx))) {
      r_paleo <- r_paleo[[idx]]
      names(r_paleo) <- vars
      return(r_paleo)
    }
  }
  
  if (terra::nlyr(r_paleo) >= length(vars)) {
    r_paleo <- r_paleo[[seq_along(vars)]]
    names(r_paleo) <- vars
    return(r_paleo)
  }
  
  NULL
}

nome_legivel <- function(x) {
  x |>
    stringr::str_replace_all("_", " ") |>
    stringr::str_replace_all("bio", "BIO") |>
    stringr::str_to_sentence()
}

# ------------------------------------------------------------
# 5. Processar períodos paleoclimáticos
# ------------------------------------------------------------

metadados <- list()
mapas <- list()

for (i in seq_len(nrow(periodos))) {
  
  periodo_chave <- periodos$periodo[i]
  periodo_legenda <- periodos$periodo_legenda[i]
  
  message("Preparando paleoclima: ", periodo_legenda)
  
  origem <- "arquivo_local_paleoclim"
  
  r_paleo <- carregar_local_paleoclim(periodo_chave)
  
  if (is.null(r_paleo)) {
    origem <- "fallback_didatico_derivado_do_atual"
    r_paleo <- gerar_fallback_paleo(amb_atual, periodo_chave)
  } else {
    r_paleo <- harmonizar_paleo(r_paleo, vars)
    
    if (is.null(r_paleo)) {
      origem <- "fallback_didatico_harmonizacao"
      r_paleo <- gerar_fallback_paleo(amb_atual, periodo_chave)
    }
  }
  
  r_paleo <- terra::crop(r_paleo, bioma_vect)
  r_paleo <- terra::mask(r_paleo, bioma_vect)
  
  if (!terra::compareGeom(amb_atual, r_paleo, stopOnError = FALSE)) {
    r_paleo <- terra::resample(r_paleo, amb_atual, method = "bilinear")
  }
  
  names(r_paleo) <- names(amb_atual)
  
  saida <- paste0(
    "dados/unidade15/processados/variaveis_paleoclimaticas_",
    periodo_chave,
    ".tif"
  )
  
  terra::writeRaster(
    r_paleo,
    saida,
    overwrite = TRUE
  )
  
  metadados[[periodo_chave]] <- tibble::tibble(
    periodo = periodo_chave,
    periodo_legenda = periodo_legenda,
    origem = origem,
    arquivo = saida,
    n_variaveis = terra::nlyr(r_paleo),
    variaveis = paste(names(r_paleo), collapse = ", ")
  )
  
  var_plot <- vars[grepl("bio12|prec", vars, ignore.case = TRUE)][1]
  if (is.na(var_plot)) var_plot <- vars[1]
  
  df_plot <- as.data.frame(
    r_paleo[[var_plot]],
    xy = TRUE,
    na.rm = TRUE
  )
  
  names(df_plot) <- c("lon", "lat", "valor")
  
  mapas[[periodo_chave]] <- df_plot |>
    dplyr::mutate(
      periodo = periodo_chave,
      periodo_legenda = periodo_legenda,
      variavel = var_plot,
      variavel_legivel = nome_legivel(var_plot)
    )
}

metadados <- dplyr::bind_rows(metadados)

readr::write_csv(
  metadados,
  "tabelas/unidade15/metadados_paleoclima.csv"
)

# ------------------------------------------------------------
# 6. Mapa diagnóstico das variáveis paleoclimáticas
# ------------------------------------------------------------

mapas_df <- dplyr::bind_rows(mapas)

bioma_plot <- sf::st_transform(bioma, 4326)
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
    linewidth = 0.30
  ) +
  geom_raster(
    data = mapas_df,
    aes(x = lon, y = lat, fill = valor)
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
    name = "Valor ambiental",
    option = "viridis",
    na.value = NA
  ) +
  coord_sf(
    xlim = c(bbox_bioma["xmin"], bbox_bioma["xmax"]),
    ylim = c(bbox_bioma["ymin"], bbox_bioma["ymax"]),
    expand = FALSE
  ) +
  facet_wrap(~ periodo_legenda, ncol = 3) +
  ggspatial::annotation_scale(
    location = "bl",
    width_hint = 0.25,
    text_cex = 0.55
  ) +
  labs(
    title = "Variáveis paleoclimáticas preparadas para projeção histórica",
    subtitle = paste0("Variável exibida: ", unique(mapas_df$variavel_legivel)[1]),
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
  "figuras/unidade15/unidade15_variaveis_paleoclimaticas.png",
  plot = g,
  width = 13,
  height = 6,
  dpi = 600
)

message("Variáveis paleoclimáticas preparadas com sucesso.")
print(metadados)
