source("scripts/_bootstrap.R")

# ============================================================
# Unidade 13 - Projeções climáticas futuras em SDM
# Script 02: Preparar variáveis climáticas futuras
# ============================================================

# ------------------------------------------------------------
# 1. Pacotes
# ------------------------------------------------------------

pacotes <- c(
  "dplyr",
  "readr",
  "terra",
  "sf",
  "geodata",
  "tibble",
  "stringr",
  "ggplot2",
  "ggspatial",
  "viridis",
  "patchwork",
  "tidyr"
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

dir.create("dados/unidade13/brutos/cmip6", recursive = TRUE, showWarnings = FALSE)
dir.create("dados/unidade13/processados", recursive = TRUE, showWarnings = FALSE)
dir.create("figuras/unidade13", recursive = TRUE, showWarnings = FALSE)
dir.create("tabelas/unidade13", recursive = TRUE, showWarnings = FALSE)
dir.create("resultados/unidade13", recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# 4. Entradas
# ------------------------------------------------------------

arquivo_bioma <- "dados/unidade01/brutos/amazon_biome_border.shp"

arquivo_atual <- "dados/unidade04/processados/variaveis_ambientais_selecionadas_bioma.tif"

arquivo_vars <- "dados/unidade04/processados/variaveis_selecionadas_final.csv"

if (!file.exists(arquivo_bioma)) {
  stop("Limite do bioma Amazônia não encontrado: ", arquivo_bioma)
}

if (!file.exists(arquivo_atual)) {
  stop("Raster ambiental selecionado do bioma não encontrado: ", arquivo_atual)
}

if (!file.exists(arquivo_vars)) {
  stop("Arquivo de variáveis selecionadas não encontrado: ", arquivo_vars)
}

# ------------------------------------------------------------
# 5. Leitura dos dados atuais
# ------------------------------------------------------------

bioma_raw <- sf::st_read(
  arquivo_bioma,
  quiet = TRUE
)

amb_atual <- terra::rast(
  arquivo_atual
)

vars_tab <- readr::read_csv(
  arquivo_vars,
  show_col_types = FALSE
)

if (!"variavel" %in% names(vars_tab)) {
  stop("O arquivo de variáveis selecionadas precisa conter a coluna 'variavel'.")
}

vars <- vars_tab$variavel
vars <- vars[vars %in% names(amb_atual)]

if (length(vars) < 2) {
  stop("Poucas variáveis selecionadas foram encontradas no raster atual.")
}

amb_atual <- amb_atual[[vars]]

# ------------------------------------------------------------
# 6. Preparar bioma no CRS do raster
# ------------------------------------------------------------

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

amb_atual <- terra::crop(
  amb_atual,
  bioma_vect
)

amb_atual <- terra::mask(
  amb_atual,
  bioma_vect
)

# ------------------------------------------------------------
# 7. Configurações climáticas
# ------------------------------------------------------------

cenarios <- c("ssp126", "ssp245", "ssp370", "ssp585")

periodo <- "2061-2080"

modelo_gcm <- "MIROC6"

resolucao <- 10

dir_download <- "dados/unidade13/brutos/cmip6"

# ------------------------------------------------------------
# 8. Função: baixar CMIP6 real
# ------------------------------------------------------------

baixar_cmip6 <- function(ssp) {
  
  message("Tentando baixar CMIP6: ", modelo_gcm, " | ", ssp, " | ", periodo)
  
  r <- tryCatch(
    {
      geodata::cmip6_world(
        model = modelo_gcm,
        ssp = ssp,
        time = periodo,
        var = "bioc",
        res = resolucao,
        path = dir_download
      )
    },
    error = function(e) {
      message("Download CMIP6 falhou para ", ssp, ": ", e$message)
      NULL
    }
  )
  
  r
}

# ------------------------------------------------------------
# 9. Função: fallback didático
# ------------------------------------------------------------

gerar_fallback <- function(amb_atual, ssp) {
  
  delta_temp <- dplyr::case_when(
    ssp == "ssp126" ~ 1.0,
    ssp == "ssp245" ~ 1.8,
    ssp == "ssp370" ~ 2.8,
    ssp == "ssp585" ~ 4.0,
    TRUE ~ 2.0
  )
  
  fator_prec <- dplyr::case_when(
    ssp == "ssp126" ~ 0.98,
    ssp == "ssp245" ~ 0.95,
    ssp == "ssp370" ~ 0.92,
    ssp == "ssp585" ~ 0.88,
    TRUE ~ 0.95
  )
  
  futuro <- amb_atual
  
  for (nm in names(futuro)) {
    
    if (grepl("temp|temperatura|bio1|bio5|bio6|bio8|bio9|bio10|bio11", nm, ignore.case = TRUE)) {
      futuro[[nm]] <- futuro[[nm]] + delta_temp
    }
    
    if (grepl("precipitacao|prec|bio12|bio13|bio14|bio16|bio17|bio18|bio19", nm, ignore.case = TRUE)) {
      futuro[[nm]] <- futuro[[nm]] * fator_prec
    }
    
    if (grepl("sazonalidade|bio4|bio15", nm, ignore.case = TRUE)) {
      futuro[[nm]] <- futuro[[nm]] * (1 + delta_temp * 0.03)
    }
  }
  
  futuro
}

# ------------------------------------------------------------
# 10. Função: preparar CMIP6 para as variáveis selecionadas
# ------------------------------------------------------------

preparar_cmip6 <- function(r_cmip6, amb_atual, vars) {
  
  names(r_cmip6) <- paste0("bio", seq_len(terra::nlyr(r_cmip6)))
  
  vars_bio <- stringr::str_extract(vars, "bio[0-9]+")
  
  futuro <- amb_atual
  
  for (i in seq_along(vars)) {
    
    v <- vars[i]
    vb <- vars_bio[i]
    
    if (!is.na(vb) && vb %in% names(r_cmip6)) {
      
      camada <- r_cmip6[[vb]]
      names(camada) <- v
      
      camada <- terra::crop(camada, bioma_vect)
      camada <- terra::mask(camada, bioma_vect)
      camada <- terra::resample(camada, amb_atual[[v]], method = "bilinear")
      
      futuro[[v]] <- camada
      
    } else {
      
      # Variáveis sem equivalente climático futuro, como elevação,
      # declividade e orientação, permanecem constantes.
      futuro[[v]] <- amb_atual[[v]]
    }
  }
  
  names(futuro) <- vars
  
  futuro
}

# ------------------------------------------------------------
# 11. Preparar variáveis futuras por cenário
# ------------------------------------------------------------

metadados <- list()
resumos <- list()
deltas <- list()

for (ssp in cenarios) {
  
  message("Processando cenário: ", ssp)
  
  r_cmip6 <- baixar_cmip6(ssp)
  
  origem <- "CMIP6_geodata"
  
  if (is.null(r_cmip6)) {
    
    futuro <- gerar_fallback(
      amb_atual = amb_atual,
      ssp = ssp
    )
    
    origem <- "fallback_didatico_derivado_do_atual"
    
  } else {
    
    futuro <- preparar_cmip6(
      r_cmip6 = r_cmip6,
      amb_atual = amb_atual,
      vars = vars
    )
  }
  
  futuro <- terra::crop(
    futuro,
    bioma_vect
  )
  
  futuro <- terra::mask(
    futuro,
    bioma_vect
  )
  
  if (!terra::compareGeom(amb_atual, futuro, stopOnError = FALSE)) {
    futuro <- terra::resample(
      futuro,
      amb_atual,
      method = "bilinear"
    )
  }
  
  names(futuro) <- names(amb_atual)
  
  saida <- paste0(
    "dados/unidade13/processados/variaveis_futuras_",
    ssp,
    "_",
    periodo,
    "_",
    modelo_gcm,
    ".tif"
  )
  
  terra::writeRaster(
    futuro,
    saida,
    overwrite = TRUE
  )
  
  delta <- futuro - amb_atual
  names(delta) <- paste0(names(amb_atual), "_delta")
  
  saida_delta <- paste0(
    "resultados/unidade13/mudancas/delta_variaveis_futuras_",
    ssp,
    "_",
    periodo,
    "_",
    modelo_gcm,
    ".tif"
  )
  
  terra::writeRaster(
    delta,
    saida_delta,
    overwrite = TRUE
  )
  
  metadados[[ssp]] <- tibble(
    cenario = ssp,
    periodo = periodo,
    gcm = modelo_gcm,
    origem = origem,
    arquivo_futuro = saida,
    arquivo_delta = saida_delta,
    n_variaveis = terra::nlyr(futuro),
    variaveis = paste(names(futuro), collapse = ", ")
  )
  
  resumo_ssp <- lapply(names(futuro), function(v) {
    
    atual_v <- terra::global(
      amb_atual[[v]],
      c("min", "mean", "max"),
      na.rm = TRUE
    )
    
    futuro_v <- terra::global(
      futuro[[v]],
      c("min", "mean", "max"),
      na.rm = TRUE
    )
    
    delta_v <- terra::global(
      delta[[paste0(v, "_delta")]],
      c("min", "mean", "max"),
      na.rm = TRUE
    )
    
    tibble(
      cenario = ssp,
      variavel = v,
      atual_min = as.numeric(atual_v$min),
      atual_media = as.numeric(atual_v$mean),
      atual_max = as.numeric(atual_v$max),
      futuro_min = as.numeric(futuro_v$min),
      futuro_media = as.numeric(futuro_v$mean),
      futuro_max = as.numeric(futuro_v$max),
      delta_min = as.numeric(delta_v$min),
      delta_media = as.numeric(delta_v$mean),
      delta_max = as.numeric(delta_v$max)
    )
  }) |>
    bind_rows()
  
  resumos[[ssp]] <- resumo_ssp
  
  # Guardar alguns deltas para figura
  delta_df <- as.data.frame(
    delta,
    xy = TRUE,
    na.rm = TRUE
  ) |>
    rename(
      lon = x,
      lat = y
    ) |>
    mutate(cenario = ssp)
  
  deltas[[ssp]] <- delta_df
}

metadados <- bind_rows(metadados)
resumos <- bind_rows(resumos)
deltas_df <- bind_rows(deltas)

write_csv(
  metadados,
  "tabelas/unidade13/metadados_variaveis_futuras.csv"
)

write_csv(
  resumos,
  "tabelas/unidade13/resumo_variaveis_futuras.csv"
)

write_csv(
  deltas_df,
  "dados/unidade13/processados/deltas_variaveis_futuras_long.csv"
)

# ------------------------------------------------------------
# 12. Mapas de mudança climática para variáveis-chave
# ------------------------------------------------------------

bioma_plot <- sf::st_transform(
  bioma,
  4326
)

bbox_bioma <- sf::st_bbox(
  bioma_plot
)

variaveis_chave <- vars[
  grepl(
    "bio5|bio12|bio14|bio18|bio19|temperatura|precipitacao",
    vars,
    ignore.case = TRUE
  )
]

variaveis_chave <- variaveis_chave[seq_len(min(4, length(variaveis_chave)))]

if (length(variaveis_chave) > 0) {
  
  deltas_plot <- deltas_df |>
    select(
      lon,
      lat,
      cenario,
      all_of(paste0(variaveis_chave, "_delta"))
    ) |>
    pivot_longer(
      cols = ends_with("_delta"),
      names_to = "variavel",
      values_to = "delta"
    ) |>
    mutate(
      variavel = stringr::str_replace(variavel, "_delta$", "")
    )
  
  write_csv(
    deltas_plot,
    "dados/unidade13/processados/deltas_variaveis_chave_long.csv"
  )
  
  g_delta <- ggplot() +
    geom_sf(
      data = bioma_plot,
      fill = "grey96",
      color = "grey35",
      linewidth = 0.25
    ) +
    geom_raster(
      data = deltas_plot,
      aes(
        x = lon,
        y = lat,
        fill = delta
      )
    ) +
    geom_sf(
      data = bioma_plot,
      fill = NA,
      color = "grey35",
      linewidth = 0.25
    ) +
    scale_fill_gradient2(
      name = "Δ futuro - atual",
      low = "#2166AC",
      mid = "white",
      high = "#B2182B",
      midpoint = 0,
      na.value = NA
    ) +
    coord_sf(
      xlim = c(bbox_bioma["xmin"], bbox_bioma["xmax"]),
      ylim = c(bbox_bioma["ymin"], bbox_bioma["ymax"]),
      expand = FALSE
    ) +
    facet_grid(
      variavel ~ cenario
    ) +
    labs(
      title = "Mudança projetada nas variáveis ambientais selecionadas",
      subtitle = paste0(
        modelo_gcm,
        " | ",
        periodo,
        " | valores representam futuro - atual"
      ),
      x = "Longitude",
      y = "Latitude"
    ) +
    theme_bw(base_size = 9) +
    theme(
      plot.title = element_text(face = "bold", size = 13),
      plot.subtitle = element_text(size = 10),
      strip.text = element_text(face = "bold", size = 8),
      panel.grid.major = element_line(color = "grey88", linewidth = 0.20),
      panel.grid.minor = element_blank(),
      legend.position = "right"
    )
  
  ggsave(
    "figuras/unidade13/unidade13_mudancas_variaveis_futuras.png",
    plot = g_delta,
    width = 14,
    height = 10,
    dpi = 600
  )
}

# ------------------------------------------------------------
# 13. Mensagem final
# ------------------------------------------------------------

message("Variáveis climáticas futuras preparadas com sucesso.")
message("Cenários processados: ", paste(cenarios, collapse = ", "))
message("Período: ", periodo)
message("GCM: ", modelo_gcm)
message("Número de variáveis: ", length(vars))

print(metadados)
