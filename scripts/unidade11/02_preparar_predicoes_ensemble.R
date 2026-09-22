source("scripts/_bootstrap.R")

# ============================================================
# Unidade 11 - Modelagem Ensemble em SDM
# Script 02: Preparar predições dos modelos
# ============================================================

# ------------------------------------------------------------
# 1. Pacotes
# ------------------------------------------------------------

pacotes <- c(
  "dplyr",
  "readr",
  "terra",
  "tibble",
  "tidyr"
)

require_packages(pacotes)
invisible(lapply(pacotes, library, character.only = TRUE))

# ------------------------------------------------------------
# 2. Diretório raiz do projeto
# ------------------------------------------------------------
# ------------------------------------------------------------
# 3. Diretórios de saída
# ------------------------------------------------------------

dir.create("dados/unidade11/processados", recursive = TRUE, showWarnings = FALSE)
dir.create("tabelas/unidade11", recursive = TRUE, showWarnings = FALSE)
dir.create("resultados/unidade11", recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# 4. Arquivos de entrada
# ------------------------------------------------------------

arquivos <- c(
  GLM = "resultados/unidade06/adequabilidade_glm_dinizia.tif",
  GAM = "resultados/unidade07/adequabilidade_gam_dinizia.tif",
  RF = "resultados/unidade08/adequabilidade_rf_dinizia.tif",
  BRT = "resultados/unidade09/adequabilidade_brt_dinizia.tif",
  MaxEnt = "resultados/unidade10/adequabilidade_maxent_dinizia.tif"
)

faltantes <- arquivos[!file.exists(arquivos)]

if (length(faltantes) > 0) {
  stop(
    "Os seguintes mapas de adequabilidade ainda não existem:\n",
    paste(names(faltantes), faltantes, sep = " = ", collapse = "\n"),
    "\nExecute as unidades correspondentes antes da Unidade 11."
  )
}

# ------------------------------------------------------------
# 5. Ler rasters individuais
# ------------------------------------------------------------

rasters <- lapply(
  arquivos,
  terra::rast
)

nomes_modelos <- names(rasters)

# Usar GLM como referência espacial
ref <- rasters[[1]]

if (is.na(terra::crs(ref)) || terra::crs(ref) == "") {
  stop("O raster de referência não possui sistema de coordenadas definido.")
}

# ------------------------------------------------------------
# 6. Alinhar geometria dos rasters
# ------------------------------------------------------------

rasters_alinhados <- lapply(
  nomes_modelos,
  function(nm) {
    
    r <- rasters[[nm]]
    
    if (terra::nlyr(r) != 1) {
      stop("O raster do modelo ", nm, " deve conter apenas uma camada.")
    }
    
    if (is.na(terra::crs(r)) || terra::crs(r) == "") {
      stop("O raster do modelo ", nm, " não possui CRS definido.")
    }
    
    if (!terra::compareGeom(ref, r, stopOnError = FALSE)) {
      message("Reamostrando raster do modelo ", nm, " para a geometria de referência.")
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

stack_modelos <- terra::rast(rasters_alinhados)
names(stack_modelos) <- nomes_modelos

# ------------------------------------------------------------
# 7. Garantir valores dentro do intervalo 0-1
# ------------------------------------------------------------

stack_modelos <- terra::clamp(
  stack_modelos,
  lower = 0,
  upper = 1,
  values = TRUE
)

# ------------------------------------------------------------
# 8. Exportar stack de predições
# ------------------------------------------------------------

terra::writeRaster(
  stack_modelos,
  "dados/unidade11/processados/stack_predicoes_modelos.tif",
  overwrite = TRUE
)

terra::writeRaster(
  stack_modelos,
  "resultados/unidade11/stack_predicoes_modelos.tif",
  overwrite = TRUE
)

# ------------------------------------------------------------
# 9. Converter para tabela pixel a pixel
# ------------------------------------------------------------

df_pred <- as.data.frame(
  stack_modelos,
  xy = TRUE,
  na.rm = TRUE
) |>
  rename(
    lon = x,
    lat = y
  )

write_csv(
  df_pred,
  "dados/unidade11/processados/predicoes_modelos_ensemble.csv"
)

# ------------------------------------------------------------
# 10. Resumo estatístico das predições
# ------------------------------------------------------------

resumo <- df_pred |>
  pivot_longer(
    cols = all_of(nomes_modelos),
    names_to = "modelo",
    values_to = "adequabilidade"
  ) |>
  group_by(modelo) |>
  summarise(
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

write_csv(
  resumo,
  "tabelas/unidade11/resumo_predicoes_modelos.csv"
)

# ------------------------------------------------------------
# 11. Correlação espacial entre modelos
# ------------------------------------------------------------

cor_modelos <- stats::cor(
  df_pred[, nomes_modelos],
  use = "complete.obs",
  method = "pearson"
)

cor_modelos_tab <- as.data.frame(cor_modelos) |>
  tibble::rownames_to_column("modelo_1") |>
  pivot_longer(
    cols = -modelo_1,
    names_to = "modelo_2",
    values_to = "correlacao_pearson"
  )

write_csv(
  cor_modelos_tab,
  "tabelas/unidade11/correlacao_espacial_modelos_ensemble.csv"
)

# ------------------------------------------------------------
# 12. Metadados do stack
# ------------------------------------------------------------

metadados <- tibble(
  modelo = nomes_modelos,
  arquivo_origem = unname(arquivos),
  camada_stack = nomes_modelos,
  n_linhas = terra::nrow(stack_modelos),
  n_colunas = terra::ncol(stack_modelos),
  resolucao_x = terra::res(stack_modelos)[1],
  resolucao_y = terra::res(stack_modelos)[2],
  crs = terra::crs(stack_modelos)
)

write_csv(
  metadados,
  "tabelas/unidade11/metadados_stack_predicoes_modelos.csv"
)

# ------------------------------------------------------------
# 13. Mensagem final
# ------------------------------------------------------------

message("Predições individuais preparadas para modelagem ensemble.")
message("Modelos incluídos: ", paste(nomes_modelos, collapse = ", "))
message("Número de pixels válidos: ", nrow(df_pred))
message("Stack salvo em: dados/unidade11/processados/stack_predicoes_modelos.tif")

print(resumo)
