# ============================================================
# Unidade 3 - Variáveis Ambientais em SDM
# Script 09: Harmonização de variáveis ambientais
# ============================================================

pacotes <- c("terra")

instalar <- pacotes[!pacotes %in% rownames(installed.packages())]
if (length(instalar) > 0) install.packages(instalar)

library(terra)

dir.create("dados/unidade03/processados", recursive = TRUE, showWarnings = FALSE)

worldclim <- rast("dados/unidade03/processados/worldclim_bioclim_10min.tif")
elev <- rast("dados/unidade03/processados/topografia_elevacao_10min.tif")
slope <- rast("dados/unidade03/processados/topografia_declividade_10min.tif")

elev_res <- resample(elev, worldclim[[1]], method = "bilinear")
slope_res <- resample(slope, worldclim[[1]], method = "bilinear")

stack_ambiental <- c(worldclim, elev_res, slope_res)

names(stack_ambiental)[(nlyr(worldclim) + 1):nlyr(stack_ambiental)] <- c(
  "elevacao",
  "declividade"
)

writeRaster(
  stack_ambiental,
  "dados/unidade03/processados/stack_ambiental_harmonizado.tif",
  overwrite = TRUE
)

message("Stack ambiental harmonizado criado.")
