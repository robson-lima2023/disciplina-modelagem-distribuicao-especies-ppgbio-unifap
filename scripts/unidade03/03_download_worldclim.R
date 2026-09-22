# ============================================================
# Unidade 3 - Variáveis Ambientais em SDM
# Script 03: Download e visualização do WorldClim
# ============================================================

pacotes <- c("terra", "geodata")

instalar <- pacotes[!pacotes %in% rownames(installed.packages())]
if (length(instalar) > 0) install.packages(instalar)

invisible(lapply(pacotes, library, character.only = TRUE))

dir.create("dados/unidade03/brutos/worldclim", recursive = TRUE, showWarnings = FALSE)
dir.create("figuras/unidade03", recursive = TRUE, showWarnings = FALSE)

wc <- geodata::worldclim_global(
  var = "bio",
  res = 10,
  path = "dados/unidade03/brutos/worldclim"
)

bio1 <- wc[[1]]

png(
  filename = "figuras/unidade03/unidade03_worldclim_bio1.png",
  width = 1800,
  height = 1200,
  res = 200
)

plot(
  bio1,
  main = "WorldClim BIO1 - Temperatura média anual"
)

dev.off()

terra::writeRaster(
  wc,
  "dados/unidade03/processados/worldclim_bioclim_10min.tif",
  overwrite = TRUE
)

message("WorldClim baixado e processado com sucesso.")
