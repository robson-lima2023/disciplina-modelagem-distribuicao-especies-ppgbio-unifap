# ============================================================
# Unidade 3 - Variáveis Ambientais em SDM
# Script 07: Variáveis topográficas com geodata
# ============================================================

pacotes <- c("terra", "geodata")

instalar <- pacotes[!pacotes %in% rownames(installed.packages())]
if (length(instalar) > 0) install.packages(instalar)

invisible(lapply(pacotes, library, character.only = TRUE))

dir.create("dados/unidade03/brutos/topografia", recursive = TRUE, showWarnings = FALSE)
dir.create("figuras/unidade03", recursive = TRUE, showWarnings = FALSE)
dir.create("dados/unidade03/processados", recursive = TRUE, showWarnings = FALSE)

# Exemplo didático: Brasil em baixa resolução.
# Para áreas menores, recomenda-se usar SRTM por tiles.

elev <- geodata::elevation_global(
  res = 10,
  path = "dados/unidade03/brutos/topografia"
)

declividade <- terra::terrain(
  elev,
  v = "slope",
  unit = "degrees"
)

terra::writeRaster(
  elev,
  "dados/unidade03/processados/topografia_elevacao_10min.tif",
  overwrite = TRUE
)

terra::writeRaster(
  declividade,
  "dados/unidade03/processados/topografia_declividade_10min.tif",
  overwrite = TRUE
)

png(
  filename = "figuras/unidade03/unidade03_topografia_elevacao.png",
  width = 1800,
  height = 1200,
  res = 200
)

plot(
  elev,
  main = "Elevação global - exemplo didático"
)

dev.off()

message("Variáveis topográficas processadas.")
