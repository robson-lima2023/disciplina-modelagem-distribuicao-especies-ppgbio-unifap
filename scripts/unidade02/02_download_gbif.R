# ============================================================
# Unidade 2 - Dados de Ocorrência em SDM
# Script 02: Download de ocorrências no GBIF
# ============================================================

pacotes <- c("rgbif", "dplyr", "readr")

instalar <- pacotes[!pacotes %in% rownames(installed.packages())]
if (length(instalar) > 0) install.packages(instalar)

invisible(lapply(pacotes, library, character.only = TRUE))

nome_especie <- "Bradypus variegatus"

dir.create("dados/unidade02/brutos", recursive = TRUE, showWarnings = FALSE)

dados_gbif <- rgbif::occ_search(
  scientificName = nome_especie,
  hasCoordinate = TRUE,
  limit = 10000
)

ocorrencias <- dados_gbif$data

ocorrencias_sel <- ocorrencias %>%
  select(
    scientificName,
    decimalLongitude,
    decimalLatitude,
    country,
    stateProvince,
    eventDate,
    basisOfRecord,
    institutionCode,
    collectionCode,
    catalogNumber,
    coordinateUncertaintyInMeters
  )

write_csv(
  ocorrencias_sel,
  "dados/unidade02/brutos/ocorrencias_gbif_bradypus_variegatus.csv"
)

message("Download concluído.")
message(paste("Número de registros baixados:", nrow(ocorrencias_sel)))
