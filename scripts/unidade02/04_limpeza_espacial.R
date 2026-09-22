# ============================================================
# Unidade 2 - Dados de Ocorrência em SDM
# Script 04: Limpeza espacial com CoordinateCleaner
# ============================================================

pacotes <- c("dplyr", "readr", "CoordinateCleaner")

instalar <- pacotes[!pacotes %in% rownames(installed.packages())]
if (length(instalar) > 0) install.packages(instalar)

invisible(lapply(pacotes, library, character.only = TRUE))

oc <- read_csv(
  "dados/unidade02/processados/ocorrencias_01_padronizadas.csv",
  show_col_types = FALSE
)

oc_cc <- clean_coordinates(
  x = oc,
  lon = "lon",
  lat = "lat",
  species = "especie",
  tests = c(
    "capitals",
    "centroids",
    "duplicates",
    "equal",
    "gbif",
    "institutions",
    "seas",
    "zeros"
  )
)

oc_limpo <- oc_cc %>%
  filter(.summary == TRUE)

oc_removido <- oc_cc %>%
  filter(.summary == FALSE)

resumo <- data.frame(
  etapa = c("Antes da limpeza espacial", "Mantidos", "Removidos"),
  n_registros = c(nrow(oc), nrow(oc_limpo), nrow(oc_removido))
)

write_csv(
  oc_limpo,
  "dados/unidade02/processados/ocorrencias_02_limpas_coordinatecleaner.csv"
)

write_csv(
  oc_removido,
  "dados/unidade02/processados/ocorrencias_removidas_coordinatecleaner.csv"
)

write_csv(
  resumo,
  "tabelas/unidade02/resumo_02_limpeza_espacial.csv"
)

print(resumo)

message("Limpeza espacial concluída.")
