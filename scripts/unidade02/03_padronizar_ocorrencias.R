# ============================================================
# Unidade 2 - Dados de Ocorrência em SDM
# Script 03: Padronização dos registros
# ============================================================

pacotes <- c("dplyr", "readr", "janitor")

instalar <- pacotes[!pacotes %in% rownames(installed.packages())]
if (length(instalar) > 0) install.packages(instalar)

invisible(lapply(pacotes, library, character.only = TRUE))

dir.create("dados/unidade02/processados", recursive = TRUE, showWarnings = FALSE)
dir.create("tabelas/unidade02", recursive = TRUE, showWarnings = FALSE)

oc <- read_csv(
  "dados/unidade02/brutos/ocorrencias_gbif_bradypus_variegatus.csv",
  show_col_types = FALSE
) %>%
  clean_names()

oc_pad <- oc %>%
  rename(
    especie = scientific_name,
    lon = decimal_longitude,
    lat = decimal_latitude,
    pais = country,
    estado = state_province,
    data_evento = event_date,
    tipo_registro = basis_of_record,
    instituicao = institution_code,
    colecao = collection_code,
    numero_catalogo = catalog_number,
    incerteza_m = coordinate_uncertainty_in_meters
  ) %>%
  filter(
    !is.na(lon),
    !is.na(lat),
    lon >= -180,
    lon <= 180,
    lat >= -90,
    lat <= 90
  ) %>%
  mutate(
    fonte = "GBIF",
    especie = as.character(especie)
  )

resumo <- data.frame(
  etapa = c("Registros brutos", "Registros com coordenadas válidas"),
  n_registros = c(nrow(oc), nrow(oc_pad))
)

write_csv(
  oc_pad,
  "dados/unidade02/processados/ocorrencias_01_padronizadas.csv"
)

write_csv(
  resumo,
  "tabelas/unidade02/resumo_01_padronizacao.csv"
)

print(resumo)

message("Padronização concluída.")
