source("scripts/_bootstrap.R")

# ============================================================
# Unidade 2 - Dados de ocorrência
# Script 01: Compilar ocorrências de Dinizia excelsa
# ============================================================
#
# Objetivo:
# Integrar registros locais/inventário com registros opcionais do GBIF.
# O script usa como base o arquivo coord-Dinizia.xlsx, mas também
# permite baixar registros do GBIF quando houver internet disponível.
#
# Entrada esperada:
# dados/unidade01/brutos/coord-Dinizia.xlsx
#
# Saída:
# dados/unidade02/processados/01_ocorrencias_compiladas_dinizia.csv
# tabelas/unidade02/resumo_fontes_ocorrencia_dinizia.csv
# ============================================================
pacotes <- c(
  "dplyr", "readr", "readxl", "stringr", "tibble"
)

require_packages(pacotes)
invisible(lapply(pacotes, library, character.only = TRUE))

pastas <- c(
  "dados/unidade02/brutos",
  "dados/unidade02/processados",
  "figuras/unidade02",
  "tabelas/unidade02",
  "resultados/unidade02"
)

for (p in pastas) dir.create(p, recursive = TRUE, showWarnings = FALSE)

arquivo_local <- "dados/unidade01/brutos/coord-Dinizia.xlsx"

if (!file.exists(arquivo_local)) {
  arquivo_local <- "dados/unidade02/brutos/coord-Dinizia.xlsx"
}

if (!file.exists(arquivo_local)) {
  stop("Arquivo coord-Dinizia.xlsx não encontrado em dados/unidade01/brutos/ ou dados/unidade02/brutos/")
}

# ------------------------------------------------------------
# 1. Ler registros locais
# ------------------------------------------------------------

oc_local_raw <- readxl::read_excel(arquivo_local, sheet = 1)

oc_local <- oc_local_raw %>%
  transmute(
    especie = as.character(species),
    lon = as.numeric(lon),
    lat = as.numeric(lat),
    fonte = "Inventario_ou_base_local",
    origem = "Arquivo local",
    id_origem = NA_character_
  ) %>%
  filter(
    !is.na(lon),
    !is.na(lat),
    lon >= -180,
    lon <= 180,
    lat >= -90,
    lat <= 90
  )

# ------------------------------------------------------------
# 2. Baixar GBIF de forma opcional
# ------------------------------------------------------------
# Se não houver internet ou se o pacote rgbif não estiver disponível,
# o script segue apenas com os dados locais.

baixar_gbif <- TRUE

oc_gbif <- tibble(
  especie = character(),
  lon = numeric(),
  lat = numeric(),
  fonte = character(),
  origem = character(),
  id_origem = character()
)

if (baixar_gbif) {

  if (!requireNamespace("rgbif", quietly = TRUE)) {
    message(
      "Pacote opcional 'rgbif' ausente; o download do GBIF será ignorado. ",
      "Instale-o explicitamente para combinar registros remotos."
    )
  }
  
  if (requireNamespace("rgbif", quietly = TRUE)) {
    
    library(rgbif)
    
    gbif_raw <- tryCatch(
      {
        rgbif::occ_search(
          scientificName = "Dinizia excelsa",
          hasCoordinate = TRUE,
          limit = 3000
        )
      },
      error = function(e) NULL
    )
    
    if (!is.null(gbif_raw) && !is.null(gbif_raw$data) && nrow(gbif_raw$data) > 0) {
      
      oc_gbif <- gbif_raw$data %>%
        transmute(
          especie = as.character(scientificName),
          lon = as.numeric(decimalLongitude),
          lat = as.numeric(decimalLatitude),
          fonte = "GBIF",
          origem = as.character(basisOfRecord),
          id_origem = as.character(gbifID)
        ) %>%
        filter(
          !is.na(lon),
          !is.na(lat),
          lon >= -180,
          lon <= 180,
          lat >= -90,
          lat <= 90
        )
    }
  }
}

# ------------------------------------------------------------
# 3. Integrar e padronizar
# ------------------------------------------------------------

oc_compiladas <- bind_rows(oc_local, oc_gbif) %>%
  mutate(
    especie = stringr::str_squish(especie),
    especie_padrao = "Dinizia excelsa"
  ) %>%
  filter(str_detect(str_to_lower(especie), "dinizia excelsa")) %>%
  distinct(lon, lat, fonte, .keep_all = TRUE)

readr::write_csv(
  oc_compiladas,
  "dados/unidade02/processados/01_ocorrencias_compiladas_dinizia.csv"
)

resumo_fontes <- oc_compiladas %>%
  count(fonte, origem, name = "n_registros") %>%
  arrange(desc(n_registros))

readr::write_csv(
  resumo_fontes,
  "tabelas/unidade02/resumo_fontes_ocorrencia_dinizia.csv"
)

message("Script 01 concluído: ocorrências compiladas.")
print(resumo_fontes)
