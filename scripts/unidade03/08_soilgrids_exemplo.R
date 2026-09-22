# ============================================================
# Unidade 3 - Variáveis Ambientais em SDM
# Script 08: Variáveis edáficas - SoilGrids
# ============================================================

dir.create("tabelas/unidade03", recursive = TRUE, showWarnings = FALSE)
dir.create("dados/unidade03/brutos/soilgrids", recursive = TRUE, showWarnings = FALSE)

soilgrids_variaveis <- data.frame(
  variavel = c(
    "phh2o",
    "soc",
    "clay",
    "sand",
    "silt",
    "cec",
    "bdod"
  ),
  significado = c(
    "pH em água",
    "Carbono orgânico do solo",
    "Teor de argila",
    "Teor de areia",
    "Teor de silte",
    "Capacidade de troca catiônica",
    "Densidade aparente"
  ),
  relevancia_ecologica = c(
    "Acidez e disponibilidade de nutrientes",
    "Matéria orgânica e fertilidade",
    "Retenção de água e estrutura",
    "Drenagem e textura",
    "Textura e retenção hídrica",
    "Fertilidade química",
    "Compactação e estrutura física"
  )
)

write.csv(
  soilgrids_variaveis,
  "tabelas/unidade03/soilgrids_variaveis_exemplo.csv",
  row.names = FALSE
)

message("Tabela de variáveis SoilGrids criada.")
