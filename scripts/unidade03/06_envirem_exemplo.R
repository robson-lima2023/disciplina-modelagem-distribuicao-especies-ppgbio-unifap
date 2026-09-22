# ============================================================
# Unidade 3 - Variáveis Ambientais em SDM
# Script 06: Exemplo conceitual ENVIREM
# ============================================================

dir.create("tabelas/unidade03", recursive = TRUE, showWarnings = FALSE)

envirem_variaveis <- data.frame(
  variavel = c(
    "PET",
    "Climatic Moisture Index",
    "Continentalidade",
    "Emberger Pluviothermic Quotient"
  ),
  significado = c(
    "Evapotranspiração potencial",
    "Balanço entre precipitação e demanda evaporativa",
    "Amplitude térmica associada à distância do oceano",
    "Índice bioclimático baseado em precipitação e temperatura"
  ),
  relevancia = c(
    "Disponibilidade energética e demanda hídrica",
    "Estresse hídrico climático",
    "Gradientes térmicos regionais",
    "Representação integrada de clima mediterrâneo/seco"
  )
)

write.csv(
  envirem_variaveis,
  "tabelas/unidade03/envirem_variaveis_exemplo.csv",
  row.names = FALSE
)

message("Tabela conceitual ENVIREM criada.")
