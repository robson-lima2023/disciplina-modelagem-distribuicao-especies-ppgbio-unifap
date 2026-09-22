# ============================================================
# Unidade 3 - Variáveis Ambientais em SDM
# Script 04: Preparação conceitual para CHELSA
# ============================================================

dir.create("dados/unidade03/brutos/chelsa", recursive = TRUE, showWarnings = FALSE)
dir.create("tabelas/unidade03", recursive = TRUE, showWarnings = FALSE)

chelsa_info <- data.frame(
  base = "CHELSA",
  descricao = "Climatologies at high resolution for the Earth's land surface areas",
  aplicacao = "Modelagem climática em áreas com forte influência topográfica",
  observacao = "Download pode ser feito diretamente no portal CHELSA ou por pacotes específicos",
  stringsAsFactors = FALSE
)

write.csv(
  chelsa_info,
  "tabelas/unidade03/info_chelsa.csv",
  row.names = FALSE
)

message("Arquivo informativo sobre CHELSA criado.")
