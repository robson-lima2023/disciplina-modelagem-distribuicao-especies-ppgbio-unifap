source("scripts/_bootstrap.R")

# ============================================================
# Unidade 13 - Projeções climáticas futuras em SDM
# Script 01: Estrutura de pastas
# ============================================================
pastas <- c(
  "dados/unidade13",
  "dados/unidade13/brutos",
  "dados/unidade13/processados",
  "figuras/unidade13",
  "tabelas/unidade13",
  "resultados/unidade13",
  "resultados/unidade13/modelos_individuais",
  "resultados/unidade13/ensemble",
  "resultados/unidade13/mudancas",
  "resultados/unidade13/binarios"
)

for (p in pastas) {
  dir.create(p, recursive = TRUE, showWarnings = FALSE)
}

message("Estrutura de pastas da Unidade 13 criada com sucesso.")
