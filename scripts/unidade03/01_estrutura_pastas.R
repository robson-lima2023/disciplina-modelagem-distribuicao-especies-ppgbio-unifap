# ============================================================
# Unidade 3 - Variáveis Ambientais em SDM
# Script 01: Estrutura de pastas
# ============================================================

pastas <- c(
  "dados/unidade03",
  "dados/unidade03/brutos",
  "dados/unidade03/processados",
  "figuras/unidade03",
  "tabelas/unidade03",
  "resultados/unidade03"
)

for (p in pastas) {
  if (!dir.exists(p)) {
    dir.create(p, recursive = TRUE)
  }
}

message("Estrutura da Unidade 3 criada com sucesso.")
