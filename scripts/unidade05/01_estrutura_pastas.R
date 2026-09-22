source("scripts/_bootstrap.R")

# ============================================================
# Unidade 5 - Área acessível (M), BAM e Background
# Script 01: Estrutura de pastas
# ============================================================
pastas <- c(
  "dados/unidade05",
  "dados/unidade05/brutos",
  "dados/unidade05/processados",
  "figuras/unidade05",
  "tabelas/unidade05",
  "resultados/unidade05"
)

for (p in pastas) {
  if (!dir.exists(p)) {
    dir.create(p, recursive = TRUE)
  }
}

message("Estrutura da Unidade 5 criada com sucesso.")
