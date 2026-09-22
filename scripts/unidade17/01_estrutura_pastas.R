source("scripts/_bootstrap.R")

# ============================================================
# Unidade 17 - Aplicações à conservação
# Script 01: Estrutura de pastas
# ============================================================
pastas <- c(
  "dados/unidade17",
  "dados/unidade17/brutos",
  "dados/unidade17/processados",
  "figuras/unidade17",
  "tabelas/unidade17",
  "resultados/unidade17",
  "resultados/unidade17/componentes"
)

for (p in pastas) {
  dir.create(p, recursive = TRUE, showWarnings = FALSE)
}

message("Estrutura de pastas da Unidade 17 criada com sucesso.")
