# ============================================================
# Unidade 17 - Projeto Integrador
# Script 01: Estrutura do projeto
# ============================================================

setwd("C:/Users/rblfl/OneDrive/Documentos/Playground/Species-Distribution-Modeling")

pastas <- c(
  "dados/unidade17/processados",
  "figuras/unidade17",
  "tabelas/unidade17",
  "resultados/unidade17"
)

for (p in pastas) {
  dir.create(p, recursive = TRUE, showWarnings = FALSE)
}

message("Estrutura da Unidade 17 criada com sucesso.")

