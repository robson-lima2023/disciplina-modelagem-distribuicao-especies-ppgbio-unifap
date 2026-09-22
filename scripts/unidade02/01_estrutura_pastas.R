# ============================================================
# Unidade 2 - Dados de Ocorrência em SDM
# Script 01: Estrutura de pastas
# ============================================================

setwd("C:/Users/rblfl/OneDrive/Documentos/Playground/Species-Distribution-Modeling")

# Este script deve ser executado a partir da raiz do projeto:
# Species-Distribution-Modeling/

pastas <- c(
  "dados/unidade02",
  "dados/unidade02/brutos",
  "dados/unidade02/processados",
  "figuras/unidade02",
  "tabelas/unidade02",
  "resultados/unidade02"
)

for (p in pastas) {
  if (!dir.exists(p)) {
    dir.create(p, recursive = TRUE)
  }
}

message("Estrutura da Unidade 2 criada com sucesso.")

