# ============================================================
# Unidade 17 - Projeto Integrador
# Script 07: Relatório final do projeto
# ============================================================

pacotes <- c("dplyr", "readr")

instalar <- pacotes[!pacotes %in% rownames(installed.packages())]
if (length(instalar) > 0) install.packages(instalar)

invisible(lapply(pacotes, library, character.only = TRUE))

avaliacao <- read_csv(
  "tabelas/unidade17/avaliacao_modelos_integrador.csv",
  show_col_types = FALSE
)

mudancas <- read_csv(
  "tabelas/unidade17/resumo_mudancas_futuras.csv",
  show_col_types = FALSE
)

melhor_modelo <- avaliacao %>%
  arrange(desc(AUC)) %>%
  slice(1)

relatorio <- c(
  "RELATÓRIO FINAL - PROJETO INTEGRADOR DE SDM",
  "====================================================",
  "",
  paste0("Melhor modelo segundo AUC: ", melhor_modelo$modelo),
  paste0("AUC do melhor modelo: ", round(melhor_modelo$AUC, 3)),
  "",
  "Produtos gerados:",
  "- Dados simulados de ocorrência e ambiente;",
  "- Modelos GLM, GAM, Random Forest e BRT;",
  "- Ensemble médio;",
  "- Mapa de incerteza entre algoritmos;",
  "- Projeções futuras simuladas;",
  "- Mapas de mudança de adequabilidade;",
  "- Tabelas de avaliação e síntese.",
  "",
  "Interpretação:",
  "O projeto integrador demonstra o fluxo completo de SDM em R, desde a preparação dos dados até produtos aplicados à conservação. Os resultados devem ser interpretados como exercício didático reprodutível, com foco na lógica científica e computacional do pipeline."
)

writeLines(
  relatorio,
  "resultados/unidade17/relatorio_final_projeto_integrador.txt"
)

message("Relatório final gerado.")
