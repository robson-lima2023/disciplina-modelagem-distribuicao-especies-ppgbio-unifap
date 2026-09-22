# ============================================================
# Unidade 2 - Dados de Ocorrência em SDM
# Script 05: Remoção de duplicatas espaciais
# ============================================================

pacotes <- c("dplyr", "readr")

instalar <- pacotes[!pacotes %in% rownames(installed.packages())]
if (length(instalar) > 0) install.packages(instalar)

invisible(lapply(pacotes, library, character.only = TRUE))

oc <- read_csv(
  "dados/unidade02/processados/ocorrencias_02_limpas_coordinatecleaner.csv",
  show_col_types = FALSE
)

n_inicial <- nrow(oc)

oc_sem_dup <- oc %>%
  distinct(especie, lon, lat, .keep_all = TRUE)

n_final <- nrow(oc_sem_dup)

resumo <- data.frame(
  etapa = c("Antes da remoção", "Após remoção"),
  n_registros = c(n_inicial, n_final)
)

write_csv(
  oc_sem_dup,
  "dados/unidade02/processados/ocorrencias_03_sem_duplicatas.csv"
)

write_csv(
  resumo,
  "tabelas/unidade02/resumo_03_duplicatas.csv"
)

print(resumo)

message("Duplicatas removidas.")
