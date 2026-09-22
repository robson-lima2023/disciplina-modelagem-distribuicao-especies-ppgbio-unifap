# ============================================================
# Unidade 2 - Dados de Ocorrência em SDM
# Script 07: Rarefação espacial
# ============================================================

pacotes <- c("dplyr", "readr", "spThin")

instalar <- pacotes[!pacotes %in% rownames(installed.packages())]
if (length(instalar) > 0) install.packages(instalar)

invisible(lapply(pacotes, library, character.only = TRUE))

oc <- read_csv(
  "dados/unidade02/processados/ocorrencias_03_sem_duplicatas.csv",
  show_col_types = FALSE
)

oc_thin <- oc %>%
  select(especie, lat, lon)

set.seed(123)

thin_result <- thin(
  loc.data = oc_thin,
  lat.col = "lat",
  long.col = "lon",
  spec.col = "especie",
  thin.par = 25,
  reps = 100,
  locs.thinned.list.return = TRUE,
  write.files = FALSE,
  verbose = FALSE
)

tamanhos <- sapply(thin_result, nrow)
melhor <- which.max(tamanhos)

oc_rarefeito <- thin_result[[melhor]]

resumo <- data.frame(
  etapa = c("Antes da rarefação", "Após rarefação"),
  n_registros = c(nrow(oc), nrow(oc_rarefeito))
)

write_csv(
  oc_rarefeito,
  "dados/unidade02/processados/ocorrencias_04_rarefeitas.csv"
)

write_csv(
  resumo,
  "tabelas/unidade02/resumo_04_rarefacao.csv"
)

print(resumo)

message("Rarefação espacial concluída.")
