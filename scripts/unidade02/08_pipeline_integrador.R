# ============================================================
# Unidade 2 - Dados de Ocorrência em SDM
# Script 08: Pipeline integrador da Unidade 2
# ============================================================

source("scripts/unidade02/01_estrutura_pastas.R")
source("scripts/unidade02/02_download_gbif.R")
source("scripts/unidade02/03_padronizar_ocorrencias.R")
source("scripts/unidade02/04_limpeza_espacial.R")
source("scripts/unidade02/05_remover_duplicatas.R")
source("scripts/unidade02/06_mapa_ocorrencias.R")
source("scripts/unidade02/07_rarefacao_espacial.R")

message("Pipeline completo da Unidade 2 executado com sucesso.")