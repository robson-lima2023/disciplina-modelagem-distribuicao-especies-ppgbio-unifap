source("scripts/_bootstrap.R")

# ============================================================
# Unidade 4 - Multicolinearidade em SDM
# Script 02: Preparar matriz ambiental real de Dinizia excelsa
# ============================================================

# ------------------------------------------------------------
# 1. Pacotes
# ------------------------------------------------------------

pacotes <- c("dplyr", "readr", "tidyr", "terra", "tibble", "stringr")

require_packages(pacotes)
invisible(lapply(pacotes, library, character.only = TRUE))

# ------------------------------------------------------------
# 2. Diretório do projeto
# ------------------------------------------------------------
# ------------------------------------------------------------
# 3. Arquivos de entrada da Unidade 3
# ------------------------------------------------------------

arquivo_oc <- "dados/unidade03/processados/ocorrencias_dinizia_variaveis_ambientais.csv"
arquivo_bg <- "dados/unidade03/processados/background_ambiental_amazonia_unidade03.csv"
arquivo_raster <- "dados/unidade03/processados/variaveis_ambientais_amazonia_unidade03.tif"

if (!file.exists(arquivo_oc)) stop("Arquivo de ocorrências ambientais da Unidade 3 não encontrado.")
if (!file.exists(arquivo_bg)) stop("Arquivo de background ambiental da Unidade 3 não encontrado.")
if (!file.exists(arquivo_raster)) stop("Raster ambiental da Unidade 3 não encontrado.")

# ------------------------------------------------------------
# 4. Leitura dos dados
# ------------------------------------------------------------

oc <- read_csv(arquivo_oc, show_col_types = FALSE)
bg <- read_csv(arquivo_bg, show_col_types = FALSE)
amb_stack <- terra::rast(arquivo_raster)

# ------------------------------------------------------------
# 5. Identificar variáveis ambientais reais pelo raster
# ------------------------------------------------------------

vars_raster <- names(amb_stack)

vars_bg <- names(bg)
vars_oc <- names(oc)

vars_ambientais <- vars_raster[
  vars_raster %in% vars_bg &
    vars_raster %in% vars_oc
]

if (length(vars_ambientais) == 0) {
  stop("Nenhuma variável ambiental do raster foi encontrada simultaneamente nos arquivos de ocorrência e background.")
}

# ------------------------------------------------------------
# 6. Verificações de consistência
# ------------------------------------------------------------

vars_ausentes_bg <- setdiff(vars_raster, vars_bg)
vars_ausentes_oc <- setdiff(vars_raster, vars_oc)

if (length(vars_ausentes_bg) > 0) {
  warning(
    "As seguintes variáveis do raster não estão no background: ",
    paste(vars_ausentes_bg, collapse = ", ")
  )
}

if (length(vars_ausentes_oc) > 0) {
  warning(
    "As seguintes variáveis do raster não estão nas ocorrências: ",
    paste(vars_ausentes_oc, collapse = ", ")
  )
}

# ------------------------------------------------------------
# 7. Preparar matriz ambiental do background
# ------------------------------------------------------------

bg_vars <- bg %>%
  dplyr::select(all_of(vars_ambientais)) %>%
  dplyr::mutate(across(everything(), as.numeric)) %>%
  tidyr::drop_na()

# ------------------------------------------------------------
# 8. Preparar matriz ambiental das ocorrências
# ------------------------------------------------------------

colunas_identificacao <- c("especie", "especie_padrao", "lon", "lat")

oc_vars <- oc %>%
  dplyr::select(any_of(colunas_identificacao), all_of(vars_ambientais)) %>%
  dplyr::mutate(across(all_of(vars_ambientais), as.numeric)) %>%
  tidyr::drop_na(all_of(vars_ambientais))

# ------------------------------------------------------------
# 9. Salvar produtos intermediários
# ------------------------------------------------------------

write_csv(
  bg_vars,
  "dados/unidade04/processados/02_background_variaveis_ambientais.csv"
)

write_csv(
  oc_vars,
  "dados/unidade04/processados/02_ocorrencias_variaveis_ambientais.csv"
)

# ------------------------------------------------------------
# 10. Salvar lista de variáveis disponíveis antes do VIF
# ------------------------------------------------------------

tabela_variaveis_disponiveis <- tibble(
  ordem = seq_along(vars_ambientais),
  variavel = vars_ambientais
)

write_csv(
  tabela_variaveis_disponiveis,
  "tabelas/unidade04/02_variaveis_ambientais_disponiveis_pre_vif.csv"
)

# ------------------------------------------------------------
# 11. Resumo da preparação
# ------------------------------------------------------------

resumo <- tibble(
  produto = c(
    "Camadas ambientais no raster da Unidade 3",
    "Variáveis ambientais comuns ao raster, background e ocorrências",
    "Variáveis do raster ausentes no background",
    "Variáveis do raster ausentes nas ocorrências",
    "Pixels/background disponíveis após remoção de NA",
    "Ocorrências com valores ambientais após remoção de NA"
  ),
  n = c(
    terra::nlyr(amb_stack),
    length(vars_ambientais),
    length(vars_ausentes_bg),
    length(vars_ausentes_oc),
    nrow(bg_vars),
    nrow(oc_vars)
  )
)

write_csv(
  resumo,
  "tabelas/unidade04/resumo_preparacao_variaveis.csv"
)

# ------------------------------------------------------------
# 12. Mensagens finais
# ------------------------------------------------------------

message("Matriz ambiental real preparada para análise de multicolinearidade.")
message("Variáveis ambientais disponíveis antes do VIF:")

print(tabela_variaveis_disponiveis)
print(resumo)
