# ============================================================
# Unidade 4 - Multicolinearidade em SDM
# Script 07: Gerar raster ambiental com variáveis selecionadas
# ============================================================

# ------------------------------------------------------------
# 1. Pacotes
# ------------------------------------------------------------

pacotes <- c("terra", "readr", "dplyr", "tibble")

instalar <- pacotes[!pacotes %in% rownames(installed.packages())]
if (length(instalar) > 0) install.packages(instalar)

invisible(lapply(pacotes, library, character.only = TRUE))

# ------------------------------------------------------------
# 2. Diretório do projeto
# ------------------------------------------------------------

setwd("C:/Users/rblfl/OneDrive/Documentos/Playground/Species-Distribution-Modeling")

# ------------------------------------------------------------
# 3. Arquivos de entrada e saída
# ------------------------------------------------------------

arquivo_raster_entrada <- "dados/unidade03/processados/variaveis_ambientais_amazonia_unidade03.tif"

arquivo_vars <- "dados/unidade04/processados/variaveis_selecionadas_final.csv"

arquivo_raster_saida <- "dados/unidade04/processados/variaveis_ambientais_selecionadas_bioma.tif"

if (!file.exists(arquivo_raster_entrada)) {
  stop("Raster ambiental da Unidade 03 não encontrado: ", arquivo_raster_entrada)
}

if (!file.exists(arquivo_vars)) {
  stop("Arquivo de variáveis selecionadas não encontrado: ", arquivo_vars)
}

# ------------------------------------------------------------
# 4. Ler raster ambiental e variáveis finais
# ------------------------------------------------------------

r <- terra::rast(arquivo_raster_entrada)

vars_tab <- readr::read_csv(
  arquivo_vars,
  show_col_types = FALSE
)

if (!"variavel" %in% names(vars_tab)) {
  stop("O arquivo variaveis_selecionadas_final.csv precisa conter a coluna 'variavel'.")
}

vars <- vars_tab$variavel

# ------------------------------------------------------------
# 5. Verificar compatibilidade entre CSV e raster
# ------------------------------------------------------------

vars_ausentes <- setdiff(vars, names(r))

if (length(vars_ausentes) > 0) {
  stop(
    "As seguintes variáveis selecionadas não estão no raster da Unidade 03: ",
    paste(vars_ausentes, collapse = ", ")
  )
}

# ------------------------------------------------------------
# 6. Selecionar camadas finais
# ------------------------------------------------------------

r_sel <- r[[vars]]

if (terra::nlyr(r_sel) != length(vars)) {
  stop("O número de camadas selecionadas não coincide com o número de variáveis finais.")
}

# ------------------------------------------------------------
# 7. Salvar raster ambiental final
# ------------------------------------------------------------

terra::writeRaster(
  r_sel,
  arquivo_raster_saida,
  overwrite = TRUE
)

# ------------------------------------------------------------
# 8. Salvar resumo
# ------------------------------------------------------------

resumo <- tibble::tibble(
  produto = c(
    "Camadas no raster ambiental original",
    "Variáveis selecionadas no CSV final",
    "Camadas no raster ambiental selecionado"
  ),
  n = c(
    terra::nlyr(r),
    length(vars),
    terra::nlyr(r_sel)
  )
)

readr::write_csv(
  resumo,
  "tabelas/unidade04/07_resumo_raster_variaveis_selecionadas.csv"
)

variaveis_raster_final <- tibble::tibble(
  ordem = seq_along(names(r_sel)),
  variavel = names(r_sel)
)

readr::write_csv(
  variaveis_raster_final,
  "tabelas/unidade04/07_variaveis_no_raster_final.csv"
)

# ------------------------------------------------------------
# 9. Mensagens finais
# ------------------------------------------------------------

message("Raster ambiental selecionado criado com sucesso.")
message("Arquivo salvo em: ", arquivo_raster_saida)
message("Número de camadas no raster final: ", terra::nlyr(r_sel))
message("Variáveis no raster final: ", paste(names(r_sel), collapse = ", "))

print(resumo)
print(variaveis_raster_final)
