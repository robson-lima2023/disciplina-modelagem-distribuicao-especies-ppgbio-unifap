# ============================================================
# Unidade 17 - Projeto Integrador
# Script 03: Área M e background
# ============================================================

pacotes <- c("dplyr", "readr", "ggplot2")

instalar <- pacotes[!pacotes %in% rownames(installed.packages())]
if (length(instalar) > 0) install.packages(instalar)

invisible(lapply(pacotes, library, character.only = TRUE))

dados <- read_csv(
  "dados/unidade17/processados/dados_modelo_integrador.csv",
  show_col_types = FALSE
)

area_m <- data.frame(
  xmin = -80,
  xmax = -40,
  ymin = -30,
  ymax = 10,
  criterio = "Área M didática definida pela extensão ambiental simulada"
)

write_csv(area_m, "tabelas/unidade17/area_m_definicao.csv")

resumo <- dados %>%
  count(pa) %>%
  mutate(classe = ifelse(pa == 1, "Presença", "Background"))

write_csv(resumo, "tabelas/unidade17/resumo_presenca_background.csv")

message("Área M e background documentados.")
print(resumo)
