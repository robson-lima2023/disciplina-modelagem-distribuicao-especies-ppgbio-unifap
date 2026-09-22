source("scripts/_bootstrap.R")

# ============================================================
# Unidade 4 - Multicolinearidade em SDM
# Script 04: VIF das variáveis ambientais
# ============================================================

# ------------------------------------------------------------
# 1. Pacotes
# ------------------------------------------------------------

pacotes <- c("dplyr", "readr", "ggplot2", "usdm", "tibble", "tidyr")

require_packages(pacotes)
invisible(lapply(pacotes, library, character.only = TRUE))

# ------------------------------------------------------------
# 2. Diretório do projeto
# ------------------------------------------------------------
# ------------------------------------------------------------
# 3. Ler matriz ambiental preparada no Script 02
# ------------------------------------------------------------

arquivo_bg <- "dados/unidade04/processados/02_background_variaveis_ambientais.csv"

if (!file.exists(arquivo_bg)) {
  stop("Arquivo 02_background_variaveis_ambientais.csv não encontrado. Execute primeiro o Script 02 da Unidade 4.")
}

bg_vars <- read_csv(arquivo_bg, show_col_types = FALSE)

# ------------------------------------------------------------
# 4. Garantir apenas variáveis numéricas
# ------------------------------------------------------------

bg_num <- bg_vars %>%
  dplyr::select(where(is.numeric)) %>%
  tidyr::drop_na()

if (ncol(bg_num) < 2) {
  stop("A matriz ambiental possui menos de duas variáveis numéricas para cálculo do VIF.")
}

# ------------------------------------------------------------
# 5. Amostragem do background para reduzir custo computacional
# ------------------------------------------------------------

set.seed(123)

n_amostra <- min(10000, nrow(bg_num))

bg_vif <- bg_num %>%
  dplyr::sample_n(size = n_amostra) %>%
  as.data.frame()

# ------------------------------------------------------------
# 6. VIF inicial
# ------------------------------------------------------------

vif_inicial <- usdm::vif(bg_vif)

vif_inicial_tab <- as.data.frame(vif_inicial) %>%
  dplyr::arrange(desc(VIF))

write_csv(
  vif_inicial_tab,
  "tabelas/unidade04/04_vif_inicial_variaveis.csv"
)

# ------------------------------------------------------------
# 7. Seleção stepwise por VIF
# ------------------------------------------------------------

limiar_vif <- 10

vif_step <- usdm::vifstep(
  bg_vif,
  th = limiar_vif
)

vif_final_tab <- as.data.frame(vif_step@results) %>%
  dplyr::arrange(desc(VIF))

write_csv(
  vif_final_tab,
  "tabelas/unidade04/04_vif_final_variaveis.csv"
)

# ------------------------------------------------------------
# 8. Variáveis mantidas após VIF
# ------------------------------------------------------------

variaveis_vif_mantidas <- tibble(
  ordem = seq_along(vif_step@results$Variables),
  variavel = vif_step@results$Variables
)

write_csv(
  variaveis_vif_mantidas,
  "resultados/unidade04/variaveis_mantidas_vif.csv"
)

write_csv(
  variaveis_vif_mantidas,
  "dados/unidade04/processados/variaveis_selecionadas_final.csv"
)

# ------------------------------------------------------------
# 9. Variáveis removidas pelo VIF
# ------------------------------------------------------------

variaveis_vif_removidas <- tibble(
  variavel = setdiff(colnames(bg_vif), variaveis_vif_mantidas$variavel)
)

write_csv(
  variaveis_vif_removidas,
  "resultados/unidade04/variaveis_removidas_vif.csv"
)

# ------------------------------------------------------------
# 10. Resumo da seleção
# ------------------------------------------------------------

resumo_vif <- tibble(
  criterio = c(
    "Número de variáveis ambientais antes do VIF",
    "Número de variáveis ambientais mantidas após VIF",
    "Número de variáveis ambientais removidas pelo VIF",
    "Limiar de VIF adotado",
    "Número de pixels/background amostrados para VIF"
  ),
  valor = c(
    ncol(bg_vif),
    nrow(variaveis_vif_mantidas),
    nrow(variaveis_vif_removidas),
    limiar_vif,
    n_amostra
  )
)

write_csv(
  resumo_vif,
  "tabelas/unidade04/04_resumo_selecao_vif.csv"
)

# ------------------------------------------------------------
# 11. Gráfico do VIF inicial
# ------------------------------------------------------------

g_vif_inicial <- ggplot(
  vif_inicial_tab,
  aes(x = reorder(Variables, VIF), y = VIF)
) +
  geom_col() +
  coord_flip() +
  geom_hline(
    yintercept = limiar_vif,
    linetype = "dashed",
    linewidth = 0.6
  ) +
  labs(
    title = "Fator de Inflação da Variância antes da seleção",
    subtitle = paste0("Linha tracejada indica VIF = ", limiar_vif),
    x = NULL,
    y = "VIF"
  ) +
  theme_bw(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave(
  "figuras/unidade04/unidade04_vif_inicial_variaveis.png",
  plot = g_vif_inicial,
  width = 8,
  height = 6,
  dpi = 600
)

# ------------------------------------------------------------
# 12. Gráfico do VIF final
# ------------------------------------------------------------

g_vif_final <- ggplot(
  vif_final_tab,
  aes(x = reorder(Variables, VIF), y = VIF)
) +
  geom_col() +
  coord_flip() +
  geom_hline(
    yintercept = limiar_vif,
    linetype = "dashed",
    linewidth = 0.6
  ) +
  labs(
    title = "Fator de Inflação da Variância após seleção",
    subtitle = paste0("Variáveis mantidas com VIF inferior ao limiar ", limiar_vif),
    x = NULL,
    y = "VIF"
  ) +
  theme_bw(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave(
  "figuras/unidade04/unidade04_vif_final_variaveis.png",
  plot = g_vif_final,
  width = 8,
  height = 6,
  dpi = 600
)

# ------------------------------------------------------------
# 13. Mensagens finais
# ------------------------------------------------------------

message("Seleção de variáveis por VIF concluída com sucesso.")
message("Resumo da seleção:")

print(resumo_vif)

message("Variáveis mantidas após VIF:")

print(variaveis_vif_mantidas)

message("Variáveis removidas pelo VIF:")

print(variaveis_vif_removidas)
