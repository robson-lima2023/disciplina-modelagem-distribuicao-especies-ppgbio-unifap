# ============================================================
# Unidade 12 - Avaliação de modelos em SDM
# Script 03: Curvas de threshold
# ============================================================

pacotes <- c("dplyr", "readr", "ggplot2", "tidyr")

instalar <- pacotes[!pacotes %in% rownames(installed.packages())]
if (length(instalar) > 0) install.packages(instalar)

invisible(lapply(pacotes, library, character.only = TRUE))

metricas_threshold <- read_csv(
  "tabelas/unidade12/metricas_por_threshold.csv",
  show_col_types = FALSE
)

# Para visualização didática, usamos o ensemble.
curvas <- metricas_threshold %>%
  filter(modelo == "ensemble") %>%
  select(threshold, sensibilidade, especificidade, TSS) %>%
  pivot_longer(
    cols = c(sensibilidade, especificidade, TSS),
    names_to = "metrica",
    values_to = "valor"
  )

g_curvas <- ggplot(
  curvas,
  aes(x = threshold, y = valor, linetype = metrica)
) +
  geom_line(linewidth = 1) +
  labs(
    title = "Efeito do limiar sobre métricas de classificação",
    subtitle = "Exemplo usando o modelo ensemble",
    x = "Limiar de corte",
    y = "Valor da métrica",
    linetype = "Métrica"
  ) +
  theme_bw(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

ggsave(
  "figuras/unidade12/unidade12_curvas_threshold.png",
  plot = g_curvas,
  width = 8,
  height = 5,
  dpi = 600
)

message("Curvas de threshold geradas.")
