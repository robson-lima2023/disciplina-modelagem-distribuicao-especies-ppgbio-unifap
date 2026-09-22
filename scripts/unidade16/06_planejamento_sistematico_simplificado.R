# ============================================================
# Unidade 16 - Aplicações à Conservação
# Script 06: Planejamento sistemático simplificado
# ============================================================

pacotes <- c("dplyr", "readr", "ggplot2")

instalar <- pacotes[!pacotes %in% rownames(installed.packages())]
if (length(instalar) > 0) install.packages(instalar)

invisible(lapply(pacotes, library, character.only = TRUE))

prioridades <- read_csv(
  "dados/unidade16/processados/areas_prioritarias.csv",
  show_col_types = FALSE
)

# ------------------------------------------------------------
# Seleção sob orçamento:
# selecionamos células com maior benefício por custo até atingir
# 10% do custo total da paisagem.
# ------------------------------------------------------------

orcamento <- 0.10 * sum(prioridades$custo, na.rm = TRUE)

selecao <- prioridades %>%
  mutate(
    beneficio = prioridade_final,
    eficiencia = beneficio / (custo + 0.05)
  ) %>%
  arrange(desc(eficiencia)) %>%
  mutate(
    custo_acumulado = cumsum(custo),
    selecionada = custo_acumulado <= orcamento
  ) %>%
  arrange(lon, lat)

write_csv(
  selecao,
  "dados/unidade16/processados/planejamento_sistematico_simplificado.csv"
)

resumo <- selecao %>%
  summarise(
    custo_total = sum(custo),
    orcamento = orcamento,
    custo_selecionado = sum(custo[selecionada]),
    n_celulas_selecionadas = sum(selecionada),
    beneficio_medio_selecionado = mean(beneficio[selecionada])
  )

write_csv(
  resumo,
  "tabelas/unidade16/resumo_planejamento_sistematico.csv"
)

g <- ggplot(
  selecao,
  aes(x = lon, y = lat, fill = selecionada)
) +
  geom_raster() +
  coord_equal() +
  labs(
    title = "Planejamento sistemático simplificado",
    subtitle = "Seleção de células sob orçamento de 10% do custo total",
    x = "Longitude",
    y = "Latitude",
    fill = "Selecionada"
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid = element_blank()
  )

ggsave(
  "figuras/unidade16/unidade16_planejamento_sistematico.png",
  plot = g,
  width = 8,
  height = 6,
  dpi = 600
)

message("Planejamento sistemático simplificado concluído.")
print(resumo)
