# ============================================================
# Unidade 16 - Aplicações à Conservação
# Script 02: Índice de áreas prioritárias
# ============================================================

pacotes <- c("dplyr", "readr", "ggplot2")

instalar <- pacotes[!pacotes %in% rownames(installed.packages())]
if (length(instalar) > 0) install.packages(instalar)

invisible(lapply(pacotes, library, character.only = TRUE))

camadas <- read_csv(
  "dados/unidade16/processados/camadas_conservacao.csv",
  show_col_types = FALSE
)

normalizar <- function(x) {
  (x - min(x, na.rm = TRUE)) /
    (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

prioridades <- camadas %>%
  mutate(
    valor_biologico =
      0.30 * riqueza_potencial +
      0.25 * especies_ameacadas +
      0.20 * arvores_gigantes +
      0.15 * biomassa_potencial +
      0.10 * adequabilidade_futura,
    
    confiabilidade = 1 - incerteza,
    
    prioridade_bruta =
      valor_biologico * confiabilidade,
    
    prioridade_custo =
      prioridade_bruta / (custo + 0.05),
    
    prioridade_final = normalizar(prioridade_custo)
  )

write_csv(
  prioridades,
  "dados/unidade16/processados/areas_prioritarias.csv"
)

g <- ggplot(
  prioridades,
  aes(x = lon, y = lat, fill = prioridade_final)
) +
  geom_raster() +
  coord_equal() +
  scale_fill_viridis_c(name = "Prioridade") +
  labs(
    title = "Áreas prioritárias para conservação",
    subtitle = "Índice multicritério com valor biológico, incerteza e custo",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid = element_blank()
  )

ggsave(
  "figuras/unidade16/unidade16_areas_prioritarias.png",
  plot = g,
  width = 8,
  height = 6,
  dpi = 600
)

message("Índice de áreas prioritárias gerado.")
