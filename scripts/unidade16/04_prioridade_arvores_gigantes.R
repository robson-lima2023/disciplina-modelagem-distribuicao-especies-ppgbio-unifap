# ============================================================
# Unidade 16 - Aplicações à Conservação
# Script 04: Prioridade funcional para árvores gigantes
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

gigantes <- camadas %>%
  mutate(
    estabilidade_climatica =
      1 - abs(adequabilidade_futura - adequabilidade_atual),
    
    prioridade_gigantes =
      0.40 * arvores_gigantes +
      0.25 * biomassa_potencial +
      0.20 * estabilidade_climatica +
      0.15 * (1 - incerteza),
    
    prioridade_gigantes = normalizar(prioridade_gigantes)
  )

write_csv(
  gigantes,
  "dados/unidade16/processados/prioridade_arvores_gigantes.csv"
)

g <- ggplot(
  gigantes,
  aes(x = lon, y = lat, fill = prioridade_gigantes)
) +
  geom_raster() +
  coord_equal() +
  scale_fill_viridis_c(name = "Prioridade") +
  labs(
    title = "Prioridade funcional para árvores gigantes",
    subtitle = "Adequabilidade, biomassa, estabilidade climática e baixa incerteza",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid = element_blank()
  )

ggsave(
  "figuras/unidade16/unidade16_prioridade_arvores_gigantes.png",
  plot = g,
  width = 8,
  height = 6,
  dpi = 600
)

message("Prioridade funcional para árvores gigantes gerada.")
