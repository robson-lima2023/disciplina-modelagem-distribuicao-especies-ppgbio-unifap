# ============================================================
# Unidade 16 - Aplicações à Conservação
# Script 03: Refúgios climáticos potenciais
# ============================================================

pacotes <- c("dplyr", "readr", "ggplot2")

instalar <- pacotes[!pacotes %in% rownames(installed.packages())]
if (length(instalar) > 0) install.packages(instalar)

invisible(lapply(pacotes, library, character.only = TRUE))

camadas <- read_csv(
  "dados/unidade16/processados/camadas_conservacao.csv",
  show_col_types = FALSE
)

refugios <- camadas %>%
  mutate(
    estabilidade = 1 - abs(adequabilidade_futura - adequabilidade_atual),
    baixa_incerteza = 1 - incerteza,
    refugio_indice =
      adequabilidade_atual *
      adequabilidade_futura *
      estabilidade *
      baixa_incerteza,
    refugio = refugio_indice >= quantile(refugio_indice, 0.90, na.rm = TRUE)
  )

write_csv(
  refugios,
  "dados/unidade16/processados/refugios_climaticos.csv"
)

g <- ggplot(
  refugios,
  aes(x = lon, y = lat, fill = refugio_indice)
) +
  geom_raster() +
  geom_contour(
    aes(z = as.numeric(refugio)),
    breaks = 0.5,
    color = "black",
    linewidth = 0.4
  ) +
  coord_equal() +
  scale_fill_viridis_c(name = "Índice") +
  labs(
    title = "Refúgios climáticos potenciais",
    subtitle = "Contorno preto indica os 10% maiores valores",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid = element_blank()
  )

ggsave(
  "figuras/unidade16/unidade16_refugios_climaticos.png",
  plot = g,
  width = 8,
  height = 6,
  dpi = 600
)

message("Refúgios climáticos potenciais identificados.")
