# ============================================================
# Unidade 13 - Projeções Climáticas Futuras em SDM
# Script 04: Mudanças de adequabilidade
# ============================================================

pacotes <- c("dplyr", "readr", "ggplot2", "tidyr")

instalar <- pacotes[!pacotes %in% rownames(installed.packages())]
if (length(instalar) > 0) install.packages(instalar)

invisible(lapply(pacotes, library, character.only = TRUE))

predicoes <- read_csv(
  "dados/unidade13/processados/predicoes_adequabilidade_cenarios.csv",
  show_col_types = FALSE
)

atual <- predicoes %>%
  filter(periodo == "Atual") %>%
  select(lon, lat, adequabilidade_atual = adequabilidade)

mudancas <- predicoes %>%
  filter(periodo != "Atual") %>%
  left_join(atual, by = c("lon", "lat")) %>%
  mutate(
    delta_adequabilidade = adequabilidade - adequabilidade_atual
  )

write_csv(
  mudancas,
  "dados/unidade13/processados/mudancas_adequabilidade_ssp.csv"
)

g <- ggplot(
  mudancas,
  aes(x = lon, y = lat, fill = delta_adequabilidade)
) +
  geom_raster() +
  coord_equal() +
  facet_wrap(~ periodo, ncol = 2) +
  scale_fill_gradient2(
    name = expression(Delta~adequabilidade),
    midpoint = 0
  ) +
  labs(
    title = "Mudanças de adequabilidade ambiental",
    subtitle = "Diferença entre cenário futuro e clima atual",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid = element_blank()
  )

ggsave(
  "figuras/unidade13/unidade13_mudancas_adequabilidade.png",
  plot = g,
  width = 10,
  height = 7,
  dpi = 600
)

message("Mudanças de adequabilidade calculadas.")
