# ============================================================
# Unidade 13 - Projeções Climáticas Futuras em SDM
# Script 05: Ganho, perda e estabilidade
# ============================================================

pacotes <- c("dplyr", "readr", "ggplot2")

instalar <- pacotes[!pacotes %in% rownames(installed.packages())]
if (length(instalar) > 0) install.packages(instalar)

invisible(lapply(pacotes, library, character.only = TRUE))

mudancas <- read_csv(
  "dados/unidade13/processados/mudancas_adequabilidade_ssp.csv",
  show_col_types = FALSE
)

limiar_delta <- 0.10

classes <- mudancas %>%
  mutate(
    classe = case_when(
      delta_adequabilidade <= -limiar_delta ~ "Perda",
      delta_adequabilidade >= limiar_delta ~ "Ganho",
      TRUE ~ "Estabilidade"
    )
  )

write_csv(
  classes,
  "dados/unidade13/processados/classes_ganho_perda_estabilidade.csv"
)

resumo <- classes %>%
  count(periodo, classe) %>%
  group_by(periodo) %>%
  mutate(
    proporcao = n / sum(n)
  ) %>%
  ungroup()

write_csv(
  resumo,
  "tabelas/unidade13/resumo_ganho_perda_estabilidade.csv"
)

g <- ggplot(
  classes,
  aes(x = lon, y = lat, fill = classe)
) +
  geom_raster() +
  coord_equal() +
  facet_wrap(~ periodo, ncol = 2) +
  labs(
    title = "Ganho, perda e estabilidade de adequabilidade",
    subtitle = "Classificação baseada em mudança mínima absoluta de 0,10",
    x = "Longitude",
    y = "Latitude",
    fill = "Classe"
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid = element_blank()
  )

ggsave(
  "figuras/unidade13/unidade13_ganho_perda_estabilidade.png",
  plot = g,
  width = 10,
  height = 7,
  dpi = 600
)

message("Classes de ganho, perda e estabilidade geradas.")
print(resumo)
