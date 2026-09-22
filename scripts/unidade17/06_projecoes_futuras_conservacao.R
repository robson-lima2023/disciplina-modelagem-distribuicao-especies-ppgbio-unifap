# ============================================================
# Unidade 17 - Projeto Integrador
# Script 06: Projeções futuras e conservação
# ============================================================

pacotes <- c("dplyr", "readr", "ggplot2", "tidyr")

instalar <- pacotes[!pacotes %in% rownames(installed.packages())]
if (length(instalar) > 0) install.packages(instalar)

invisible(lapply(pacotes, library, character.only = TRUE))

pred <- read_csv(
  "dados/unidade17/processados/predicoes_ensemble_integrador.csv",
  show_col_types = FALSE
)

set.seed(123)

futuro <- pred %>%
  transmute(
    lon,
    lat,
    Atual = ensemble,
    SSP126 = pmin(pmax(ensemble * 0.98 + 0.03 * sin(lat / 5), 0), 1),
    SSP245 = pmin(pmax(ensemble * 0.92 + 0.04 * sin(lat / 5), 0), 1),
    SSP370 = pmin(pmax(ensemble * 0.84 + 0.06 * sin(lat / 5), 0), 1),
    SSP585 = pmin(pmax(ensemble * 0.72 + 0.08 * sin(lat / 5), 0), 1)
  )

mudancas <- futuro %>%
  pivot_longer(
    cols = c(SSP126, SSP245, SSP370, SSP585),
    names_to = "cenario",
    values_to = "adequabilidade_futura"
  ) %>%
  mutate(
    delta = adequabilidade_futura - Atual,
    classe = case_when(
      delta <= -0.10 ~ "Perda",
      delta >= 0.10 ~ "Ganho",
      TRUE ~ "Estabilidade"
    )
  )

write_csv(mudancas, "dados/unidade17/processados/mudancas_futuras_integrador.csv")

g <- ggplot(mudancas, aes(x = lon, y = lat, fill = delta)) +
  geom_raster() +
  coord_equal() +
  facet_wrap(~ cenario, ncol = 2) +
  scale_fill_gradient2(name = expression(Delta), midpoint = 0) +
  labs(
    title = "Mudanças futuras de adequabilidade",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_bw(base_size = 12) +
  theme(plot.title = element_text(face = "bold"), panel.grid = element_blank())

ggsave(
  "figuras/unidade17/unidade17_mudancas_futuras.png",
  plot = g,
  width = 10,
  height = 7,
  dpi = 600
)

resumo <- mudancas %>%
  count(cenario, classe) %>%
  group_by(cenario) %>%
  mutate(proporcao = n / sum(n)) %>%
  ungroup()

write_csv(resumo, "tabelas/unidade17/resumo_mudancas_futuras.csv")

message("Projeções futuras e mudanças calculadas.")
print(resumo)
