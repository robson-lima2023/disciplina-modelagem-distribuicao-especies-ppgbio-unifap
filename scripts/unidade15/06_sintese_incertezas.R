# ============================================================
# Unidade 15 - Incertezas em SDM
# Script 06: Síntese integrada das incertezas
# ============================================================

pacotes <- c("dplyr", "readr", "ggplot2", "tidyr")

instalar <- pacotes[!pacotes %in% rownames(installed.packages())]
if (length(instalar) > 0) install.packages(instalar)

invisible(lapply(pacotes, library, character.only = TRUE))

inc_alg <- read_csv(
  "dados/unidade15/processados/incerteza_algoritmos.csv",
  show_col_types = FALSE
) %>%
  select(lon, lat, consenso_medio, incerteza_algoritmos = incerteza_sd)

inc_cen <- read_csv(
  "dados/unidade15/processados/incerteza_cenarios_ssp.csv",
  show_col_types = FALSE
) %>%
  select(lon, lat, incerteza_cenarios)

inc_dados <- read_csv(
  "dados/unidade15/processados/incerteza_dados_bootstrap.csv",
  show_col_types = FALSE
) %>%
  select(lon, lat, incerteza_dados)

inc_escala <- read_csv(
  "dados/unidade15/processados/incerteza_escala.csv",
  show_col_types = FALSE
) %>%
  select(lon, lat, incerteza_escala = diferenca_escala)

sintese <- inc_alg %>%
  left_join(inc_cen, by = c("lon", "lat")) %>%
  left_join(inc_dados, by = c("lon", "lat")) %>%
  left_join(inc_escala, by = c("lon", "lat"))

normalizar <- function(x) {
  (x - min(x, na.rm = TRUE)) /
    (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

sintese <- sintese %>%
  mutate(
    inc_alg_n = normalizar(incerteza_algoritmos),
    inc_cen_n = normalizar(incerteza_cenarios),
    inc_dados_n = normalizar(incerteza_dados),
    inc_escala_n = normalizar(incerteza_escala)
  )

sintese$incerteza_integrada <- rowMeans(
  sintese[, c("inc_alg_n", "inc_cen_n", "inc_dados_n", "inc_escala_n")],
  na.rm = TRUE
)

write_csv(
  sintese,
  "dados/unidade15/processados/sintese_incertezas.csv"
)

plot_df <- sintese %>%
  select(
    lon, lat,
    Algoritmos = inc_alg_n,
    Cenarios = inc_cen_n,
    Dados = inc_dados_n,
    Escala = inc_escala_n,
    Integrada = incerteza_integrada
  ) %>%
  pivot_longer(
    cols = c(Algoritmos, Cenarios, Dados, Escala, Integrada),
    names_to = "fonte",
    values_to = "incerteza"
  )

g <- ggplot(
  plot_df,
  aes(x = lon, y = lat, fill = incerteza)
) +
  geom_raster() +
  coord_equal() +
  facet_wrap(~ fonte, ncol = 3) +
  scale_fill_viridis_c(name = "Incerteza") +
  labs(
    title = "Síntese integrada de incertezas em SDM",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid = element_blank()
  )

ggsave(
  "figuras/unidade15/unidade15_sintese_incertezas.png",
  plot = g,
  width = 11,
  height = 7,
  dpi = 600
)

message("Síntese integrada de incertezas concluída.")
