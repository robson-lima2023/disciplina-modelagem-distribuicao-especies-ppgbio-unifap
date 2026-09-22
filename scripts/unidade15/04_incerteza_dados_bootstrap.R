# ============================================================
# Unidade 15 - Incertezas em SDM
# Script 04: Incerteza associada aos dados por bootstrap
# ============================================================

pacotes <- c("dplyr", "readr", "ggplot2", "purrr")

instalar <- pacotes[!pacotes %in% rownames(installed.packages())]
if (length(instalar) > 0) install.packages(instalar)

invisible(lapply(pacotes, library, character.only = TRUE))

base <- read_csv(
  "dados/unidade15/processados/predicoes_algoritmos_simuladas.csv",
  show_col_types = FALSE
)

set.seed(123)

# ------------------------------------------------------------
# Simulação didática:
# cada bootstrap representa uma variação possível nos dados de
# ocorrência, produzindo pequenas alterações espaciais na predição.
# ------------------------------------------------------------

n_boot <- 30

boot_preds <- map_dfc(1:n_boot, function(i) {
  ruido <- rnorm(nrow(base), 0, 0.04)
  deslocamento <- 0.05 * sin((base$lon + i) / 6) +
    0.04 * cos((base$lat - i) / 5)
  
  pred <- pmin(pmax(base$glm + ruido + deslocamento, 0), 1)
  
  tibble(!!paste0("boot_", i) := pred)
})

inc_dados <- bind_cols(
  base %>% select(lon, lat),
  boot_preds
) %>%
  mutate(
    media_bootstrap = rowMeans(select(., starts_with("boot_"))),
    incerteza_dados = apply(select(., starts_with("boot_")), 1, sd)
  )

write_csv(
  inc_dados,
  "dados/unidade15/processados/incerteza_dados_bootstrap.csv"
)

g <- ggplot(
  inc_dados,
  aes(x = lon, y = lat, fill = incerteza_dados)
) +
  geom_raster() +
  coord_equal() +
  scale_fill_viridis_c(name = "SD") +
  labs(
    title = "Incerteza associada aos dados",
    subtitle = "Simulação por reamostragens bootstrap",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid = element_blank()
  )

ggsave(
  "figuras/unidade15/unidade15_incerteza_dados.png",
  plot = g,
  width = 8,
  height = 6,
  dpi = 600
)

message("Incerteza dos dados por bootstrap calculada.")

