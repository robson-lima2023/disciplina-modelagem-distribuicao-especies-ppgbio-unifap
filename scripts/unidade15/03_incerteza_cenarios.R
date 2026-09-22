# ============================================================
# Unidade 15 - Incertezas em SDM
# Script 03: Incerteza entre cenários SSP
# ============================================================

pacotes <- c("dplyr", "readr", "ggplot2", "tidyr")

instalar <- pacotes[!pacotes %in% rownames(installed.packages())]
if (length(instalar) > 0) install.packages(instalar)

invisible(lapply(pacotes, library, character.only = TRUE))

base <- read_csv(
  "dados/unidade15/processados/predicoes_algoritmos_simuladas.csv",
  show_col_types = FALSE
)

set.seed(123)

cenarios <- base %>%
  transmute(
    lon,
    lat,
    SSP126 = glm * 0.98 + rnorm(n(), 0, 0.015),
    SSP245 = glm * 0.93 + 0.03 * sin(lat / 5) + rnorm(n(), 0, 0.020),
    SSP370 = glm * 0.85 + 0.06 * sin(lat / 5) + rnorm(n(), 0, 0.025),
    SSP585 = glm * 0.75 + 0.10 * sin(lat / 5) + rnorm(n(), 0, 0.030)
  ) %>%
  mutate(
    across(
      c(SSP126, SSP245, SSP370, SSP585),
      ~ pmin(pmax(.x, 0), 1)
    )
  )

inc_cen <- cenarios %>%
  mutate(
    media_cenarios = rowMeans(select(., SSP126, SSP245, SSP370, SSP585)),
    incerteza_cenarios = apply(select(., SSP126, SSP245, SSP370, SSP585), 1, sd)
  )

write_csv(
  inc_cen,
  "dados/unidade15/processados/incerteza_cenarios_ssp.csv"
)

g <- ggplot(
  inc_cen,
  aes(x = lon, y = lat, fill = incerteza_cenarios)
) +
  geom_raster() +
  coord_equal() +
  scale_fill_viridis_c(name = "SD") +
  labs(
    title = "Incerteza entre cenários SSP",
    subtitle = "Desvio-padrão das projeções futuras simuladas",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid = element_blank()
  )

ggsave(
  "figuras/unidade15/unidade15_incerteza_cenarios.png",
  plot = g,
  width = 8,
  height = 6,
  dpi = 600
)

message("Incerteza entre cenários calculada.")
