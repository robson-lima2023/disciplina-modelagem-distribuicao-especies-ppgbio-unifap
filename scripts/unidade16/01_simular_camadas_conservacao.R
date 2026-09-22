# ============================================================
# Unidade 16 - Aplicações à Conservação
# Script 01: Simular camadas espaciais de conservação
# ============================================================

setwd("C:/Users/rblfl/OneDrive/Documentos/Playground/Species-Distribution-Modeling")

pacotes <- c("dplyr", "readr", "ggplot2", "terra", "tidyr")

instalar <- pacotes[!pacotes %in% rownames(installed.packages())]
if (length(instalar) > 0) install.packages(instalar)

invisible(lapply(pacotes, library, character.only = TRUE))

dir.create("dados/unidade16/processados", recursive = TRUE, showWarnings = FALSE)
dir.create("figuras/unidade16", recursive = TRUE, showWarnings = FALSE)
dir.create("tabelas/unidade16", recursive = TRUE, showWarnings = FALSE)
dir.create("resultados/unidade16", recursive = TRUE, showWarnings = FALSE)

set.seed(123)

r_base <- terra::rast(
  ncols = 120,
  nrows = 100,
  xmin = -80,
  xmax = -40,
  ymin = -30,
  ymax = 10,
  crs = "EPSG:4326"
)

xy <- as.data.frame(terra::xyFromCell(r_base, 1:terra::ncell(r_base)))
names(xy) <- c("lon", "lat")

normalizar <- function(x) {
  (x - min(x, na.rm = TRUE)) /
    (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

camadas <- xy %>%
  mutate(
    riqueza_potencial =
      exp(-((lon + 62)^2) / 250) *
      exp(-((lat + 8)^2) / 220) +
      0.25 * sin(lon / 5) +
      0.15 * cos(lat / 4),
    
    adequabilidade_atual =
      exp(-((lon + 60)^2) / 280) *
      exp(-((lat + 6)^2) / 200),
    
    adequabilidade_futura =
      exp(-((lon + 58)^2) / 300) *
      exp(-((lat + 3)^2) / 220),
    
    arvores_gigantes =
      exp(-((lon + 55)^2) / 180) *
      exp(-((lat + 2)^2) / 160) +
      0.20 * cos((lon + lat) / 8),
    
    especies_ameacadas =
      exp(-((lon + 68)^2) / 120) *
      exp(-((lat + 12)^2) / 120) +
      exp(-((lon + 50)^2) / 110) *
      exp(-((lat - 2)^2) / 100),
    
    biomassa_potencial =
      0.6 * arvores_gigantes +
      0.4 * riqueza_potencial +
      rnorm(n(), 0, 0.05),
    
    incerteza =
      abs(0.4 * sin(lon / 6) + 0.3 * cos(lat / 5)) +
      runif(n(), 0, 0.15),
    
    custo =
      0.5 + normalizar(abs(lon + 60)) * 0.4 +
      normalizar(abs(lat + 5)) * 0.3 +
      runif(n(), 0, 0.25),
    
    protecao_atual =
      ifelse(
        (lon > -72 & lon < -63 & lat > -12 & lat < -2) |
          (lon > -55 & lon < -47 & lat > -5 & lat < 5),
        1,
        0
      )
  ) %>%
  mutate(
    across(
      c(
        riqueza_potencial,
        adequabilidade_atual,
        adequabilidade_futura,
        arvores_gigantes,
        especies_ameacadas,
        biomassa_potencial,
        incerteza,
        custo
      ),
      normalizar
    )
  )

write_csv(
  camadas,
  "dados/unidade16/processados/camadas_conservacao.csv"
)

plot_df <- camadas %>%
  select(
    lon, lat,
    Riqueza = riqueza_potencial,
    Futuro = adequabilidade_futura,
    Incerteza = incerteza,
    Custo = custo,
    Gigantes = arvores_gigantes,
    Ameacadas = especies_ameacadas
  ) %>%
  pivot_longer(
    cols = c(Riqueza, Futuro, Incerteza, Custo, Gigantes, Ameacadas),
    names_to = "camada",
    values_to = "valor"
  )

g <- ggplot(
  plot_df,
  aes(x = lon, y = lat, fill = valor)
) +
  geom_raster() +
  coord_equal() +
  facet_wrap(~ camada, ncol = 3) +
  scale_fill_viridis_c(name = "Valor") +
  labs(
    title = "Camadas simuladas para aplicações à conservação",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid = element_blank()
  )

ggsave(
  "figuras/unidade16/unidade16_camadas_conservacao.png",
  plot = g,
  width = 11,
  height = 7,
  dpi = 600
)

message("Camadas de conservação simuladas.")
