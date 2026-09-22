# ============================================================
# Unidade 13 - Projeções Climáticas Futuras em SDM
# Script 01: Simular clima atual e cenários futuros
# ============================================================

setwd("C:/Users/rblfl/OneDrive/Documentos/Playground/Species-Distribution-Modeling")

pacotes <- c("dplyr", "readr", "ggplot2", "terra", "tidyr")

instalar <- pacotes[!pacotes %in% rownames(installed.packages())]
if (length(instalar) > 0) install.packages(instalar)

invisible(lapply(pacotes, library, character.only = TRUE))

dir.create("dados/unidade13/processados", recursive = TRUE, showWarnings = FALSE)
dir.create("figuras/unidade13", recursive = TRUE, showWarnings = FALSE)
dir.create("tabelas/unidade13", recursive = TRUE, showWarnings = FALSE)
dir.create("resultados/unidade13", recursive = TRUE, showWarnings = FALSE)

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

clima_atual <- xy %>%
  mutate(
    temperatura = 25 + 0.20 * lat + sin(lon / 6) * 2 + rnorm(n(), 0, 1.1),
    precipitacao = 1800 - 10 * abs(lon + 60) + cos(lat / 4) * 180 + rnorm(n(), 0, 90),
    sazonalidade = 45 + 0.4 * abs(lat) + rnorm(n(), 0, 3),
    periodo = "Atual"
  )

criar_cenario <- function(dados, nome, delta_temp, fator_prec) {
  dados %>%
    mutate(
      temperatura = temperatura + delta_temp,
      precipitacao = precipitacao * fator_prec,
      sazonalidade = sazonalidade + delta_temp * 2,
      periodo = nome
    )
}

ssp126 <- criar_cenario(clima_atual, "SSP126", delta_temp = 1.2, fator_prec = 0.98)
ssp245 <- criar_cenario(clima_atual, "SSP245", delta_temp = 2.0, fator_prec = 0.95)
ssp370 <- criar_cenario(clima_atual, "SSP370", delta_temp = 3.2, fator_prec = 0.90)
ssp585 <- criar_cenario(clima_atual, "SSP585", delta_temp = 4.5, fator_prec = 0.85)

climas <- bind_rows(
  clima_atual,
  ssp126,
  ssp245,
  ssp370,
  ssp585
)

write_csv(
  climas,
  "dados/unidade13/processados/clima_atual_futuro_simulado.csv"
)

r_temp <- r_base
terra::values(r_temp) <- clima_atual$temperatura

terra::writeRaster(
  r_temp,
  "resultados/unidade13/temperatura_atual.tif",
  overwrite = TRUE
)

png(
  "figuras/unidade13/unidade13_clima_atual_temperatura.png",
  width = 1800,
  height = 1400,
  res = 220
)

plot(r_temp, main = "Temperatura simulada - clima atual")

dev.off()

message("Clima atual e cenários futuros simulados.")
