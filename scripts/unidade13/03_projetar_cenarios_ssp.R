# ============================================================
# Unidade 13 - Projeções Climáticas Futuras em SDM
# Script 03: Projetar cenários SSP
# ============================================================

pacotes <- c("dplyr", "readr", "terra", "ggplot2", "tidyr")

instalar <- pacotes[!pacotes %in% rownames(installed.packages())]
if (length(instalar) > 0) install.packages(instalar)

invisible(lapply(pacotes, library, character.only = TRUE))

climas <- read_csv(
  "dados/unidade13/processados/clima_atual_futuro_simulado.csv",
  show_col_types = FALSE
)

modelo <- readRDS(
  "resultados/unidade13/modelo_glm_clima_atual.rds"
)

cenarios <- c("Atual", "SSP126", "SSP245", "SSP370", "SSP585")

predicoes <- climas %>%
  filter(periodo %in% cenarios) %>%
  group_by(periodo) %>%
  group_modify(~ {
    .x$adequabilidade <- predict(
      modelo,
      newdata = .x,
      type = "response"
    )
    .x
  }) %>%
  ungroup()

write_csv(
  predicoes,
  "dados/unidade13/processados/predicoes_adequabilidade_cenarios.csv"
)

# Figura com facetas em ggplot para todos os cenários

g <- ggplot(
  predicoes,
  aes(x = lon, y = lat, fill = adequabilidade)
) +
  geom_raster() +
  coord_equal() +
  facet_wrap(~ periodo, ncol = 3) +
  scale_fill_viridis_c(name = "Adequabilidade") +
  labs(
    title = "Projeções de adequabilidade sob cenários SSP",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid = element_blank()
  )

ggsave(
  "figuras/unidade13/unidade13_projecoes_ssp.png",
  plot = g,
  width = 10,
  height = 7,
  dpi = 600
)

# Exportar rasters individuais

r_base <- terra::rast(
  ncols = 120,
  nrows = 100,
  xmin = -80,
  xmax = -40,
  ymin = -30,
  ymax = 10,
  crs = "EPSG:4326"
)

for (c in cenarios) {
  dados_c <- predicoes %>% filter(periodo == c)
  
  r <- r_base
  terra::values(r) <- dados_c$adequabilidade
  
  terra::writeRaster(
    r,
    paste0("resultados/unidade13/adequabilidade_", c, ".tif"),
    overwrite = TRUE
  )
}

message("Projeções para cenários SSP concluídas.")
