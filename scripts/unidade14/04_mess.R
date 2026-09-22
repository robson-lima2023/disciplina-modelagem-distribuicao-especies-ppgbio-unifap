# ============================================================
# Unidade 14 - Transferência espacial e temporal em SDM
# Script 04: MESS didático
# ============================================================

pacotes <- c("dplyr", "readr", "ggplot2", "terra")

instalar <- pacotes[!pacotes %in% rownames(installed.packages())]
if (length(instalar) > 0) install.packages(instalar)

invisible(lapply(pacotes, library, character.only = TRUE))

dados_amb <- read_csv(
  "dados/unidade14/processados/ambientes_calibracao_projecao.csv",
  show_col_types = FALSE
)

vars <- c("temperatura", "precipitacao", "sazonalidade")

calib <- dados_amb %>% filter(dominio == "Calibracao")
proj <- dados_amb %>% filter(dominio == "Projecao")

calc_mess_var <- function(x, min_ref, max_ref) {
  amplitude <- max_ref - min_ref
  
  ifelse(
    x < min_ref,
    100 * (x - min_ref) / amplitude,
    ifelse(
      x > max_ref,
      100 * (max_ref - x) / amplitude,
      100 * pmin(
        (x - min_ref) / amplitude,
        (max_ref - x) / amplitude
      )
    )
  )
}

mess_vars <- lapply(vars, function(v) {
  min_v <- min(calib[[v]], na.rm = TRUE)
  max_v <- max(calib[[v]], na.rm = TRUE)
  
  calc_mess_var(
    x = proj[[v]],
    min_ref = min_v,
    max_ref = max_v
  )
})

names(mess_vars) <- paste0("mess_", vars)

mess_df <- bind_cols(
  proj %>% select(lon, lat, all_of(vars)),
  as.data.frame(mess_vars)
) %>%
  mutate(
    MESS = pmin(mess_temperatura, mess_precipitacao, mess_sazonalidade),
    variavel_limitante = vars[max.col(
      -as.matrix(select(., starts_with("mess_"))),
      ties.method = "first"
    )]
  )

write_csv(
  mess_df,
  "dados/unidade14/processados/mess_projecao.csv"
)

r_base <- terra::rast(
  ncols = 120,
  nrows = 100,
  xmin = -80,
  xmax = -40,
  ymin = -30,
  ymax = 10,
  crs = "EPSG:4326"
)

r_mess <- r_base
terra::values(r_mess) <- mess_df$MESS

terra::writeRaster(
  r_mess,
  "resultados/unidade14/mess_projecao.tif",
  overwrite = TRUE
)

g <- ggplot(
  mess_df,
  aes(x = lon, y = lat, fill = MESS)
) +
  geom_raster() +
  coord_equal() +
  scale_fill_gradient2(
    midpoint = 0,
    name = "MESS"
  ) +
  labs(
    title = "Multivariate Environmental Similarity Surface",
    subtitle = "Valores negativos indicam extrapolação ambiental",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid = element_blank()
  )

ggsave(
  "figuras/unidade14/unidade14_mess.png",
  plot = g,
  width = 8,
  height = 6,
  dpi = 600
)

message("MESS calculado com sucesso.")
