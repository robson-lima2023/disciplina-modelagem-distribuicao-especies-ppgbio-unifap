# ============================================================
# Unidade 14 - Transferência espacial e temporal em SDM
# Script 05: MOP didático
# ============================================================

pacotes <- c("dplyr", "readr", "ggplot2", "FNN")

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

# ------------------------------------------------------------
# 1. Padronizar variáveis com média e desvio da calibração
# ------------------------------------------------------------

medias <- sapply(calib[vars], mean, na.rm = TRUE)
desvios <- sapply(calib[vars], sd, na.rm = TRUE)

calib_z <- scale(
  calib[vars],
  center = medias,
  scale = desvios
) %>%
  as.data.frame()

proj_z <- scale(
  proj[vars],
  center = medias,
  scale = desvios
) %>%
  as.data.frame()

# ------------------------------------------------------------
# 2. Identificar extrapolação estrita
# ------------------------------------------------------------

limites <- lapply(vars, function(v) {
  c(
    min = min(calib[[v]], na.rm = TRUE),
    max = max(calib[[v]], na.rm = TRUE)
  )
})

names(limites) <- vars

extrapolacao_estrita <- rep(FALSE, nrow(proj))

for (v in vars) {
  extrapolacao_estrita <- extrapolacao_estrita |
    proj[[v]] < limites[[v]]["min"] |
    proj[[v]] > limites[[v]]["max"]
}

# ------------------------------------------------------------
# 3. Distância ambiental mínima
# ------------------------------------------------------------
# Para cada pixel projetado, calculamos a distância euclidiana
# ao ponto ambiental mais próximo do domínio de calibração.

nn <- FNN::get.knnx(
  data = as.matrix(calib_z),
  query = as.matrix(proj_z),
  k = 1
)

mop_df <- proj %>%
  select(lon, lat, all_of(vars)) %>%
  mutate(
    distancia_minima = nn$nn.dist[, 1],
    extrapolacao_estrita = extrapolacao_estrita,
    MOP = ifelse(extrapolacao_estrita, NA, distancia_minima)
  )

write_csv(
  mop_df,
  "dados/unidade14/processados/mop_projecao.csv"
)

g <- ggplot(
  mop_df,
  aes(x = lon, y = lat, fill = distancia_minima)
) +
  geom_raster() +
  coord_equal() +
  labs(
    title = "MOP didático",
    subtitle = "Distância ambiental mínima em relação à calibração",
    x = "Longitude",
    y = "Latitude",
    fill = "Distância"
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid = element_blank()
  )

ggsave(
  "figuras/unidade14/unidade14_mop.png",
  plot = g,
  width = 8,
  height = 6,
  dpi = 600
)

message("MOP didático calculado com sucesso.")
