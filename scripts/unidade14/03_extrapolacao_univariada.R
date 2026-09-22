# ============================================================
# Unidade 14 - Transferência espacial e temporal em SDM
# Script 03: Diagnóstico de extrapolação univariada
# ============================================================

pacotes <- c("dplyr", "readr", "ggplot2")

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

limites <- lapply(vars, function(v) {
  data.frame(
    variavel = v,
    min_calib = min(calib[[v]], na.rm = TRUE),
    max_calib = max(calib[[v]], na.rm = TRUE)
  )
}) %>%
  bind_rows()

write_csv(
  limites,
  "tabelas/unidade14/limites_ambientais_calibracao.csv"
)

for (v in vars) {
  min_v <- limites$min_calib[limites$variavel == v]
  max_v <- limites$max_calib[limites$variavel == v]
  
  proj[[paste0("extra_", v)]] <- proj[[v]] < min_v | proj[[v]] > max_v
}

proj <- proj %>%
  mutate(
    extrapolacao_univariada =
      extra_temperatura | extra_precipitacao | extra_sazonalidade
  )

write_csv(
  proj,
  "dados/unidade14/processados/extrapolacao_univariada.csv"
)

g <- ggplot(
  proj,
  aes(x = lon, y = lat, fill = extrapolacao_univariada)
) +
  geom_raster() +
  coord_equal() +
  labs(
    title = "Extrapolação univariada",
    subtitle = "TRUE indica pelo menos uma variável fora do intervalo de calibração",
    x = "Longitude",
    y = "Latitude",
    fill = "Extrapolação"
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid = element_blank()
  )

ggsave(
  "figuras/unidade14/unidade14_extrapolacao_univariada.png",
  plot = g,
  width = 8,
  height = 6,
  dpi = 600
)

message("Diagnóstico de extrapolação univariada concluído.")
