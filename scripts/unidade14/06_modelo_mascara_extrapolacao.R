# ============================================================
# Unidade 14 - Transferência espacial e temporal em SDM
# Script 06: Projeção com máscara de extrapolação
# ============================================================

pacotes <- c("dplyr", "readr", "ggplot2")

instalar <- pacotes[!pacotes %in% rownames(installed.packages())]
if (length(instalar) > 0) install.packages(instalar)

invisible(lapply(pacotes, library, character.only = TRUE))

dados_amb <- read_csv(
  "dados/unidade14/processados/ambientes_calibracao_projecao.csv",
  show_col_types = FALSE
)

mess <- read_csv(
  "dados/unidade14/processados/mess_projecao.csv",
  show_col_types = FALSE
)

calib <- dados_amb %>%
  filter(dominio == "Calibracao")

proj <- dados_amb %>%
  filter(dominio == "Projecao")

# ------------------------------------------------------------
# 1. Simular presenças e background na calibração
# ------------------------------------------------------------

set.seed(123)

calib <- calib %>%
  mutate(
    eta =
      -6 +
      2.8 * exp(-((temperatura - 25)^2) / (2 * 3^2)) +
      2.4 * exp(-((precipitacao - 1700)^2) / (2 * 350^2)) -
      0.025 * sazonalidade,
    adequabilidade_real = plogis(eta),
    peso_presenca = adequabilidade_real / max(adequabilidade_real)
  )

presencas <- calib %>%
  sample_frac(size = 1, weight = peso_presenca) %>%
  slice(1:350) %>%
  mutate(pa = 1)

background <- calib %>%
  slice_sample(n = 3500) %>%
  mutate(pa = 0)

dados_modelo <- bind_rows(presencas, background) %>%
  select(pa, temperatura, precipitacao, sazonalidade)

# ------------------------------------------------------------
# 2. Ajustar GLM didático
# ------------------------------------------------------------

modelo <- glm(
  pa ~ temperatura + I(temperatura^2) +
    precipitacao + I(precipitacao^2) +
    sazonalidade,
  data = dados_modelo,
  family = binomial(link = "logit")
)

saveRDS(
  modelo,
  "resultados/unidade14/modelo_transferencia_glm.rds"
)

# ------------------------------------------------------------
# 3. Projetar para o domínio futuro/novo
# ------------------------------------------------------------

proj$adequabilidade <- predict(
  modelo,
  newdata = proj,
  type = "response"
)

proj$MESS <- mess$MESS

proj <- proj %>%
  mutate(
    extrapolacao = MESS < 0,
    adequabilidade_mascarada = ifelse(extrapolacao, NA, adequabilidade)
  )

write_csv(
  proj,
  "dados/unidade14/processados/projecao_com_mascara_extrapolacao.csv"
)

# ------------------------------------------------------------
# 4. Figura comparativa
# ------------------------------------------------------------

plot_df <- bind_rows(
  proj %>%
    transmute(
      lon, lat,
      valor = adequabilidade,
      mapa = "Projeção original"
    ),
  proj %>%
    transmute(
      lon, lat,
      valor = adequabilidade_mascarada,
      mapa = "Projeção sem áreas extrapoladas"
    )
)

g <- ggplot(
  plot_df,
  aes(x = lon, y = lat, fill = valor)
) +
  geom_raster() +
  coord_equal() +
  facet_wrap(~ mapa, ncol = 2) +
  scale_fill_viridis_c(
    name = "Adequabilidade",
    na.value = "grey85"
  ) +
  labs(
    title = "Projeção com e sem máscara de extrapolação",
    subtitle = "Áreas cinzas indicam pixels com MESS negativo",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid = element_blank()
  )

ggsave(
  "figuras/unidade14/unidade14_projecao_mascarada.png",
  plot = g,
  width = 10,
  height = 5.5,
  dpi = 600
)

message("Projeção com máscara de extrapolação concluída.")
