source("scripts/_bootstrap.R")

# ============================================================
# Unidade 2 - Dados de ocorrência
# Script 05: Diagnóstico ambiental dos registros finais
# ============================================================

pacotes <- c("dplyr", "readr", "ggplot2", "terra", "tidyr", "patchwork")

require_packages(pacotes)
invisible(lapply(pacotes, library, character.only = TRUE))

arquivo_oc <- "dados/unidade02/processados/04_ocorrencias_rarefeitas_dinizia.csv"
arquivo_raster <- "dados/unidade02/processados/worldclim_variaveis_selecionadas_amazonia_unidade02.tif"

if (!file.exists(arquivo_oc)) stop("Execute primeiro o script 04.")
if (!file.exists(arquivo_raster)) stop("Execute primeiro o script 03.")

oc <- read_csv(arquivo_oc, show_col_types = FALSE)
bio <- terra::rast(arquivo_raster)

# ------------------------------------------------------------
# 1. Background ambiental amazônico
# ------------------------------------------------------------

set.seed(123)

background <- terra::spatSample(
  bio,
  size = 10000,
  method = "random",
  na.rm = TRUE,
  xy = TRUE,
  values = TRUE
) %>%
  as.data.frame() %>%
  rename(lon = x, lat = y) %>%
  mutate(tipo = "Background amazônico")

readr::write_csv(
  background,
  "dados/unidade02/processados/05_background_ambiental_amazonia.csv"
)

# ------------------------------------------------------------
# 2. Histogramas das variáveis
# ------------------------------------------------------------

vars <- names(bio)

oc_long <- oc %>%
  select(all_of(vars)) %>%
  pivot_longer(
    cols = everything(),
    names_to = "variavel",
    values_to = "valor"
  )

g_hist <- ggplot(
  oc_long,
  aes(x = valor)
) +
  geom_histogram(
    bins = 25,
    fill = "grey35",
    color = "white"
  ) +
  facet_wrap(~ variavel, scales = "free", ncol = 3) +
  labs(
    title = expression("Distribuição ambiental dos registros finais de " * italic("Dinizia excelsa")),
    x = "Valor ambiental",
    y = "Frequência"
  ) +
  theme_bw(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold")
  )

ggsave(
  "figuras/unidade02/unidade02_histogramas_variaveis_dinizia.png",
  plot = g_hist,
  width = 11,
  height = 7,
  dpi = 600
)

# ------------------------------------------------------------
# 3. Espaço climático BIO5 x BIO12
# ------------------------------------------------------------

g_clima <- ggplot() +
  geom_point(
    data = background,
    aes(
      x = bio5_temp_max_mes_quente,
      y = bio12_precipitacao_anual
    ),
    color = "grey70",
    alpha = 0.35,
    size = 0.6
  ) +
  geom_point(
    data = oc,
    aes(
      x = bio5_temp_max_mes_quente,
      y = bio12_precipitacao_anual
    ),
    color = "red",
    alpha = 0.85,
    size = 1.8
  ) +
  labs(
    title = expression("Espaço climático ocupado por " * italic("Dinizia excelsa")),
    subtitle = "Temperatura máxima do mês mais quente versus precipitação anual",
    x = "BIO5 - Temperatura máxima do mês mais quente (°C)",
    y = "BIO12 - Precipitação anual (mm)"
  ) +
  theme_bw(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

ggsave(
  "figuras/unidade02/unidade02_espaco_climatico_dinizia.png",
  plot = g_clima,
  width = 8,
  height = 6,
  dpi = 600
)

# ------------------------------------------------------------
# 4. Resumo ambiental
# ------------------------------------------------------------

resumo_env <- oc %>%
  summarise(
    n = n(),
    across(
      all_of(vars),
      list(
        media = ~ mean(.x, na.rm = TRUE),
        min = ~ min(.x, na.rm = TRUE),
        max = ~ max(.x, na.rm = TRUE),
        sd = ~ sd(.x, na.rm = TRUE)
      )
    )
  )

readr::write_csv(
  resumo_env,
  "tabelas/unidade02/resumo_ambiental_ocorrencias_finais_dinizia.csv"
)

message("Script 05 concluído: diagnóstico ambiental finalizado.")
