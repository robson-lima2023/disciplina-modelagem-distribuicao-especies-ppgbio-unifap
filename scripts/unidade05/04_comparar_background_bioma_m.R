source("scripts/_bootstrap.R")

# ============================================================
# Unidade 5 - Área acessível M e background
# Script 04: Comparar background no bioma e na área M
# ============================================================

pacotes <- c(
  "dplyr", "readr", "sf", "terra", "ggplot2",
  "patchwork", "tidyr"
)

require_packages(pacotes)
invisible(lapply(pacotes, library, character.only = TRUE))

arquivo_bg_m <- "dados/unidade05/processados/background_area_m_dinizia.csv"
arquivo_bg_bioma <- "dados/unidade03/processados/background_ambiental_amazonia_unidade03.csv"
arquivo_oc <- "dados/unidade02/processados/04_ocorrencias_rarefeitas_dinizia.csv"

if (!file.exists(arquivo_bg_m)) stop("Execute primeiro o script 03.")
if (!file.exists(arquivo_bg_bioma)) stop("Background do bioma da Unidade 3 não encontrado.")
if (!file.exists(arquivo_oc)) {
  arquivo_oc <- "dados/unidade02/processados/03_ocorrencias_ambiente_sem_duplicata_raster.csv"
}

bg_m <- readr::read_csv(arquivo_bg_m, show_col_types = FALSE) %>%
  dplyr::mutate(origem = "Background na área M")

bg_bioma <- readr::read_csv(arquivo_bg_bioma, show_col_types = FALSE) %>%
  dplyr::mutate(origem = "Background no bioma")

oc <- readr::read_csv(arquivo_oc, show_col_types = FALSE)

# ------------------------------------------------------------
# Selecionar variáveis comparáveis
# ------------------------------------------------------------

vars_comuns <- intersect(names(bg_m), names(bg_bioma))
vars_comuns <- vars_comuns[!vars_comuns %in% c("lon", "lat", "pa", "tipo", "origem")]

# Usar duas variáveis principais para visualização
var_x <- vars_comuns[grepl("temp|temperatura|bio5|bio8", vars_comuns)][1]
var_y <- vars_comuns[grepl("precipitacao|bio12|bio14|bio18|bio19", vars_comuns)][1]

if (is.na(var_x) || is.na(var_y)) {
  var_x <- vars_comuns[1]
  var_y <- vars_comuns[2]
}

# ------------------------------------------------------------
# Amostrar para gráfico leve
# ------------------------------------------------------------

set.seed(123)

bg_bioma_plot <- bg_bioma %>%
  dplyr::select(lon, lat, origem, all_of(c(var_x, var_y))) %>%
  dplyr::sample_n(size = min(5000, nrow(.)))

bg_m_plot <- bg_m %>%
  dplyr::select(lon, lat, origem, all_of(c(var_x, var_y))) %>%
  dplyr::sample_n(size = min(5000, nrow(.)))

bg_plot <- bind_rows(bg_bioma_plot, bg_m_plot)

# ------------------------------------------------------------
# Gráfico do espaço ambiental
# ------------------------------------------------------------

g_env <- ggplot(
  bg_plot,
  aes(x = .data[[var_x]], y = .data[[var_y]], color = origem)
) +
  geom_point(alpha = 0.30, size = 0.7) +
  labs(
    title = "Comparação ambiental entre backgrounds",
    subtitle = "Bioma Amazônia versus área acessível M",
    x = var_x,
    y = var_y,
    color = NULL
  ) +
  theme_bw(base_size = 11) +
  theme(plot.title = element_text(face = "bold"))

# ------------------------------------------------------------
# Histogramas
# ------------------------------------------------------------

bg_long <- bg_plot %>%
  tidyr::pivot_longer(
    cols = all_of(c(var_x, var_y)),
    names_to = "variavel",
    values_to = "valor"
  )

g_hist <- ggplot(
  bg_long,
  aes(x = valor, fill = origem)
) +
  geom_density(alpha = 0.35) +
  facet_wrap(~ variavel, scales = "free", ncol = 1) +
  labs(
    title = "Distribuição ambiental do background",
    x = "Valor ambiental",
    y = "Densidade",
    fill = NULL
  ) +
  theme_bw(base_size = 11) +
  theme(plot.title = element_text(face = "bold"))

fig <- g_env | g_hist

ggsave(
  "figuras/unidade05/unidade05_comparacao_background.png",
  plot = fig,
  width = 13,
  height = 6,
  dpi = 600
)

# ------------------------------------------------------------
# Tabela-resumo
# ------------------------------------------------------------

resumo <- bg_plot %>%
  group_by(origem) %>%
  summarise(
    n = n(),
    media_x = mean(.data[[var_x]], na.rm = TRUE),
    sd_x = sd(.data[[var_x]], na.rm = TRUE),
    media_y = mean(.data[[var_y]], na.rm = TRUE),
    sd_y = sd(.data[[var_y]], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    variavel_x = var_x,
    variavel_y = var_y
  )

readr::write_csv(
  resumo,
  "tabelas/unidade05/resumo_comparacao_background.csv"
)

message("Comparação entre background no bioma e na área M concluída.")
print(resumo)
