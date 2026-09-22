source("scripts/_bootstrap.R")

# ============================================================
# Unidade 5 - Área acessível M e background
# Script 05: Preparar base presença-background
# ============================================================

pacotes <- c(
  "dplyr", "readr", "sf", "terra", "ggplot2",
  "tidyr", "tibble"
)

require_packages(pacotes)
invisible(lapply(pacotes, library, character.only = TRUE))

arquivo_oc <- "dados/unidade02/processados/04_ocorrencias_rarefeitas_dinizia.csv"
arquivo_bg <- "dados/unidade05/processados/background_area_m_dinizia.csv"
arquivo_amb_m <- "dados/unidade05/processados/variaveis_ambientais_area_m.tif"

if (!file.exists(arquivo_oc)) {
  arquivo_oc <- "dados/unidade02/processados/03_ocorrencias_ambiente_sem_duplicata_raster.csv"
}

if (!file.exists(arquivo_oc)) stop("Ocorrências finais não encontradas.")
if (!file.exists(arquivo_bg)) stop("Background da área M não encontrado. Execute o script 03.")
if (!file.exists(arquivo_amb_m)) stop("Variáveis ambientais da área M não encontradas. Execute o script 03.")

oc <- readr::read_csv(arquivo_oc, show_col_types = FALSE)
bg <- readr::read_csv(arquivo_bg, show_col_types = FALSE)
amb_m <- terra::rast(arquivo_amb_m)

# ------------------------------------------------------------
# Extrair ambiente nas presenças
# ------------------------------------------------------------

oc_xy <- oc %>%
  dplyr::select(lon, lat) %>%
  as.data.frame()

val_oc <- terra::extract(
  amb_m,
  as.matrix(oc_xy)
)

val_oc_df <- as.data.frame(val_oc)

if ("ID" %in% names(val_oc_df)) {
  val_oc_df <- val_oc_df %>% dplyr::select(-ID)
}

pres <- dplyr::bind_cols(
  oc %>% dplyr::select(any_of(c("especie", "especie_padrao")), lon, lat),
  val_oc_df
) %>%
  dplyr::mutate(
    pa = 1,
    tipo = "Presenca"
  ) %>%
  tidyr::drop_na()

# ------------------------------------------------------------
# Ajustar background
# ------------------------------------------------------------

vars <- names(amb_m)

bg2 <- bg %>%
  dplyr::select(lon, lat, all_of(vars)) %>%
  dplyr::mutate(
    pa = 0,
    tipo = "Background"
  ) %>%
  tidyr::drop_na()

# ------------------------------------------------------------
# Base final presença-background
# ------------------------------------------------------------

dados_pb <- dplyr::bind_rows(
  pres %>% dplyr::select(lon, lat, all_of(vars), pa, tipo),
  bg2 %>% dplyr::select(lon, lat, all_of(vars), pa, tipo)
)

readr::write_csv(
  dados_pb,
  "dados/unidade05/processados/dados_presenca_background_dinizia.csv"
)

readr::write_csv(
  pres,
  "dados/unidade05/processados/presencas_area_m_dinizia.csv"
)

# ------------------------------------------------------------
# Espaço ambiental
# ------------------------------------------------------------

var_x <- vars[grepl("temp|temperatura|bio5|bio8", vars)][1]
var_y <- vars[grepl("precipitacao|bio12|bio14|bio18|bio19", vars)][1]

if (is.na(var_x) || is.na(var_y)) {
  var_x <- vars[1]
  var_y <- vars[2]
}

set.seed(123)

bg_plot <- dados_pb %>%
  filter(pa == 0) %>%
  sample_n(size = min(5000, nrow(.)))

pres_plot <- dados_pb %>%
  filter(pa == 1)

plot_df <- bind_rows(bg_plot, pres_plot)

g <- ggplot(
  plot_df,
  aes(x = .data[[var_x]], y = .data[[var_y]], color = tipo)
) +
  geom_point(alpha = 0.45, size = 0.8) +
  labs(
    title = expression("Espaço ambiental de calibração para " * italic("Dinizia excelsa")),
    subtitle = "Presenças versus background dentro da área M",
    x = var_x,
    y = var_y,
    color = NULL
  ) +
  theme_bw(base_size = 11) +
  theme(plot.title = element_text(face = "bold"))

ggsave(
  "figuras/unidade05/unidade05_espaco_ambiental_presenca_background.png",
  plot = g,
  width = 8,
  height = 6,
  dpi = 600
)

# ------------------------------------------------------------
# Resumo
# ------------------------------------------------------------

resumo <- dados_pb %>%
  count(tipo, pa, name = "n")

readr::write_csv(
  resumo,
  "tabelas/unidade05/resumo_presenca_background.csv"
)

message("Base presença-background preparada com sucesso.")
print(resumo)
