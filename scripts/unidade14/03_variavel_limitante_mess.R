source("scripts/_bootstrap.R")

# ============================================================
# Unidade 14 - Transferência, extrapolação, MESS e MOP
# Script 03: Variável limitante no MESS
# ============================================================

# ------------------------------------------------------------
# 1. Pacotes
# ------------------------------------------------------------

pacotes <- c(
  "dplyr",
  "readr",
  "sf",
  "ggplot2",
  "ggspatial",
  "tidyr",
  "tibble",
  "stringr",
  "patchwork"
)

require_packages(pacotes)
invisible(lapply(pacotes, library, character.only = TRUE))

sf::sf_use_s2(FALSE)

# ------------------------------------------------------------
# 2. Diretório raiz
# ------------------------------------------------------------
# ------------------------------------------------------------
# 3. Diretórios
# ------------------------------------------------------------

dir.create("dados/unidade14/processados", recursive = TRUE, showWarnings = FALSE)
dir.create("figuras/unidade14", recursive = TRUE, showWarnings = FALSE)
dir.create("tabelas/unidade14", recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# 4. Configurações
# ------------------------------------------------------------

cenarios <- c("ssp126", "ssp245", "ssp370", "ssp585")

periodo <- "2061-2080"

modelo_gcm <- "MIROC6"

arquivo_bioma <- "dados/unidade01/brutos/amazon_biome_border.shp"

arquivo_m <- "dados/unidade05/processados/area_m_dinizia.gpkg"

arquivo_oc <- "dados/unidade05/processados/presencas_area_m_dinizia.csv"

if (!file.exists(arquivo_bioma)) {
  stop("Limite do bioma Amazônia não encontrado: ", arquivo_bioma)
}

if (!file.exists(arquivo_oc)) {
  stop("Arquivo de ocorrências não encontrado: ", arquivo_oc)
}

if (!file.exists(arquivo_m)) {
  warning("Área M não encontrada. Os mapas serão gerados sem contorno da área M.")
}

# ------------------------------------------------------------
# 5. Ler arquivos MESS por cenário
# ------------------------------------------------------------

dados <- lapply(cenarios, function(ssp) {
  
  arquivo <- paste0(
    "dados/unidade14/processados/mess_",
    ssp,
    "_",
    periodo,
    "_",
    modelo_gcm,
    ".csv"
  )
  
  # Compatibilidade com nome antigo, caso exista
  if (!file.exists(arquivo)) {
    arquivo_antigo <- paste0(
      "dados/unidade14/processados/mess_",
      ssp,
      ".csv"
    )
    
    if (file.exists(arquivo_antigo)) {
      arquivo <- arquivo_antigo
    }
  }
  
  if (!file.exists(arquivo)) {
    stop("Arquivo MESS não encontrado para ", ssp, ". Execute o Script 02 da Unidade 14.")
  }
  
  read_csv(
    arquivo,
    show_col_types = FALSE
  ) |>
    select(
      lon,
      lat,
      MESS,
      extrapolacao,
      variavel_limitante
    ) |>
    mutate(
      cenario = ssp,
      variavel_limitante = as.character(variavel_limitante)
    )
}) |>
  bind_rows()

dados <- dados |>
  filter(
    !is.na(lon),
    !is.na(lat),
    !is.na(MESS),
    !is.na(variavel_limitante)
  )

if (nrow(dados) == 0) {
  stop("A tabela de variáveis limitantes está vazia.")
}

# ------------------------------------------------------------
# 6. Nomes legíveis das variáveis
# ------------------------------------------------------------

nome_legivel <- function(x) {
  x |>
    stringr::str_replace_all("_", " ") |>
    stringr::str_replace_all("bio", "BIO") |>
    stringr::str_to_sentence()
}

dados <- dados |>
  mutate(
    variavel_legivel = nome_legivel(variavel_limitante)
  )

write_csv(
  dados,
  "dados/unidade14/processados/variavel_limitante_mess.csv"
)

# ------------------------------------------------------------
# 7. Frequência das variáveis limitantes
# ------------------------------------------------------------

tab <- dados |>
  count(
    cenario,
    variavel_limitante,
    variavel_legivel,
    name = "n"
  ) |>
  group_by(cenario) |>
  mutate(
    prop = n / sum(n)
  ) |>
  ungroup() |>
  arrange(
    cenario,
    desc(prop)
  )

write_csv(
  tab,
  "tabelas/unidade14/frequencia_variavel_limitante_mess.csv"
)

# Frequência apenas em áreas extrapoladas
tab_extrap <- dados |>
  filter(extrapolacao == TRUE) |>
  count(
    cenario,
    variavel_limitante,
    variavel_legivel,
    name = "n"
  ) |>
  group_by(cenario) |>
  mutate(
    prop = n / sum(n)
  ) |>
  ungroup() |>
  arrange(
    cenario,
    desc(prop)
  )

write_csv(
  tab_extrap,
  "tabelas/unidade14/frequencia_variavel_limitante_mess_extrapolacao.csv"
)

# ------------------------------------------------------------
# 8. Preparar bioma, área M e ocorrências
# ------------------------------------------------------------

bioma_raw <- sf::st_read(
  arquivo_bioma,
  quiet = TRUE
)

bioma_plot <- bioma_raw |>
  sf::st_transform(5880) |>
  sf::st_make_valid() |>
  sf::st_union() |>
  sf::st_as_sf() |>
  sf::st_make_valid() |>
  sf::st_transform(4326)

bbox_bioma <- sf::st_bbox(
  bioma_plot
)

area_m_plot <- NULL

if (file.exists(arquivo_m)) {
  area_m_plot <- sf::st_read(
    arquivo_m,
    quiet = TRUE
  ) |>
    sf::st_make_valid() |>
    sf::st_transform(4326)
}

oc <- read_csv(
  arquivo_oc,
  show_col_types = FALSE
)

oc_sf <- sf::st_as_sf(
  oc,
  coords = c("lon", "lat"),
  crs = 4326,
  remove = FALSE
)

# ------------------------------------------------------------
# 9. Mapa da variável limitante
# ------------------------------------------------------------

g_mapa <- ggplot() +
  geom_sf(
    data = bioma_plot,
    fill = "grey96",
    color = "grey35",
    linewidth = 0.30
  ) +
  geom_raster(
    data = dados,
    aes(
      x = lon,
      y = lat,
      fill = variavel_legivel
    )
  ) +
  {
    if (!is.null(area_m_plot)) {
      geom_sf(
        data = area_m_plot,
        fill = NA,
        color = "grey20",
        linewidth = 0.25,
        linetype = "dashed"
      )
    }
  } +
  geom_sf(
    data = oc_sf,
    color = "black",
    fill = "red",
    shape = 21,
    size = 0.50,
    stroke = 0.13,
    alpha = 0.65
  ) +
  coord_sf(
    xlim = c(bbox_bioma["xmin"], bbox_bioma["xmax"]),
    ylim = c(bbox_bioma["ymin"], bbox_bioma["ymax"]),
    expand = FALSE
  ) +
  facet_wrap(
    ~ cenario,
    ncol = 2
  ) +
  ggspatial::annotation_scale(
    location = "bl",
    width_hint = 0.25,
    text_cex = 0.60
  ) +
  labs(
    title = expression("Variável limitante no MESS para " * italic("Dinizia excelsa")),
    subtitle = "Variável com menor similaridade ambiental em cada célula futura",
    x = "Longitude",
    y = "Latitude",
    fill = "Variável"
  ) +
  theme_bw(base_size = 10) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10),
    strip.text = element_text(face = "bold"),
    panel.grid.major = element_line(color = "grey88", linewidth = 0.20),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    legend.text = element_text(size = 8)
  )

ggsave(
  "figuras/unidade14/unidade14_variavel_limitante_mess.png",
  plot = g_mapa,
  width = 12,
  height = 9,
  dpi = 600
)

# ------------------------------------------------------------
# 10. Gráfico de barras das variáveis limitantes
# ------------------------------------------------------------

g_bar <- ggplot(
  tab,
  aes(
    x = reorder(variavel_legivel, prop),
    y = prop
  )
) +
  geom_col(
    fill = "grey55",
    color = "grey25",
    linewidth = 0.15
  ) +
  coord_flip() +
  facet_wrap(
    ~ cenario,
    ncol = 2,
    scales = "free_y"
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1)
  ) +
  labs(
    title = "Frequência das variáveis limitantes no MESS",
    subtitle = "Proporção de células em que cada variável define a menor similaridade ambiental",
    x = NULL,
    y = "Proporção de células"
  ) +
  theme_bw(base_size = 10) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10),
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave(
  "figuras/unidade14/unidade14_frequencia_variavel_limitante_mess.png",
  plot = g_bar,
  width = 11,
  height = 8,
  dpi = 600
)

# ------------------------------------------------------------
# 11. Figura composta
# ------------------------------------------------------------

fig_composta <- g_mapa / g_bar +
  patchwork::plot_annotation(
    title = expression("Diagnóstico das variáveis limitantes de transferência para " * italic("Dinizia excelsa")),
    subtitle = "Mapas e frequências das variáveis que mais restringem a similaridade ambiental futura"
  ) &
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 10)
  )

ggsave(
  "figuras/unidade14/unidade14_variavel_limitante_mess_patchwork.png",
  plot = fig_composta,
  width = 12,
  height = 16,
  dpi = 600
)

# ------------------------------------------------------------
# 12. Mensagem final
# ------------------------------------------------------------

message("Variável limitante do MESS mapeada com sucesso.")

print(tab)
