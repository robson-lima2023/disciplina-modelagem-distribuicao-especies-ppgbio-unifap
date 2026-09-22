source("scripts/_bootstrap.R")

# ============================================================
# Unidade 16 - Incertezas em SDM
# Script 08: Síntese das incertezas
# ============================================================

# ------------------------------------------------------------
# 1. Pacotes
# ------------------------------------------------------------

pacotes <- c(
  "dplyr",
  "readr",
  "terra",
  "ggplot2",
  "tibble",
  "tidyr",
  "scales",
  "patchwork"
)

require_packages(pacotes)
invisible(lapply(pacotes, library, character.only = TRUE))

# ------------------------------------------------------------
# 2. Diretório raiz
# ------------------------------------------------------------
# ------------------------------------------------------------
# 3. Diretórios
# ------------------------------------------------------------

dir.create("dados/unidade16/processados", recursive = TRUE, showWarnings = FALSE)
dir.create("figuras/unidade16", recursive = TRUE, showWarnings = FALSE)
dir.create("tabelas/unidade16", recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# 4. Arquivos de entrada
# ------------------------------------------------------------

arquivo_classes <- "dados/unidade16/processados/adequabilidade_incerteza_classes.csv"

arquivo_ref <- "resultados/unidade11/ensemble_media_simples_dinizia.tif"

arquivo_resumo_componentes <- "tabelas/unidade16/resumo_incerteza_integrada.csv"

if (!file.exists(arquivo_classes)) {
  stop("Arquivo de classes não encontrado. Execute o Script 07 da Unidade 16.")
}

if (!file.exists(arquivo_ref)) {
  stop("Raster de referência não encontrado. Execute a Unidade 11.")
}

# ------------------------------------------------------------
# 5. Ler classes de adequabilidade-incerteza
# ------------------------------------------------------------

classes <- readr::read_csv(
  arquivo_classes,
  show_col_types = FALSE
) |>
  dplyr::mutate(
    classe_nome = as.character(classe_nome),
    adequabilidade = as.numeric(adequabilidade),
    incerteza_integrada = as.numeric(incerteza_integrada)
  ) |>
  dplyr::filter(
    !is.na(lon),
    !is.na(lat),
    !is.na(classe_nome)
  )

if (nrow(classes) == 0) {
  stop("A tabela de classes está vazia após remover NA.")
}

# ------------------------------------------------------------
# 6. Área por célula
# ------------------------------------------------------------

r_ref <- terra::rast(
  arquivo_ref
)

area_cell <- terra::cellSize(
  r_ref,
  unit = "km"
)

area_df <- as.data.frame(
  area_cell,
  xy = TRUE,
  na.rm = TRUE
)

names(area_df) <- c("lon", "lat", "area_km2")

classes_area <- classes |>
  dplyr::left_join(
    area_df,
    by = c("lon", "lat")
  ) |>
  dplyr::filter(
    !is.na(area_km2)
  ) |>
  dplyr::mutate(
    classe_nome = factor(
      classe_nome,
      levels = c(
        "Baixa adequabilidade / baixa incerteza",
        "Baixa adequabilidade / alta incerteza",
        "Alta adequabilidade / alta incerteza",
        "Alta adequabilidade / baixa incerteza"
      )
    )
  )

readr::write_csv(
  classes_area,
  "dados/unidade16/processados/adequabilidade_incerteza_classes_area.csv"
)

# ------------------------------------------------------------
# 7. Síntese por classe
# ------------------------------------------------------------

sintese <- classes_area |>
  dplyr::group_by(classe_nome) |>
  dplyr::summarise(
    area_km2 = sum(area_km2, na.rm = TRUE),
    area_mil_km2 = area_km2 / 1000,
    n_pixels = dplyr::n(),
    adequabilidade_media = mean(adequabilidade, na.rm = TRUE),
    adequabilidade_mediana = median(adequabilidade, na.rm = TRUE),
    incerteza_media = mean(incerteza_integrada, na.rm = TRUE),
    incerteza_mediana = median(incerteza_integrada, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    prop_area = area_km2 / sum(area_km2, na.rm = TRUE)
  ) |>
  dplyr::arrange(
    dplyr::desc(area_km2)
  )

readr::write_csv(
  sintese,
  "tabelas/unidade16/sintese_adequabilidade_incerteza.csv"
)

# ------------------------------------------------------------
# 8. Tabela ampla para interpretação
# ------------------------------------------------------------

sintese_wide <- sintese |>
  dplyr::select(
    classe_nome,
    area_km2,
    prop_area,
    adequabilidade_media,
    incerteza_media
  ) |>
  tidyr::pivot_longer(
    cols = -classe_nome,
    names_to = "metrica",
    values_to = "valor"
  )

readr::write_csv(
  sintese_wide,
  "tabelas/unidade16/sintese_adequabilidade_incerteza_long.csv"
)

# ------------------------------------------------------------
# 9. Resumo dos componentes de incerteza integrada
# ------------------------------------------------------------

resumo_componentes <- NULL

if (file.exists(arquivo_resumo_componentes)) {
  
  resumo_componentes <- readr::read_csv(
    arquivo_resumo_componentes,
    show_col_types = FALSE
  ) |>
    dplyr::filter(
      componente %in% c(
        "algoritmica",
        "cenarios",
        "extrapolacao",
        "paleoclimatica",
        "incerteza_integrada"
      )
    ) |>
    dplyr::mutate(
      componente_legenda = dplyr::case_when(
        componente == "algoritmica" ~ "Algorítmica",
        componente == "cenarios" ~ "Cenários futuros",
        componente == "extrapolacao" ~ "Extrapolação",
        componente == "paleoclimatica" ~ "Paleoclimática",
        componente == "incerteza_integrada" ~ "Integrada",
        TRUE ~ componente
      )
    )
  
  readr::write_csv(
    resumo_componentes,
    "tabelas/unidade16/sintese_componentes_incerteza.csv"
  )
}

# ------------------------------------------------------------
# 10. Cores
# ------------------------------------------------------------

cores_classes <- c(
  "Baixa adequabilidade / baixa incerteza" = "grey85",
  "Baixa adequabilidade / alta incerteza" = "#D9A441",
  "Alta adequabilidade / alta incerteza" = "#B2182B",
  "Alta adequabilidade / baixa incerteza" = "#1A9850"
)

# ------------------------------------------------------------
# 11. Figura 1: área absoluta por classe
# ------------------------------------------------------------

g_area <- ggplot(
  sintese,
  aes(
    x = reorder(classe_nome, area_km2),
    y = area_km2,
    fill = classe_nome
  )
) +
  geom_col(
    color = "grey25",
    linewidth = 0.15,
    show.legend = FALSE
  ) +
  coord_flip() +
  scale_fill_manual(
    values = cores_classes,
    drop = FALSE
  ) +
  scale_y_continuous(
    labels = scales::label_number(
      big.mark = ".",
      decimal.mark = ","
    )
  ) +
  labs(
    title = expression("Síntese de adequabilidade e incerteza para " * italic("Dinizia excelsa")),
    subtitle = "Área absoluta por classe de robustez ecológica",
    x = NULL,
    y = expression("Área (km"^2*")")
  ) +
  theme_bw(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10),
    panel.grid.minor = element_blank()
  )

ggsave(
  "figuras/unidade16/unidade16_sintese_incertezas.png",
  plot = g_area,
  width = 9,
  height = 6,
  dpi = 600
)

# ------------------------------------------------------------
# 12. Figura 2: proporção da área por classe
# ------------------------------------------------------------

g_prop <- ggplot(
  sintese,
  aes(
    x = reorder(classe_nome, prop_area),
    y = prop_area,
    fill = classe_nome
  )
) +
  geom_col(
    color = "grey25",
    linewidth = 0.15,
    show.legend = FALSE
  ) +
  coord_flip() +
  scale_fill_manual(
    values = cores_classes,
    drop = FALSE
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1)
  ) +
  labs(
    title = "Proporção da área por classe de adequabilidade-incerteza",
    subtitle = "Áreas verdes representam alta adequabilidade com baixa incerteza",
    x = NULL,
    y = "Proporção da área"
  ) +
  theme_bw(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10),
    panel.grid.minor = element_blank()
  )

ggsave(
  "figuras/unidade16/unidade16_sintese_incertezas_proporcao.png",
  plot = g_prop,
  width = 9,
  height = 6,
  dpi = 600
)

# ------------------------------------------------------------
# 13. Figura 3: adequabilidade média e incerteza média por classe
# ------------------------------------------------------------

sintese_metricas <- sintese |>
  dplyr::select(
    classe_nome,
    adequabilidade_media,
    incerteza_media
  ) |>
  tidyr::pivot_longer(
    cols = c(adequabilidade_media, incerteza_media),
    names_to = "metrica",
    values_to = "valor"
  ) |>
  dplyr::mutate(
    metrica = dplyr::case_when(
      metrica == "adequabilidade_media" ~ "Adequabilidade média",
      metrica == "incerteza_media" ~ "Incerteza média",
      TRUE ~ metrica
    )
  )

g_metricas <- ggplot(
  sintese_metricas,
  aes(
    x = reorder(classe_nome, valor),
    y = valor,
    fill = classe_nome
  )
) +
  geom_col(
    color = "grey25",
    linewidth = 0.15,
    show.legend = FALSE
  ) +
  coord_flip() +
  facet_wrap(
    ~ metrica,
    ncol = 1,
    scales = "free_y"
  ) +
  scale_fill_manual(
    values = cores_classes,
    drop = FALSE
  ) +
  scale_y_continuous(
    limits = c(0, 1)
  ) +
  labs(
    title = "Adequabilidade e incerteza médias por classe",
    subtitle = "Síntese dos valores médios dentro de cada classe espacial",
    x = NULL,
    y = "Valor médio"
  ) +
  theme_bw(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10),
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave(
  "figuras/unidade16/unidade16_metricas_medias_classes.png",
  plot = g_metricas,
  width = 9,
  height = 8,
  dpi = 600
)

# ------------------------------------------------------------
# 14. Figura 4: componentes de incerteza, se disponível
# ------------------------------------------------------------

if (!is.null(resumo_componentes) && nrow(resumo_componentes) > 0) {
  
  g_componentes <- ggplot(
    resumo_componentes,
    aes(
      x = reorder(componente_legenda, media),
      y = media
    )
  ) +
    geom_col(
      fill = "grey55",
      color = "grey25",
      linewidth = 0.15
    ) +
    coord_flip() +
    scale_y_continuous(
      limits = c(0, 1)
    ) +
    labs(
      title = "Contribuição média dos componentes de incerteza",
      subtitle = "Valores normalizados entre 0 e 1",
      x = NULL,
      y = "Valor médio normalizado"
    ) +
    theme_bw(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold", size = 13),
      plot.subtitle = element_text(size = 10),
      panel.grid.minor = element_blank()
    )
  
  ggsave(
    "figuras/unidade16/unidade16_componentes_incerteza_media.png",
    plot = g_componentes,
    width = 8,
    height = 5,
    dpi = 600
  )
  
  fig_composta <- (g_area / g_prop / g_metricas / g_componentes) +
    patchwork::plot_annotation(
      title = expression("Síntese final das incertezas em SDM para " * italic("Dinizia excelsa")),
      subtitle = "Classes de robustez, proporções espaciais e componentes médios de incerteza"
    ) &
    theme(
      plot.title = element_text(face = "bold", size = 15),
      plot.subtitle = element_text(size = 10)
    )
  
} else {
  
  fig_composta <- (g_area / g_prop / g_metricas) +
    patchwork::plot_annotation(
      title = expression("Síntese final das incertezas em SDM para " * italic("Dinizia excelsa")),
      subtitle = "Classes de robustez, proporções espaciais e métricas médias"
    ) &
    theme(
      plot.title = element_text(face = "bold", size = 15),
      plot.subtitle = element_text(size = 10)
    )
}

ggsave(
  "figuras/unidade16/unidade16_sintese_incertezas_patchwork.png",
  plot = fig_composta,
  width = 10,
  height = 18,
  dpi = 600
)

# ------------------------------------------------------------
# 15. Mensagem final
# ------------------------------------------------------------

message("Síntese de incertezas concluída com sucesso.")

print(sintese)
