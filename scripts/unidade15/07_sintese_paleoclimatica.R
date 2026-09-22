source("scripts/_bootstrap.R")

# ============================================================
# Unidade 15 - Paleoclima e nicho climático passado
# Script 07: Síntese paleoclimática
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

dir.create("figuras/unidade15", recursive = TRUE, showWarnings = FALSE)
dir.create("tabelas/unidade15", recursive = TRUE, showWarnings = FALSE)
dir.create("dados/unidade15/processados", recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# 4. Entradas
# ------------------------------------------------------------

arquivo <- "dados/unidade15/processados/estabilidade_refugios_paleoclimaticos.csv"

arquivo_ref <- "resultados/unidade11/ensemble_media_simples_dinizia.tif"

arquivo_ensemble_passado <- "dados/unidade15/processados/ensemble_passado_todos_periodos.csv"

if (!file.exists(arquivo)) {
  stop("Arquivo de estabilidade não encontrado. Execute o Script 06 da Unidade 15.")
}

if (!file.exists(arquivo_ref)) {
  stop("Raster de referência atual não encontrado. Execute a Unidade 11.")
}

# ------------------------------------------------------------
# 5. Ler dados
# ------------------------------------------------------------

dados <- readr::read_csv(
  arquivo,
  show_col_types = FALSE
) |>
  dplyr::mutate(
    periodo = as.character(periodo),
    periodo_legenda = as.character(periodo_legenda),
    classe_nome = as.character(classe_nome)
  ) |>
  dplyr::filter(
    !is.na(lon),
    !is.na(lat),
    !is.na(periodo),
    !is.na(classe_nome)
  )

if (nrow(dados) == 0) {
  stop("A tabela de estabilidade está vazia.")
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

dados_area <- dados |>
  dplyr::left_join(
    area_df,
    by = c("lon", "lat")
  ) |>
  dplyr::filter(
    !is.na(area_km2)
  ) |>
  dplyr::mutate(
    periodo_legenda = factor(
      periodo_legenda,
      levels = c(
        "Último Interglacial",
        "Último Máximo Glacial",
        "Holoceno Médio"
      )
    ),
    classe_nome = factor(
      classe_nome,
      levels = c(
        "Inadequado em ambos",
        "Adequado apenas passado",
        "Adequado apenas atual",
        "Estável adequado"
      )
    )
  )

readr::write_csv(
  dados_area,
  "dados/unidade15/processados/estabilidade_refugios_area.csv"
)

# ------------------------------------------------------------
# 7. Síntese por período e classe
# ------------------------------------------------------------

sintese <- dados_area |>
  dplyr::group_by(
    periodo,
    periodo_legenda,
    classe_nome
  ) |>
  dplyr::summarise(
    area_km2 = sum(area_km2, na.rm = TRUE),
    area_mil_km2 = area_km2 / 1000,
    n_pixels = dplyr::n(),
    .groups = "drop"
  ) |>
  dplyr::group_by(periodo, periodo_legenda) |>
  dplyr::mutate(
    area_total_km2 = sum(area_km2, na.rm = TRUE),
    proporcao = area_km2 / area_total_km2
  ) |>
  dplyr::ungroup()

readr::write_csv(
  sintese,
  "tabelas/unidade15/sintese_paleoclimatica_area.csv"
)

# ------------------------------------------------------------
# 8. Tabela ampla
# ------------------------------------------------------------

sintese_wide <- sintese |>
  dplyr::select(
    periodo,
    periodo_legenda,
    classe_nome,
    area_km2,
    proporcao
  ) |>
  tidyr::pivot_wider(
    names_from = classe_nome,
    values_from = c(area_km2, proporcao),
    values_fill = 0
  )

readr::write_csv(
  sintese_wide,
  "tabelas/unidade15/sintese_paleoclimatica_area_wide.csv"
)

# ------------------------------------------------------------
# 9. Resumo textual
# ------------------------------------------------------------

resumo_textual <- sintese |>
  dplyr::group_by(
    periodo,
    periodo_legenda
  ) |>
  dplyr::summarise(
    area_total_km2 = unique(area_total_km2),
    area_refugio_km2 = sum(area_km2[classe_nome == "Estável adequado"], na.rm = TRUE),
    area_apenas_passado_km2 = sum(area_km2[classe_nome == "Adequado apenas passado"], na.rm = TRUE),
    area_apenas_atual_km2 = sum(area_km2[classe_nome == "Adequado apenas atual"], na.rm = TRUE),
    area_inadequado_ambos_km2 = sum(area_km2[classe_nome == "Inadequado em ambos"], na.rm = TRUE),
    prop_refugio = area_refugio_km2 / area_total_km2,
    prop_apenas_passado = area_apenas_passado_km2 / area_total_km2,
    prop_apenas_atual = area_apenas_atual_km2 / area_total_km2,
    prop_inadequado_ambos = area_inadequado_ambos_km2 / area_total_km2,
    .groups = "drop"
  )

readr::write_csv(
  resumo_textual,
  "tabelas/unidade15/resumo_textual_paleoclimatico.csv"
)

# ------------------------------------------------------------
# 10. Integrar adequabilidade média dos ensembles passados
# ------------------------------------------------------------

if (file.exists(arquivo_ensemble_passado)) {
  
  ens_passado <- readr::read_csv(
    arquivo_ensemble_passado,
    show_col_types = FALSE
  )
  
  resumo_ensemble <- ens_passado |>
    dplyr::group_by(
      periodo,
      periodo_legenda
    ) |>
    dplyr::summarise(
      adequabilidade_media = mean(adequabilidade, na.rm = TRUE),
      adequabilidade_mediana = median(adequabilidade, na.rm = TRUE),
      incerteza_media = mean(incerteza, na.rm = TRUE),
      .groups = "drop"
    )
  
  readr::write_csv(
    resumo_ensemble,
    "tabelas/unidade15/resumo_adequabilidade_ensemble_passado.csv"
  )
  
} else {
  
  resumo_ensemble <- NULL
  
}

# ------------------------------------------------------------
# 11. Cores
# ------------------------------------------------------------

cores_classes <- c(
  "Inadequado em ambos" = "grey85",
  "Adequado apenas passado" = "#4575B4",
  "Adequado apenas atual" = "#FDAE61",
  "Estável adequado" = "#1A9850"
)

# ------------------------------------------------------------
# 12. Figura: área absoluta
# ------------------------------------------------------------

g_area <- ggplot(
  sintese,
  aes(
    x = periodo_legenda,
    y = area_km2,
    fill = classe_nome
  )
) +
  geom_col(
    color = "grey25",
    linewidth = 0.15
  ) +
  scale_fill_manual(
    values = cores_classes,
    drop = FALSE,
    name = "Classe"
  ) +
  scale_y_continuous(
    labels = scales::label_number(
      big.mark = ".",
      decimal.mark = ","
    )
  ) +
  labs(
    title = expression("Síntese paleoclimática para " * italic("Dinizia excelsa")),
    subtitle = "Área estimada por classe histórica de adequabilidade",
    x = "Período paleoclimático",
    y = expression("Área (km"^2*")")
  ) +
  theme_bw(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10),
    axis.text.x = element_text(angle = 20, hjust = 1),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

ggsave(
  "figuras/unidade15/unidade15_sintese_paleoclimatica.png",
  plot = g_area,
  width = 10,
  height = 6,
  dpi = 600
)

# ------------------------------------------------------------
# 13. Figura: proporções
# ------------------------------------------------------------

g_prop <- ggplot(
  sintese,
  aes(
    x = periodo_legenda,
    y = proporcao,
    fill = classe_nome
  )
) +
  geom_col(
    color = "grey25",
    linewidth = 0.15
  ) +
  scale_fill_manual(
    values = cores_classes,
    drop = FALSE,
    name = "Classe"
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1)
  ) +
  labs(
    title = expression("Proporção das classes paleoclimáticas para " * italic("Dinizia excelsa")),
    subtitle = "Participação relativa das áreas estáveis, atuais e passadas",
    x = "Período paleoclimático",
    y = "Proporção da área"
  ) +
  theme_bw(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10),
    axis.text.x = element_text(angle = 20, hjust = 1),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

ggsave(
  "figuras/unidade15/unidade15_sintese_paleoclimatica_proporcao.png",
  plot = g_prop,
  width = 10,
  height = 6,
  dpi = 600
)

# ------------------------------------------------------------
# 14. Figura: adequabilidade média, se disponível
# ------------------------------------------------------------

if (!is.null(resumo_ensemble)) {
  
  g_adeq <- ggplot(
    resumo_ensemble,
    aes(
      x = periodo_legenda,
      y = adequabilidade_media
    )
  ) +
    geom_col(
      fill = "grey55",
      color = "grey25",
      linewidth = 0.15
    ) +
    geom_errorbar(
      aes(
        ymin = pmax(0, adequabilidade_media - incerteza_media),
        ymax = pmin(1, adequabilidade_media + incerteza_media)
      ),
      width = 0.15,
      linewidth = 0.40
    ) +
    scale_y_continuous(
      limits = c(0, 1)
    ) +
    labs(
      title = expression("Adequabilidade média paleoclimática de " * italic("Dinizia excelsa")),
      subtitle = "Barras indicam média do ensemble; erro representa incerteza média entre modelos",
      x = "Período paleoclimático",
      y = "Adequabilidade média"
    ) +
    theme_bw(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold", size = 13),
      plot.subtitle = element_text(size = 10),
      axis.text.x = element_text(angle = 20, hjust = 1),
      panel.grid.minor = element_blank()
    )
  
  ggsave(
    "figuras/unidade15/unidade15_adequabilidade_media_paleoclimatica.png",
    plot = g_adeq,
    width = 9,
    height = 6,
    dpi = 600
  )
  
  fig_composta <- (g_area / g_prop / g_adeq) +
    patchwork::plot_annotation(
      title = expression("Síntese histórica da adequabilidade climática de " * italic("Dinizia excelsa")),
      subtitle = "Áreas, proporções e adequabilidade média em períodos paleoclimáticos"
    ) &
    theme(
      plot.title = element_text(face = "bold", size = 15),
      plot.subtitle = element_text(size = 10)
    )
  
} else {
  
  fig_composta <- (g_area / g_prop) +
    patchwork::plot_annotation(
      title = expression("Síntese histórica da adequabilidade climática de " * italic("Dinizia excelsa")),
      subtitle = "Áreas e proporções em períodos paleoclimáticos"
    ) &
    theme(
      plot.title = element_text(face = "bold", size = 15),
      plot.subtitle = element_text(size = 10)
    )
}

ggsave(
  "figuras/unidade15/unidade15_sintese_paleoclimatica_patchwork.png",
  plot = fig_composta,
  width = 11,
  height = 16,
  dpi = 600
)

# ------------------------------------------------------------
# 15. Mensagem final
# ------------------------------------------------------------

message("Síntese paleoclimática concluída com sucesso.")

print(resumo_textual)
