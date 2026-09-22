source("scripts/_bootstrap.R")

# ============================================================
# Unidade 14 - Transferência, extrapolação, MESS e MOP
# Script 06: Síntese da extrapolação
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
  "scales"
)

require_packages(pacotes)
invisible(lapply(pacotes, library, character.only = TRUE))

# ------------------------------------------------------------
# 2. Diretório raiz
# ------------------------------------------------------------
# ------------------------------------------------------------
# 3. Diretórios
# ------------------------------------------------------------

dir.create("figuras/unidade14", recursive = TRUE, showWarnings = FALSE)
dir.create("tabelas/unidade14", recursive = TRUE, showWarnings = FALSE)
dir.create("dados/unidade14/processados", recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# 4. Arquivos de entrada
# ------------------------------------------------------------

arquivo <- "dados/unidade14/processados/adequabilidade_extrapolacao.csv"

arquivo_ref <- "resultados/unidade11/ensemble_media_simples_dinizia.tif"

if (!file.exists(arquivo)) {
  stop("Arquivo de adequabilidade/extrapolação não encontrado. Execute o Script 05 da Unidade 14.")
}

if (!file.exists(arquivo_ref)) {
  stop("Raster de referência do ensemble atual não encontrado. Execute a Unidade 11.")
}

# ------------------------------------------------------------
# 5. Leitura dos dados
# ------------------------------------------------------------

dados <- readr::read_csv(
  arquivo,
  show_col_types = FALSE
) |>
  dplyr::mutate(
    cenario = as.character(cenario),
    classe = as.character(classe),
    adequabilidade = as.numeric(adequabilidade)
  ) |>
  dplyr::filter(
    !is.na(lon),
    !is.na(lat),
    !is.na(cenario),
    !is.na(classe),
    !is.na(adequabilidade)
  )

if (nrow(dados) == 0) {
  stop("A tabela de adequabilidade/extrapolação está vazia após remover NA.")
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

# ------------------------------------------------------------
# 7. Associar área às classes
# ------------------------------------------------------------

dados_area <- dados |>
  dplyr::left_join(
    area_df,
    by = c("lon", "lat")
  ) |>
  dplyr::filter(
    !is.na(area_km2)
  ) |>
  dplyr::mutate(
    cenario = factor(
      cenario,
      levels = c("ssp126", "ssp245", "ssp370", "ssp585")
    ),
    classe = factor(
      classe,
      levels = c("Sem extrapolação", "Sob extrapolação")
    )
  )

readr::write_csv(
  dados_area,
  "dados/unidade14/processados/adequabilidade_extrapolacao_area.csv"
)

# ------------------------------------------------------------
# 8. Síntese por cenário e classe
# ------------------------------------------------------------

sintese <- dados_area |>
  dplyr::group_by(
    cenario,
    classe
  ) |>
  dplyr::summarise(
    area_km2 = sum(area_km2, na.rm = TRUE),
    area_mil_km2 = area_km2 / 1000,
    n_pixels = dplyr::n(),
    adequabilidade_media = mean(adequabilidade, na.rm = TRUE),
    adequabilidade_mediana = median(adequabilidade, na.rm = TRUE),
    adequabilidade_q25 = quantile(adequabilidade, 0.25, na.rm = TRUE),
    adequabilidade_q75 = quantile(adequabilidade, 0.75, na.rm = TRUE),
    adequabilidade_max = max(adequabilidade, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::group_by(cenario) |>
  dplyr::mutate(
    area_total_km2 = sum(area_km2, na.rm = TRUE),
    prop_area = area_km2 / area_total_km2
  ) |>
  dplyr::ungroup()

readr::write_csv(
  sintese,
  "tabelas/unidade14/sintese_extrapolacao_cenarios.csv"
)

# ------------------------------------------------------------
# 9. Tabela ampla para interpretação
# ------------------------------------------------------------

sintese_wide <- sintese |>
  dplyr::select(
    cenario,
    classe,
    area_km2,
    prop_area,
    adequabilidade_media,
    adequabilidade_mediana
  ) |>
  tidyr::pivot_wider(
    names_from = classe,
    values_from = c(
      area_km2,
      prop_area,
      adequabilidade_media,
      adequabilidade_mediana
    ),
    values_fill = 0
  )

readr::write_csv(
  sintese_wide,
  "tabelas/unidade14/sintese_extrapolacao_cenarios_wide.csv"
)

# ------------------------------------------------------------
# 10. Resumo textual por cenário
# ------------------------------------------------------------

resumo_textual <- sintese |>
  dplyr::group_by(cenario) |>
  dplyr::summarise(
    area_total_km2 = unique(area_total_km2),
    area_sem_extrapolacao_km2 = sum(area_km2[classe == "Sem extrapolação"], na.rm = TRUE),
    area_sob_extrapolacao_km2 = sum(area_km2[classe == "Sob extrapolação"], na.rm = TRUE),
    prop_sem_extrapolacao = area_sem_extrapolacao_km2 / area_total_km2,
    prop_sob_extrapolacao = area_sob_extrapolacao_km2 / area_total_km2,
    adequabilidade_media_sem_extrapolacao = mean(
      adequabilidade_media[classe == "Sem extrapolação"],
      na.rm = TRUE
    ),
    adequabilidade_media_sob_extrapolacao = mean(
      adequabilidade_media[classe == "Sob extrapolação"],
      na.rm = TRUE
    ),
    .groups = "drop"
  )

readr::write_csv(
  resumo_textual,
  "tabelas/unidade14/resumo_textual_extrapolacao_cenarios.csv"
)

# ------------------------------------------------------------
# 11. Cores
# ------------------------------------------------------------

cores_extrapolacao <- c(
  "Sem extrapolação" = "#1B7837",
  "Sob extrapolação" = "#B2182B"
)

# ------------------------------------------------------------
# 12. Figura: proporção de área
# ------------------------------------------------------------

g_prop <- ggplot(
  sintese,
  aes(
    x = cenario,
    y = prop_area,
    fill = classe
  )
) +
  geom_col(
    color = "grey25",
    linewidth = 0.15
  ) +
  scale_fill_manual(
    values = cores_extrapolacao,
    drop = FALSE,
    name = "Classe"
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1)
  ) +
  labs(
    title = expression("Síntese da extrapolação futura para " * italic("Dinizia excelsa")),
    subtitle = "Proporção da área projetada com e sem extrapolação ambiental combinada por MESS e MOP",
    x = "Cenário climático",
    y = "Proporção da área"
  ) +
  theme_bw(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

ggsave(
  "figuras/unidade14/unidade14_sintese_extrapolacao.png",
  plot = g_prop,
  width = 9,
  height = 6,
  dpi = 600
)

# ------------------------------------------------------------
# 13. Figura: área absoluta
# ------------------------------------------------------------

g_area <- ggplot(
  sintese,
  aes(
    x = cenario,
    y = area_km2,
    fill = classe
  )
) +
  geom_col(
    color = "grey25",
    linewidth = 0.15
  ) +
  scale_fill_manual(
    values = cores_extrapolacao,
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
    title = expression("Área sob extrapolação futura para " * italic("Dinizia excelsa")),
    subtitle = "Área absoluta classificada com e sem extrapolação ambiental combinada",
    x = "Cenário climático",
    y = expression("Área (km"^2*")")
  ) +
  theme_bw(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

ggsave(
  "figuras/unidade14/unidade14_area_extrapolacao.png",
  plot = g_area,
  width = 9,
  height = 6,
  dpi = 600
)

# ------------------------------------------------------------
# 14. Figura: adequabilidade média por classe
# ------------------------------------------------------------

g_adequabilidade <- ggplot(
  sintese,
  aes(
    x = cenario,
    y = adequabilidade_media,
    fill = classe
  )
) +
  geom_col(
    position = "dodge",
    color = "grey25",
    linewidth = 0.15
  ) +
  scale_fill_manual(
    values = cores_extrapolacao,
    drop = FALSE,
    name = "Classe"
  ) +
  scale_y_continuous(
    limits = c(0, 1)
  ) +
  labs(
    title = expression("Adequabilidade média sob extrapolação para " * italic("Dinizia excelsa")),
    subtitle = "Comparação entre áreas com e sem extrapolação ambiental",
    x = "Cenário climático",
    y = "Adequabilidade média"
  ) +
  theme_bw(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

ggsave(
  "figuras/unidade14/unidade14_adequabilidade_media_extrapolacao.png",
  plot = g_adequabilidade,
  width = 9,
  height = 6,
  dpi = 600
)

# ------------------------------------------------------------
# 15. Mensagem final
# ------------------------------------------------------------

message("Síntese da extrapolação concluída com sucesso.")
print(resumo_textual)

