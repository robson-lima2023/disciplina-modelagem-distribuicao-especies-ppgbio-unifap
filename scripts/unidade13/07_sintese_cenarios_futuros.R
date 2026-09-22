source("scripts/_bootstrap.R")

# ============================================================
# Unidade 13 - Projeções climáticas futuras em SDM
# Script 07: Síntese dos cenários futuros
# ============================================================

# ------------------------------------------------------------
# 1. Pacotes
# ------------------------------------------------------------

pacotes <- c(
  "dplyr",
  "readr",
  "ggplot2",
  "terra",
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

dir.create("figuras/unidade13", recursive = TRUE, showWarnings = FALSE)
dir.create("tabelas/unidade13", recursive = TRUE, showWarnings = FALSE)
dir.create("dados/unidade13/processados", recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# 4. Entradas
# ------------------------------------------------------------

arquivo_classes <- "dados/unidade13/processados/estabilidade_perda_ganho.csv"

arquivo_ref <- "resultados/unidade11/ensemble_media_simples_dinizia.tif"

if (!file.exists(arquivo_classes)) {
  stop("Arquivo de classes não encontrado. Execute primeiro o Script 06 da Unidade 13.")
}

if (!file.exists(arquivo_ref)) {
  stop("Raster de referência do ensemble atual não encontrado. Execute a Unidade 11.")
}

classes <- readr::read_csv(
  arquivo_classes,
  show_col_types = FALSE
)

if (!all(c("lon", "lat", "cenario", "classe_nome") %in% names(classes))) {
  stop("O arquivo de classes precisa conter: lon, lat, cenario e classe_nome.")
}

# ------------------------------------------------------------
# 5. Estimar área por célula em km²
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
# 6. Associar área às classes
# ------------------------------------------------------------

classes_area <- classes |>
  left_join(
    area_df,
    by = c("lon", "lat")
  ) |>
  filter(
    !is.na(area_km2),
    !is.na(classe_nome),
    !is.na(cenario)
  ) |>
  mutate(
    classe_nome = factor(
      classe_nome,
      levels = c(
        "Inadequado estável",
        "Perda potencial",
        "Ganho potencial",
        "Adequado estável"
      )
    ),
    cenario = factor(
      cenario,
      levels = c("ssp126", "ssp245", "ssp370", "ssp585")
    )
  )

write_csv(
  classes_area,
  "dados/unidade13/processados/estabilidade_perda_ganho_area.csv"
)

# ------------------------------------------------------------
# 7. Síntese por cenário e classe
# ------------------------------------------------------------

sintese <- classes_area |>
  group_by(
    cenario,
    classe_nome
  ) |>
  summarise(
    area_km2 = sum(area_km2, na.rm = TRUE),
    n_pixels = n(),
    .groups = "drop"
  ) |>
  group_by(cenario) |>
  mutate(
    area_total_km2 = sum(area_km2, na.rm = TRUE),
    proporcao = area_km2 / area_total_km2,
    area_mil_km2 = area_km2 / 1000
  ) |>
  ungroup()

readr::write_csv(
  sintese,
  "tabelas/unidade13/sintese_area_classes_futuras.csv"
)

# ------------------------------------------------------------
# 8. Síntese ampla por cenário
# ------------------------------------------------------------

sintese_ampla <- sintese |>
  select(
    cenario,
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
  sintese_ampla,
  "tabelas/unidade13/sintese_area_classes_futuras_wide.csv"
)

# ------------------------------------------------------------
# 9. Cores das classes
# ------------------------------------------------------------

cores_classes <- c(
  "Inadequado estável" = "grey85",
  "Perda potencial" = "#B2182B",
  "Ganho potencial" = "#2166AC",
  "Adequado estável" = "#1B7837"
)

# ------------------------------------------------------------
# 10. Figura: área absoluta por classe
# ------------------------------------------------------------

g_area <- ggplot(
  sintese,
  aes(
    x = cenario,
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
    title = expression("Síntese de área futura para " * italic("Dinizia excelsa")),
    subtitle = "Classes de estabilidade, perda e ganho sob cenários climáticos futuros",
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
  "figuras/unidade13/unidade13_sintese_area_adequada.png",
  plot = g_area,
  width = 9,
  height = 6,
  dpi = 600
)

# ------------------------------------------------------------
# 11. Figura: proporção por classe
# ------------------------------------------------------------

g_prop <- ggplot(
  sintese,
  aes(
    x = cenario,
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
    title = expression("Proporção de classes futuras para " * italic("Dinizia excelsa")),
    subtitle = "Distribuição relativa das classes de estabilidade, perda e ganho",
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
  "figuras/unidade13/unidade13_sintese_proporcao_classes.png",
  plot = g_prop,
  width = 9,
  height = 6,
  dpi = 600
)

# ------------------------------------------------------------
# 12. Figura: foco em perda, ganho e área adequada estável
# ------------------------------------------------------------

sintese_foco <- sintese |>
  filter(
    classe_nome %in% c(
      "Perda potencial",
      "Ganho potencial",
      "Adequado estável"
    )
  )

g_foco <- ggplot(
  sintese_foco,
  aes(
    x = cenario,
    y = area_km2,
    fill = classe_nome
  )
) +
  geom_col(
    position = "dodge",
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
    title = expression("Áreas de perda, ganho e estabilidade adequada para " * italic("Dinizia excelsa")),
    subtitle = "Comparação direta das classes mais relevantes para interpretação biogeográfica",
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
  "figuras/unidade13/unidade13_perda_ganho_estabilidade_area.png",
  plot = g_foco,
  width = 10,
  height = 6,
  dpi = 600
)

# ------------------------------------------------------------
# 13. Tabela-resumo textual
# ------------------------------------------------------------

resumo_texto <- sintese |>
  group_by(cenario) |>
  summarise(
    area_total_km2 = unique(area_total_km2),
    area_perda_km2 = sum(area_km2[classe_nome == "Perda potencial"], na.rm = TRUE),
    area_ganho_km2 = sum(area_km2[classe_nome == "Ganho potencial"], na.rm = TRUE),
    area_adequado_estavel_km2 = sum(area_km2[classe_nome == "Adequado estável"], na.rm = TRUE),
    area_inadequado_estavel_km2 = sum(area_km2[classe_nome == "Inadequado estável"], na.rm = TRUE),
    prop_perda = area_perda_km2 / area_total_km2,
    prop_ganho = area_ganho_km2 / area_total_km2,
    prop_adequado_estavel = area_adequado_estavel_km2 / area_total_km2,
    prop_inadequado_estavel = area_inadequado_estavel_km2 / area_total_km2,
    balanco_ganho_perda_km2 = area_ganho_km2 - area_perda_km2,
    .groups = "drop"
  )

readr::write_csv(
  resumo_texto,
  "tabelas/unidade13/resumo_textual_cenarios_futuros.csv"
)

# ------------------------------------------------------------
# 14. Mensagem final
# ------------------------------------------------------------

message("Síntese dos cenários futuros concluída com sucesso.")
message("Cenários avaliados: ", paste(levels(classes_area$cenario), collapse = ", "))

print(resumo_texto)

