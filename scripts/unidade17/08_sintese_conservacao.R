source("scripts/_bootstrap.R")

# ============================================================
# Unidade 17 - Aplicações à conservação
# Script 08: Síntese quantitativa da conservação
# ============================================================

pacotes <- c(
  "dplyr", "readr", "terra", "ggplot2",
  "tibble", "tidyr", "scales", "patchwork"
)

require_packages(pacotes)
invisible(lapply(pacotes, library, character.only = TRUE))
dir.create("dados/unidade17/processados", recursive = TRUE, showWarnings = FALSE)
dir.create("figuras/unidade17", recursive = TRUE, showWarnings = FALSE)
dir.create("tabelas/unidade17", recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# 1. Arquivos
# ------------------------------------------------------------

arquivo_prioridade <- "resultados/unidade17/prioridade_integrada_conservacao.tif"
arquivo_classes <- "resultados/unidade17/componentes/classes_prioridade_integrada.tif"
arquivo_lacunas <- "resultados/unidade17/componentes/lacunas_protecao.tif"
arquivo_refugios <- "resultados/unidade17/componentes/refugios_climaticos_top10.tif"

if (!file.exists(arquivo_prioridade)) stop("Prioridade integrada não encontrada. Execute o Script 07.")
if (!file.exists(arquivo_classes)) stop("Classes de prioridade não encontradas. Execute o Script 07.")
if (!file.exists(arquivo_lacunas)) stop("Lacunas de proteção não encontradas. Execute o Script 06.")
if (!file.exists(arquivo_refugios)) stop("Refúgios climáticos não encontrados. Execute o Script 05.")

prioridade <- terra::rast(arquivo_prioridade)
classes <- terra::rast(arquivo_classes)
lacunas <- terra::rast(arquivo_lacunas)
refugios <- terra::rast(arquivo_refugios)

if (!terra::compareGeom(prioridade, classes, stopOnError = FALSE)) {
  classes <- terra::resample(classes, prioridade, method = "near")
}

if (!terra::compareGeom(prioridade, lacunas, stopOnError = FALSE)) {
  lacunas <- terra::resample(lacunas, prioridade, method = "near")
}

if (!terra::compareGeom(prioridade, refugios, stopOnError = FALSE)) {
  refugios <- terra::resample(refugios, prioridade, method = "near")
}

names(prioridade) <- "prioridade"
names(classes) <- "classe_prioridade"
names(lacunas) <- "lacuna"
names(refugios) <- "refugio"

# ------------------------------------------------------------
# 2. Área por célula
# ------------------------------------------------------------

area_cell <- terra::cellSize(
  prioridade,
  unit = "km"
)

stack <- c(
  prioridade,
  classes,
  lacunas,
  refugios,
  area_cell
)

names(stack) <- c(
  "prioridade",
  "classe_prioridade",
  "lacuna",
  "refugio",
  "area_km2"
)

df <- as.data.frame(
  stack,
  xy = TRUE,
  na.rm = TRUE
) |>
  dplyr::rename(
    lon = x,
    lat = y
  ) |>
  dplyr::mutate(
    classe_nome = dplyr::case_when(
      classe_prioridade == 4 ~ "Muito alta",
      classe_prioridade == 3 ~ "Alta",
      classe_prioridade == 2 ~ "Moderada",
      classe_prioridade == 1 ~ "Baixa",
      TRUE ~ NA_character_
    ),
    classe_nome = factor(
      classe_nome,
      levels = c("Baixa", "Moderada", "Alta", "Muito alta")
    ),
    lacuna_nome = dplyr::case_when(
      lacuna == 1 ~ "Lacuna de proteção",
      lacuna == 0 ~ "Sem lacuna",
      TRUE ~ NA_character_
    ),
    refugio_nome = dplyr::case_when(
      refugio == 1 ~ "Refúgio climático",
      TRUE ~ "Não refúgio"
    )
  )

readr::write_csv(
  df,
  "dados/unidade17/processados/sintese_conservacao_espacial.csv"
)

# ------------------------------------------------------------
# 3. Síntese por prioridade
# ------------------------------------------------------------

sintese_prioridade <- df |>
  dplyr::group_by(classe_nome) |>
  dplyr::summarise(
    area_km2 = sum(area_km2, na.rm = TRUE),
    prioridade_media = mean(prioridade, na.rm = TRUE),
    area_lacuna_km2 = sum(area_km2[lacuna == 1], na.rm = TRUE),
    area_refugio_km2 = sum(area_km2[refugio == 1], na.rm = TRUE),
    n_pixels = dplyr::n(),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    prop_area = area_km2 / sum(area_km2, na.rm = TRUE),
    prop_lacuna_na_classe = area_lacuna_km2 / area_km2,
    prop_refugio_na_classe = area_refugio_km2 / area_km2
  )

readr::write_csv(
  sintese_prioridade,
  "tabelas/unidade17/sintese_classes_prioridade.csv"
)

# ------------------------------------------------------------
# 4. Síntese geral
# ------------------------------------------------------------

sintese_geral <- tibble::tibble(
  metrica = c(
    "Área total avaliada",
    "Área de prioridade alta ou muito alta",
    "Área de prioridade muito alta",
    "Área em lacuna de proteção",
    "Área de refúgio climático",
    "Prioridade média"
  ),
  valor = c(
    sum(df$area_km2, na.rm = TRUE),
    sum(df$area_km2[df$classe_nome %in% c("Alta", "Muito alta")], na.rm = TRUE),
    sum(df$area_km2[df$classe_nome == "Muito alta"], na.rm = TRUE),
    sum(df$area_km2[df$lacuna == 1], na.rm = TRUE),
    sum(df$area_km2[df$refugio == 1], na.rm = TRUE),
    mean(df$prioridade, na.rm = TRUE)
  )
) |>
  dplyr::mutate(
    unidade = c(
      "km²", "km²", "km²", "km²", "km²", "índice"
    ),
    proporcao_area = dplyr::case_when(
      unidade == "km²" ~ valor / valor[metrica == "Área total avaliada"],
      TRUE ~ NA_real_
    )
  )

readr::write_csv(
  sintese_geral,
  "tabelas/unidade17/sintese_geral_conservacao.csv"
)

# ------------------------------------------------------------
# 5. Cruzamento lacunas x refúgios
# ------------------------------------------------------------

sintese_lacuna_refugio <- df |>
  dplyr::group_by(lacuna_nome, refugio_nome) |>
  dplyr::summarise(
    area_km2 = sum(area_km2, na.rm = TRUE),
    prioridade_media = mean(prioridade, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    prop_area = area_km2 / sum(area_km2, na.rm = TRUE)
  )

readr::write_csv(
  sintese_lacuna_refugio,
  "tabelas/unidade17/sintese_lacunas_refugios.csv"
)

# ------------------------------------------------------------
# 6. Gráficos
# ------------------------------------------------------------

cores_prioridade <- c(
  "Baixa" = "grey85",
  "Moderada" = "#A6D96A",
  "Alta" = "#1A9850",
  "Muito alta" = "#006837"
)

g_area <- ggplot(
  sintese_prioridade,
  aes(
    x = classe_nome,
    y = area_km2,
    fill = classe_nome
  )
) +
  geom_col(
    color = "grey25",
    linewidth = 0.15,
    show.legend = FALSE
  ) +
  scale_fill_manual(values = cores_prioridade, drop = FALSE) +
  scale_y_continuous(
    labels = scales::label_number(
      big.mark = ".",
      decimal.mark = ","
    )
  ) +
  labs(
    title = expression("Área por classe de prioridade para " * italic("Dinizia excelsa")),
    x = "Classe de prioridade",
    y = expression("Área (km"^2*")")
  ) +
  theme_bw(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

g_lacuna <- ggplot(
  sintese_prioridade,
  aes(
    x = classe_nome,
    y = prop_lacuna_na_classe,
    fill = classe_nome
  )
) +
  geom_col(
    color = "grey25",
    linewidth = 0.15,
    show.legend = FALSE
  ) +
  scale_fill_manual(values = cores_prioridade, drop = FALSE) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1)
  ) +
  labs(
    title = "Proporção de lacunas por classe de prioridade",
    x = "Classe de prioridade",
    y = "Proporção em lacuna"
  ) +
  theme_bw(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

g_refugio <- ggplot(
  sintese_prioridade,
  aes(
    x = classe_nome,
    y = prop_refugio_na_classe,
    fill = classe_nome
  )
) +
  geom_col(
    color = "grey25",
    linewidth = 0.15,
    show.legend = FALSE
  ) +
  scale_fill_manual(values = cores_prioridade, drop = FALSE) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1)
  ) +
  labs(
    title = "Proporção de refúgios por classe de prioridade",
    x = "Classe de prioridade",
    y = "Proporção em refúgio climático"
  ) +
  theme_bw(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

g_cross <- ggplot(
  sintese_lacuna_refugio,
  aes(
    x = lacuna_nome,
    y = area_km2,
    fill = refugio_nome
  )
) +
  geom_col(
    color = "grey25",
    linewidth = 0.15
  ) +
  scale_fill_manual(
    values = c(
      "Não refúgio" = "grey80",
      "Refúgio climático" = "#1A9850"
    ),
    name = "Classe"
  ) +
  scale_y_continuous(
    labels = scales::label_number(
      big.mark = ".",
      decimal.mark = ","
    )
  ) +
  labs(
    title = "Cruzamento entre lacunas de proteção e refúgios climáticos",
    x = NULL,
    y = expression("Área (km"^2*")")
  ) +
  theme_bw(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

fig <- (g_area + g_lacuna) / (g_refugio + g_cross) +
  patchwork::plot_annotation(
    title = expression("Síntese quantitativa de conservação para " * italic("Dinizia excelsa")),
    subtitle = "Classes de prioridade, lacunas de proteção e refúgios climáticos potenciais"
  ) &
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 10)
  )

ggsave(
  "figuras/unidade17/unidade17_sintese_conservacao.png",
  plot = fig,
  width = 14,
  height = 10,
  dpi = 600
)

# ------------------------------------------------------------
# 7. Tabela simplificada para texto
# ------------------------------------------------------------

tabela_texto <- sintese_prioridade |>
  dplyr::mutate(
    area_km2 = round(area_km2, 2),
    prop_area = round(100 * prop_area, 2),
    prioridade_media = round(prioridade_media, 3),
    prop_lacuna_na_classe = round(100 * prop_lacuna_na_classe, 2),
    prop_refugio_na_classe = round(100 * prop_refugio_na_classe, 2)
  ) |>
  dplyr::rename(
    classe = classe_nome,
    area_percentual = prop_area,
    lacuna_percentual = prop_lacuna_na_classe,
    refugio_percentual = prop_refugio_na_classe
  )

readr::write_csv(
  tabela_texto,
  "tabelas/unidade17/tabela_texto_sintese_conservacao.csv"
)

message("Síntese quantitativa de conservação concluída com sucesso.")
print(sintese_geral)
