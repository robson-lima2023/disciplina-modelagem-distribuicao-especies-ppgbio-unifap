source("scripts/_bootstrap.R")

# ============================================================
# Unidade 12 - Avaliação de modelos em SDM
# Script 02: Preparar predições para avaliação integrada
# ============================================================

# ------------------------------------------------------------
# 1. Pacotes
# ------------------------------------------------------------

pacotes <- c(
  "dplyr",
  "readr",
  "tidyr",
  "tibble",
  "purrr"
)

require_packages(pacotes)
invisible(lapply(pacotes, library, character.only = TRUE))

# ------------------------------------------------------------
# 2. Diretório raiz do projeto
# ------------------------------------------------------------
# ------------------------------------------------------------
# 3. Diretórios de saída
# ------------------------------------------------------------

dir.create("dados/unidade12/processados", recursive = TRUE, showWarnings = FALSE)
dir.create("tabelas/unidade12", recursive = TRUE, showWarnings = FALSE)
dir.create("resultados/unidade12", recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# 4. Arquivos de predição dos modelos
# ------------------------------------------------------------

arquivos_predicoes <- tibble::tibble(
  modelo = c("GLM", "GAM", "RF", "BRT", "MaxEnt"),
  arquivo = c(
    "dados/unidade06/processados/teste_predicoes_glm_dinizia.csv",
    "dados/unidade07/processados/teste_predicoes_gam_dinizia.csv",
    "dados/unidade08/processados/teste_predicoes_rf_dinizia.csv",
    "dados/unidade09/processados/teste_predicoes_brt_dinizia.csv",
    "dados/unidade10/processados/teste_predicoes_maxent_dinizia.csv"
  ),
  coluna_pred = c(
    "pred_glm",
    "pred_gam",
    "pred_rf",
    "pred_brt",
    "pred_maxent"
  )
)

# ------------------------------------------------------------
# 5. Função para ler predições
# ------------------------------------------------------------

ler_predicao <- function(arquivo, modelo, coluna_pred) {
  
  if (!file.exists(arquivo)) {
    warning("Arquivo não encontrado para ", modelo, ": ", arquivo)
    return(NULL)
  }
  
  tab <- readr::read_csv(
    arquivo,
    show_col_types = FALSE
  )
  
  colunas_obrigatorias <- c("lon", "lat", "pa", coluna_pred)
  
  faltantes <- setdiff(colunas_obrigatorias, names(tab))
  
  if (length(faltantes) > 0) {
    warning(
      "Colunas ausentes no arquivo de ",
      modelo,
      ": ",
      paste(faltantes, collapse = ", ")
    )
    return(NULL)
  }
  
  if (!"tipo" %in% names(tab)) {
    tab <- tab |>
      mutate(
        tipo = ifelse(pa == 1, "presenca", "background")
      )
  }
  
  tab |>
    select(
      lon,
      lat,
      pa,
      tipo,
      pred = all_of(coluna_pred)
    ) |>
    mutate(
      modelo = modelo,
      pa = as.numeric(pa),
      pred = as.numeric(pred),
      tipo = as.character(tipo)
    ) |>
    filter(
      !is.na(lon),
      !is.na(lat),
      !is.na(pa),
      !is.na(pred),
      is.finite(pred),
      pa %in% c(0, 1)
    ) |>
    mutate(
      pred = pmin(pmax(pred, 0), 1)
    )
}

# ------------------------------------------------------------
# 6. Ler predições individuais
# ------------------------------------------------------------

predicoes <- purrr::pmap_dfr(
  arquivos_predicoes,
  function(modelo, arquivo, coluna_pred) {
    ler_predicao(
      arquivo = arquivo,
      modelo = modelo,
      coluna_pred = coluna_pred
    )
  }
)

if (nrow(predicoes) == 0) {
  stop("Nenhuma predição de teste foi encontrada. Execute as Unidades 6 a 10.")
}

# ------------------------------------------------------------
# 7. Ensemble média no conjunto de teste
# ------------------------------------------------------------

ensemble_media_teste <- predicoes |>
  group_by(lon, lat, pa, tipo) |>
  summarise(
    pred = mean(pred, na.rm = TRUE),
    n_modelos = n_distinct(modelo),
    .groups = "drop"
  ) |>
  mutate(
    modelo = "Ensemble_media"
  )

# ------------------------------------------------------------
# 8. Ensemble ponderado no conjunto de teste
# ------------------------------------------------------------

arquivo_pesos <- "tabelas/unidade11/pesos_ensemble_ponderado.csv"

ensemble_ponderado_teste <- NULL

if (file.exists(arquivo_pesos)) {
  
  pesos <- read_csv(
    arquivo_pesos,
    show_col_types = FALSE
  ) |>
    select(modelo, peso)
  
  ensemble_ponderado_teste <- predicoes |>
    left_join(
      pesos,
      by = "modelo"
    ) |>
    mutate(
      peso = ifelse(is.na(peso), 0, peso)
    ) |>
    group_by(lon, lat, pa, tipo) |>
    summarise(
      pred = ifelse(
        sum(peso, na.rm = TRUE) > 0,
        weighted.mean(pred, w = peso, na.rm = TRUE),
        mean(pred, na.rm = TRUE)
      ),
      n_modelos = n_distinct(modelo),
      .groups = "drop"
    ) |>
    mutate(
      modelo = "Ensemble_ponderado"
    )
  
} else {
  
  warning("Arquivo de pesos do ensemble ponderado não encontrado. Apenas ensemble média será criado.")
  
}

# ------------------------------------------------------------
# 9. Consolidar predições finais
# ------------------------------------------------------------

predicoes_individuais <- predicoes |>
  mutate(
    n_modelos = 1
  )

predicoes_final <- bind_rows(
  predicoes_individuais,
  ensemble_media_teste,
  ensemble_ponderado_teste
) |>
  mutate(
    modelo = factor(
      modelo,
      levels = c(
        "GLM",
        "GAM",
        "RF",
        "BRT",
        "MaxEnt",
        "Ensemble_media",
        "Ensemble_ponderado"
      )
    )
  ) |>
  arrange(modelo, desc(pa))

write_csv(
  predicoes_final,
  "dados/unidade12/processados/predicoes_teste_modelos.csv"
)

# ------------------------------------------------------------
# 10. Resumo das predições
# ------------------------------------------------------------

resumo <- predicoes_final |>
  group_by(modelo) |>
  summarise(
    n = n(),
    presencas = sum(pa == 1),
    background = sum(pa == 0),
    pred_min = min(pred, na.rm = TRUE),
    pred_q25 = quantile(pred, 0.25, na.rm = TRUE),
    pred_mediana = median(pred, na.rm = TRUE),
    pred_media = mean(pred, na.rm = TRUE),
    pred_q75 = quantile(pred, 0.75, na.rm = TRUE),
    pred_max = max(pred, na.rm = TRUE),
    pred_sd = sd(pred, na.rm = TRUE),
    n_modelos_medio = mean(n_modelos, na.rm = TRUE),
    .groups = "drop"
  )

write_csv(
  resumo,
  "tabelas/unidade12/resumo_predicoes_teste.csv"
)

# ------------------------------------------------------------
# 11. Checagem de consistência por modelo
# ------------------------------------------------------------

checagem <- predicoes_final |>
  group_by(modelo) |>
  summarise(
    possui_presenca = any(pa == 1),
    possui_background = any(pa == 0),
    predicao_constante = sd(pred, na.rm = TRUE) == 0,
    n_predicoes_na = sum(is.na(pred)),
    .groups = "drop"
  )

write_csv(
  checagem,
  "tabelas/unidade12/checagem_predicoes_teste.csv"
)

# ------------------------------------------------------------
# 12. Metadados
# ------------------------------------------------------------

metadados <- arquivos_predicoes |>
  mutate(
    arquivo_existe = file.exists(arquivo)
  )

write_csv(
  metadados,
  "tabelas/unidade12/metadados_predicoes_modelos.csv"
)

# ------------------------------------------------------------
# 13. Mensagem final
# ------------------------------------------------------------

message("Predições de teste preparadas para avaliação integrada.")
message("Modelos incluídos: ", paste(unique(as.character(predicoes_final$modelo)), collapse = ", "))

print(resumo)
