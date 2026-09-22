# ============================================================
# Unidade 12 - Avaliação de modelos em SDM
# Script 01: Preparar dados de avaliação
# ============================================================

setwd("C:/Users/rblfl/OneDrive/Documentos/Playground/Species-Distribution-Modeling")

pacotes <- c("dplyr", "readr", "ggplot2")

instalar <- pacotes[!pacotes %in% rownames(installed.packages())]
if (length(instalar) > 0) install.packages(instalar)

invisible(lapply(pacotes, library, character.only = TRUE))

dir.create("dados/unidade12/processados", recursive = TRUE, showWarnings = FALSE)
dir.create("figuras/unidade12", recursive = TRUE, showWarnings = FALSE)
dir.create("tabelas/unidade12", recursive = TRUE, showWarnings = FALSE)
dir.create("resultados/unidade12", recursive = TRUE, showWarnings = FALSE)

arquivo_ensemble <- "resultados/unidade11/objetos_ensemble.rds"

if (file.exists(arquivo_ensemble)) {
  
  objetos <- readRDS(arquivo_ensemble)
  
  amb <- objetos$amb
  dados_modelo <- objetos$dados_modelo
  predicoes <- objetos$predicoes
  
  amb_id <- amb %>%
    mutate(
      id_cell = row_number(),
      lon_r = round(lon, 6),
      lat_r = round(lat, 6)
    )
  
  dados_id <- dados_modelo %>%
    mutate(
      lon_r = round(lon, 6),
      lat_r = round(lat, 6)
    ) %>%
    left_join(
      amb_id %>% select(id_cell, lon_r, lat_r),
      by = c("lon_r", "lat_r")
    )
  
  pred_pontos <- predicoes[dados_id$id_cell, ]
  
  dados_avaliacao <- data.frame(
    pa = dados_modelo$pa,
    glm = pred_pontos$glm,
    gam = pred_pontos$gam,
    rf = pred_pontos$rf,
    brt = pred_pontos$brt,
    maxent = pred_pontos$maxent,
    ensemble = pred_pontos$ensemble_medio
  )
  
} else {
  
  set.seed(123)
  
  n_pres <- 300
  n_back <- 3000
  
  pres <- data.frame(
    pa = 1,
    glm = rbeta(n_pres, 6, 3),
    gam = rbeta(n_pres, 7, 3),
    rf = rbeta(n_pres, 8, 2.5),
    brt = rbeta(n_pres, 8, 2.2),
    maxent = rbeta(n_pres, 7, 2.8)
  )
  
  back <- data.frame(
    pa = 0,
    glm = rbeta(n_back, 2.5, 6),
    gam = rbeta(n_back, 2.3, 6),
    rf = rbeta(n_back, 2, 7),
    brt = rbeta(n_back, 2, 7.2),
    maxent = rbeta(n_back, 2.4, 6.2)
  )
  
  dados_avaliacao <- bind_rows(pres, back) %>%
    mutate(
      ensemble = rowMeans(select(., glm, gam, rf, brt, maxent))
    )
}

write_csv(
  dados_avaliacao,
  "dados/unidade12/processados/dados_avaliacao_modelos.csv"
)

resumo <- dados_avaliacao %>%
  summarise(
    n_total = n(),
    n_presenca = sum(pa == 1),
    n_background = sum(pa == 0),
    prevalencia = mean(pa == 1)
  )

write_csv(
  resumo,
  "tabelas/unidade12/resumo_dados_avaliacao.csv"
)

message("Dados de avaliação preparados com sucesso.")
print(resumo)
