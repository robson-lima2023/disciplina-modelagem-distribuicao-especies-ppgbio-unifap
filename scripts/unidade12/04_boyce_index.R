# ============================================================
# Unidade 12 - Avaliação de modelos em SDM
# Script 04: Boyce Index didático
# ============================================================

pacotes <- c("dplyr", "readr", "ggplot2")

instalar <- pacotes[!pacotes %in% rownames(installed.packages())]
if (length(instalar) > 0) install.packages(instalar)

invisible(lapply(pacotes, library, character.only = TRUE))

dados <- read_csv(
  "dados/unidade12/processados/dados_avaliacao_modelos.csv",
  show_col_types = FALSE
)

# ------------------------------------------------------------
# Função didática para Boyce Index
# ------------------------------------------------------------
# A função divide o gradiente de adequabilidade em classes,
# calcula a razão P/E:
# P = frequência de presenças na classe
# E = frequência de valores disponíveis/background na classe
# e calcula a correlação de Spearman entre adequabilidade e P/E.

calcular_boyce <- function(obs, pred, n_bins = 10) {
  
  breaks <- seq(
    min(pred, na.rm = TRUE),
    max(pred, na.rm = TRUE),
    length.out = n_bins + 1
  )
  
  classe <- cut(
    pred,
    breaks = breaks,
    include.lowest = TRUE
  )
  
  tabela <- data.frame(
    obs = obs,
    pred = pred,
    classe = classe
  ) %>%
    group_by(classe) %>%
    summarise(
      pred_medio = mean(pred, na.rm = TRUE),
      n_total = n(),
      n_pres = sum(obs == 1),
      .groups = "drop"
    ) %>%
    mutate(
      freq_disp = n_total / sum(n_total),
      freq_pres = n_pres / sum(n_pres),
      pe_ratio = freq_pres / freq_disp
    ) %>%
    filter(
      is.finite(pe_ratio),
      !is.na(pe_ratio)
    )
  
  boyce <- suppressWarnings(
    cor(
      tabela$pred_medio,
      tabela$pe_ratio,
      method = "spearman"
    )
  )
  
  list(
    boyce = boyce,
    tabela = tabela
  )
}

resultado <- calcular_boyce(
  obs = dados$pa,
  pred = dados$ensemble,
  n_bins = 10
)

boyce_valor <- resultado$boyce
boyce_tabela <- resultado$tabela

write_csv(
  boyce_tabela,
  "tabelas/unidade12/boyce_tabela_pe_ratio.csv"
)

write_csv(
  data.frame(
    modelo = "ensemble",
    boyce_index = boyce_valor
  ),
  "tabelas/unidade12/boyce_index_ensemble.csv"
)

g_boyce <- ggplot(
  boyce_tabela,
  aes(x = pred_medio, y = pe_ratio)
) +
  geom_point(size = 2.5) +
  geom_line(linewidth = 1) +
  labs(
    title = "Boyce Index didático",
    subtitle = paste0("Correlação de Spearman = ", round(boyce_valor, 3)),
    x = "Adequabilidade média da classe",
    y = "Razão P/E"
  ) +
  theme_bw(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

ggsave(
  "figuras/unidade12/unidade12_boyce_plot.png",
  plot = g_boyce,
  width = 7,
  height = 5,
  dpi = 600
)

message("Boyce Index calculado.")
print(data.frame(boyce_index = boyce_valor))
