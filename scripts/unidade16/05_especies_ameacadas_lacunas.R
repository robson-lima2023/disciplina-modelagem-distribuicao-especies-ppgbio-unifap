# ============================================================
# Unidade 16 - Aplicações à Conservação
# Script 05: Espécies ameaçadas e lacunas de proteção
# ============================================================

pacotes <- c("dplyr", "readr", "ggplot2")

instalar <- pacotes[!pacotes %in% rownames(installed.packages())]
if (length(instalar) > 0) install.packages(instalar)

invisible(lapply(pacotes, library, character.only = TRUE))

camadas <- read_csv(
  "dados/unidade16/processados/camadas_conservacao.csv",
  show_col_types = FALSE
)

lacunas <- camadas %>%
  mutate(
    alta_adequabilidade_ameacadas =
      especies_ameacadas >= quantile(especies_ameacadas, 0.85, na.rm = TRUE),
    
    lacuna_protecao =
      alta_adequabilidade_ameacadas & protecao_atual == 0,
    
    classe = case_when(
      protecao_atual == 1 & alta_adequabilidade_ameacadas ~ "Protegida e adequada",
      protecao_atual == 0 & alta_adequabilidade_ameacadas ~ "Lacuna de proteção",
      protecao_atual == 1 & !alta_adequabilidade_ameacadas ~ "Protegida",
      TRUE ~ "Baixa prioridade"
    )
  )

write_csv(
  lacunas,
  "dados/unidade16/processados/lacunas_especies_ameacadas.csv"
)

resumo <- lacunas %>%
  count(classe) %>%
  mutate(proporcao = n / sum(n))

write_csv(
  resumo,
  "tabelas/unidade16/resumo_lacunas_especies_ameacadas.csv"
)

g <- ggplot(
  lacunas,
  aes(x = lon, y = lat, fill = classe)
) +
  geom_raster() +
  coord_equal() +
  labs(
    title = "Lacunas de proteção para espécies ameaçadas",
    x = "Longitude",
    y = "Latitude",
    fill = "Classe"
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid = element_blank()
  )

ggsave(
  "figuras/unidade16/unidade16_lacunas_especies_ameacadas.png",
  plot = g,
  width = 8,
  height = 6,
  dpi = 600
)

message("Lacunas de proteção para espécies ameaçadas avaliadas.")
print(resumo)
