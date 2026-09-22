source("scripts/_bootstrap.R")

# ============================================================
# Unidade 4 - Multicolinearidade em SDM
# Script 05: Seleção final de variáveis ambientais
# ============================================================

# ------------------------------------------------------------
# 1. Pacotes
# ------------------------------------------------------------

pacotes <- c("dplyr", "readr", "ggplot2", "tibble")

require_packages(pacotes)
invisible(lapply(pacotes, library, character.only = TRUE))

# ------------------------------------------------------------
# 2. Diretório do projeto
# ------------------------------------------------------------
# ------------------------------------------------------------
# 3. Ler variáveis mantidas pelo VIF
# ------------------------------------------------------------

arquivo_vif <- "resultados/unidade04/variaveis_mantidas_vif.csv"

if (!file.exists(arquivo_vif)) {
  stop("Arquivo de variáveis mantidas pelo VIF não encontrado. Execute primeiro o Script 04 da Unidade 4.")
}

vif_vars <- read_csv(arquivo_vif, show_col_types = FALSE)

if (!"variavel" %in% names(vif_vars)) {
  stop("O arquivo variaveis_mantidas_vif.csv precisa conter uma coluna chamada 'variavel'.")
}

# ------------------------------------------------------------
# 4. Seleção final
# ------------------------------------------------------------
# A seleção final corresponde exatamente às variáveis aprovadas
# pelo VIF. Nenhuma variável é removida manualmente nesta etapa.
# ------------------------------------------------------------

variaveis_final <- vif_vars %>%
  dplyr::distinct(variavel, .keep_all = TRUE)

message("Número de variáveis aprovadas pelo VIF: ", nrow(variaveis_final))

# ------------------------------------------------------------
# 5. Classificação ecológica das variáveis
# ------------------------------------------------------------

variaveis_final <- variaveis_final %>%
  dplyr::mutate(
    grupo_ecologico = dplyr::case_when(
      grepl(
        "bio1|bio2|bio3|bio4|bio5|bio6|bio7|bio8|bio9|bio10|bio11|temp|temperatura",
        variavel,
        ignore.case = TRUE
      ) ~ "Temperatura",
      
      grepl(
        "bio12|bio13|bio14|bio15|bio16|bio17|bio18|bio19|prec|precipitacao|chuva",
        variavel,
        ignore.case = TRUE
      ) ~ "Precipitação",
      
      grepl(
        "elev|elevation|altitude|alt",
        variavel,
        ignore.case = TRUE
      ) ~ "Elevação",
      
      grepl(
        "decliv|slope",
        variavel,
        ignore.case = TRUE
      ) ~ "Declividade",
      
      grepl(
        "aspect|orient",
        variavel,
        ignore.case = TRUE
      ) ~ "Orientação",
      
      TRUE ~ "Outras"
    ),
    justificativa = dplyr::case_when(
      grupo_ecologico == "Temperatura" ~
        "Representa gradientes térmicos relacionados ao desempenho fisiológico e à distribuição geográfica da espécie.",
      
      grupo_ecologico == "Precipitação" ~
        "Representa disponibilidade hídrica, sazonalidade climática e restrições associadas ao balanço hídrico.",
      
      grupo_ecologico == "Elevação" ~
        "Representa gradientes topográficos associados a clima, drenagem e heterogeneidade ambiental.",
      
      grupo_ecologico == "Declividade" ~
        "Representa características geomorfológicas que influenciam drenagem e estabilidade do solo.",
      
      grupo_ecologico == "Orientação" ~
        "Representa diferenças microclimáticas relacionadas à exposição do relevo.",
      
      TRUE ~
        "Variável mantida após diagnóstico de multicolinearidade."
    )
  )

# ------------------------------------------------------------
# 6. Salvar variáveis finais
# ------------------------------------------------------------

write_csv(
  variaveis_final,
  "dados/unidade04/processados/variaveis_selecionadas_final.csv"
)

write_csv(
  variaveis_final,
  "resultados/unidade04/variaveis_selecionadas_final.csv"
)

write_csv(
  variaveis_final,
  "tabelas/unidade04/variaveis_selecionadas_final.csv"
)

# ------------------------------------------------------------
# 7. Resumo da seleção final
# ------------------------------------------------------------

resumo_final <- tibble(
  criterio = c(
    "Variáveis aprovadas pelo VIF",
    "Grupos ecológicos representados"
  ),
  valor = c(
    nrow(variaveis_final),
    dplyr::n_distinct(variaveis_final$grupo_ecologico)
  )
)

write_csv(
  resumo_final,
  "tabelas/unidade04/resumo_variaveis_finais.csv"
)

# ------------------------------------------------------------
# 8. Gráfico das variáveis selecionadas
# ------------------------------------------------------------

g_sel <- ggplot(
  variaveis_final,
  aes(
    x = reorder(variavel, grupo_ecologico),
    y = 1,
    fill = grupo_ecologico
  )
) +
  geom_col(width = 0.75) +
  coord_flip() +
  labs(
    title = "Variáveis ambientais selecionadas para modelagem",
    subtitle = "Seleção final correspondente às variáveis aprovadas pelo VIF",
    x = NULL,
    y = NULL,
    fill = "Grupo ecológico"
  ) +
  theme_bw(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 9),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )

ggsave(
  "figuras/unidade04/unidade04_variaveis_selecionadas.png",
  plot = g_sel,
  width = 8,
  height = 5,
  dpi = 600
)

# ------------------------------------------------------------
# 9. Mensagens finais
# ------------------------------------------------------------

message("Seleção final de variáveis concluída.")
message("Resumo da seleção final:")

print(resumo_final)

message("Variáveis finais selecionadas:")

print(variaveis_final)
