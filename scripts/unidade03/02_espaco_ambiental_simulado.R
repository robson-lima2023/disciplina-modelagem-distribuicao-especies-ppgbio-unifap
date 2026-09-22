# ============================================================
# Unidade 3 - Variáveis Ambientais em SDM
# Script 02: Espaço ambiental simulado
# ============================================================

pacotes <- c("ggplot2", "dplyr", "readr")

instalar <- pacotes[!pacotes %in% rownames(installed.packages())]
if (length(instalar) > 0) install.packages(instalar)

invisible(lapply(pacotes, library, character.only = TRUE))

dir.create("figuras/unidade03", recursive = TRUE, showWarnings = FALSE)
dir.create("tabelas/unidade03", recursive = TRUE, showWarnings = FALSE)

set.seed(123)

background <- data.frame(
  temperatura = runif(5000, 5, 35),
  precipitacao = runif(5000, 300, 3500),
  tipo = "Background"
)

presencas <- data.frame(
  temperatura = rnorm(300, 24, 3),
  precipitacao = rnorm(300, 1800, 400),
  tipo = "Presença"
) %>%
  filter(
    temperatura >= 5,
    temperatura <= 35,
    precipitacao >= 300,
    precipitacao <= 3500
  )

dados <- bind_rows(background, presencas)

grafico <- ggplot() +
  geom_point(
    data = background,
    aes(x = temperatura, y = precipitacao),
    color = "grey70",
    alpha = 0.35,
    size = 1
  ) +
  geom_point(
    data = presencas,
    aes(x = temperatura, y = precipitacao),
    color = "darkgreen",
    alpha = 0.85,
    size = 1.5
  ) +
  labs(
    title = "Espaço ambiental disponível e ocupado",
    subtitle = "Simulação com temperatura e precipitação",
    x = "Temperatura média anual (°C)",
    y = "Precipitação anual (mm)"
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid = element_line(color = "grey90")
  )

print(grafico)

ggsave(
  "figuras/unidade03/unidade03_espaco_ambiental_simulado.png",
  plot = grafico,
  width = 8,
  height = 6,
  dpi = 600
)

write_csv(
  dados,
  "tabelas/unidade03/dados_espaco_ambiental_simulado.csv"
)

message("Script 02 concluído.")
