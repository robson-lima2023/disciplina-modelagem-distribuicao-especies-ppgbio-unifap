# ============================================================
# Unidade 5 - Área acessível (M), BAM e Background
# Script 05: Comparação entre diferentes áreas M
# ============================================================

pacotes <- c("sf", "dplyr", "ggplot2", "readr")

instalar <- pacotes[!pacotes %in% rownames(installed.packages())]
if (length(instalar) > 0) install.packages(instalar)

invisible(lapply(pacotes, library, character.only = TRUE))

dir.create("figuras/unidade05", recursive = TRUE, showWarnings = FALSE)
dir.create("tabelas/unidade05", recursive = TRUE, showWarnings = FALSE)

m_250 <- st_read("dados/unidade05/processados/area_m_250km.gpkg", quiet = TRUE)
m_500 <- st_read("dados/unidade05/processados/area_m_500km.gpkg", quiet = TRUE)
m_1000 <- st_read("dados/unidade05/processados/area_m_1000km.gpkg", quiet = TRUE)

# ------------------------------------------------------------
# Calcular área em km²
# ------------------------------------------------------------

calcular_area <- function(x, nome) {
  x_proj <- st_transform(x, 6933)
  area_km2 <- as.numeric(st_area(x_proj)) / 1e6
  
  data.frame(
    area_m = nome,
    area_km2 = area_km2
  )
}

resumo_area <- bind_rows(
  calcular_area(m_250, "M_250km"),
  calcular_area(m_500, "M_500km"),
  calcular_area(m_1000, "M_1000km")
)

write_csv(
  resumo_area,
  "tabelas/unidade05/resumo_areas_m.csv"
)

grafico <- ggplot(
  resumo_area,
  aes(x = area_m, y = area_km2)
) +
  geom_col() +
  labs(
    title = "Comparação entre áreas acessíveis M",
    subtitle = "Área total delimitada por diferentes buffers",
    x = "Área acessível",
    y = "Área (km²)"
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold")
  )

print(grafico)

ggsave(
  "figuras/unidade05/unidade05_comparacao_area_m.png",
  plot = grafico,
  width = 7,
  height = 5,
  dpi = 600
)

message("Comparação entre áreas M concluída.")
print(resumo_area)
