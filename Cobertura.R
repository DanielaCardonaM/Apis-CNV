library(readr)
library(dplyr)
library(ggplot2)

# 1. Leer CSV
genomas <- read_delim(
  "C:/Users/RENZO MURILLO/OneDrive/Documentos/secuencias/cobertura.csv",
  delim = ";",
  locale = locale(decimal_mark = ",", grouping_mark = ".", encoding = "UTF-8")
)

# 2. Eliminar columna vacía y valores NA
genomas <- genomas %>%
  select(-`...4`) %>%           # quita la columna extra
  filter(!is.na(Cobertura),     # sin NA en Cobertura
         Ubicación != "")       # sin ubicaciones vacías

# 3. Normalizar nombre de país
genomas$Ubicación[genomas$Ubicación == "Kenya"] <- "Kenia"

# 4. Resumen estadístico por país
resumen <- genomas %>%
  group_by(Ubicación) %>%
  summarise(
    Cobertura_promedio = mean(Cobertura),
    Desv_estandar      = sd(Cobertura),
    n                  = n(),
    SE                 = Desv_estandar / sqrt(n),
    IC_inf             = Cobertura_promedio - 1.96 * SE,
    IC_sup             = Cobertura_promedio + 1.96 * SE,
    .groups            = "drop"
  )

# 5. Colores iguales al mapa de calor
colores_ubicacion <- c(
  "Argentina" = "#E0FFFF",
  "Kenia"     = "#00BFFF",
  "EE.UU"     = "#36648B"
)

# 6. Gráfico de cobertura con borde SOLO para Argentina
resumen_plot <- resumen %>%
  mutate(borde = ifelse(Ubicación == "Argentina", "#607B8B", "transparent"))

ggplot(resumen_plot,
       aes(x = reorder(Ubicación, -Cobertura_promedio),
           y = Cobertura_promedio,
           fill = Ubicación)) +
  geom_col(aes(color = borde), width = 0.7, size = 0.9, show.legend = FALSE) +
  geom_errorbar(aes(ymin = IC_inf, ymax = IC_sup),
                width = 0.2, color = "black") +
  labs(title = "Cobertura genómica promedio por país con IC 95%",
       x = "País",
       y = "Cobertura promedio (X)") +
  scale_fill_manual(values = colores_ubicacion) +
  scale_color_identity() +  # usa los valores tal cual ("black" o "transparent")
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 11)
  ) +
  ylim(0, max(resumen_plot$IC_sup) * 1.05)

resumen_completo <- genomas %>%
  group_by(Ubicación) %>%
  summarise(
    n = n(),
    Media = mean(Cobertura),
    Mediana = median(Cobertura),
    SD = sd(Cobertura),
    CV_porcentaje = (SD / Media) * 100,
    Min = min(Cobertura),
    Q1 = quantile(Cobertura, 0.25),
    Q3 = quantile(Cobertura, 0.75),
    Max = max(Cobertura),
    SE = SD / sqrt(n),
    IC_inf = Media - 1.96 * SE,
    IC_sup = Media + 1.96 * SE,
    .groups = "drop"
  )
resumen_completo %>%
  mutate(across(where(is.numeric), ~ round(.x, 2))) %>%
  print(width = Inf)
