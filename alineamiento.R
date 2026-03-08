library(readr)
library(dplyr)
library(ggplot2)

# 2. Leer el archivo CSV con datos de alineamiento
alineamiento <- read_delim("C:/Users/RENZO MURILLO/OneDrive/Documentos/secuencias/Alineamiento_de_todo.csv",
                           delim = ";", 
                           locale = locale(encoding = "UTF-8"))

# 3. Asegurarse que el porcentaje est? en formato num?rico
alineamiento$Alineamiento <- as.numeric(gsub(",", ".", alineamiento$Alineamiento))

# 4. Eliminar filas incompletas
alineamiento_limpio <- alineamiento %>%
  filter(!is.na(Alineamiento), Ubicación != "")
alineamiento_limpio$Ubicación[alineamiento_limpio$Ubicación == "Kenya"] <- "Kenia"
# 5. Calcular estad?sticas por pa?s
resumen_alineamiento <- alineamiento_limpio %>%
  group_by(Ubicación) %>%
  summarise(
    Alineamiento_promedio = mean(Alineamiento),
    Desv_estandar = sd(Alineamiento),
    n = n(),
    SE = Desv_estandar / sqrt(n),
    IC_inf = Alineamiento_promedio - 1.96 * SE,
    IC_sup = Alineamiento_promedio + 1.96 * SE
  )
# 6. Mostrar en consola
resumen_alineamiento_completo <- alineamiento_limpio %>%
  group_by(Ubicación) %>%
  summarise(
    n = n(),
    Media = mean(Alineamiento),
    Mediana = median(Alineamiento),
    SD = sd(Alineamiento),
    CV_porcentaje = (SD / Media) * 100,
    Min = min(Alineamiento),
    Q1 = quantile(Alineamiento, 0.25),
    Q3 = quantile(Alineamiento, 0.75),
    Max = max(Alineamiento),
    SE = SD / sqrt(n),
    IC_inf = Media - 1.96 * SE,
    IC_sup = Media + 1.96 * SE
  ) %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))

print(resumen_alineamiento_completo, width = Inf)

# 7. Graficar con borde solo para Argentina
ggplot(resumen_alineamiento, 
       aes(x = reorder(Ubicación, -Alineamiento_promedio), 
           y = Alineamiento_promedio, 
           fill = Ubicación,
           color = Ubicación)) +
  geom_bar(stat = "identity", width = 0.7, show.legend = FALSE, size = 0.8) +
  geom_errorbar(aes(ymin = IC_inf, ymax = IC_sup), 
                width = 0.2, color = "black") +
  labs(title = "Promedio del porcentaje de alineamiento por país con IC 95%",
       x = "País",
       y = "Alineamiento promedio (%)") +
  scale_fill_manual(values = c("Argentina" = "#E0FFFF",
                               "Kenia" = "#00BFFF",
                               "EE.UU" = "#36648B")) +
  scale_color_manual(values = c("Argentina" = "#607B8B",
                                "Kenia" = NA,
                                "EE.UU" = NA),
                     guide = "none") +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 11)
  ) +
  ylim(0, max(resumen_alineamiento$IC_sup) * 1.05)


