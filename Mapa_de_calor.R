# Cargar librer?as
library(readr)
library(pheatmap)

matriz <- read_delim(
  "C:/Users/RENZO MURILLO/OneDrive/Documentos/Doc_root/archivos Call/Genes_detox_filtro.csv",
  delim = ";",
  locale = locale(encoding = "Latin1")
)

stopifnot("Gene" %in% names(matriz))
rownames(matriz) <- matriz$Gene

# Matriz num?rica (columnas = muestras)
matriz_heat <- as.data.frame(lapply(matriz[, setdiff(names(matriz), "Gene")], as.numeric))
rownames(matriz_heat) <- rownames(matriz)

# =========================
# 2) Metadata de columnas (Ubicaci?n)
# =========================
metadata <- read_delim(
  "C:/Users/RENZO MURILLO/OneDrive/Documentos/Doc_root/Ubicacion.csv",
  delim = ";",
  locale = locale(encoding = "Latin1")
)

# Normaliza nombre de la columna y pa?ses
if (!("Ubicación" %in% names(metadata)) && "Ubicación" %in% names(metadata)) {
  names(metadata)[names(metadata)=="Ubicación"] <- "Ubicación"
}
stopifnot(all(c("Código","Ubicación") %in% names(metadata)))
metadata$Ubicación <- gsub("Kenya", "Kenia", metadata$Ubicación)

annotation_col <- data.frame(
  Ubicación = metadata$Ubicación,
  row.names = metadata$Código,
  check.names = FALSE
)

# Alinear a columnas reales de la matriz
annotation_col <- annotation_col[colnames(matriz_heat), , drop = FALSE]

# =========================
# 3) Metadata de filas (Fase por gen)
# =========================
genes_fases <- read_delim(
  "C:/Users/RENZO MURILLO/OneDrive/Documentos/Doc_root/Genes_fases.csv",
  delim = ";",
  locale = locale(encoding = "Latin1")
)
stopifnot(ncol(genes_fases) >= 2)
colnames(genes_fases)[1:2] <- c("Gene","Fase")

fase_por_gen <- genes_fases$Fase[match(rownames(matriz_heat), genes_fases$Gene)]
annotation_row <- data.frame(Fase = fase_por_gen,
                             row.names = rownames(matriz_heat),
                             check.names = FALSE)

# =========================
# 4) Colores de anotaciones
# =========================
colores_ubicación <- c(
  "Argentina" = "#E0FFFF",  # dorado
  "Kenia"     = "#00BFFF",  # naranja
  "EE.UU"     = "#36648B"   # verde suave
)

colores_fase <- c(
  "Fase I"   = "#FFF68F",  # amarillo
  "Fase II"  = "#FFA54F",  # ?mbar
  "Fase III" = "#CD8500"   # gris c?lido
)

ann_colors <- list(
  Ubicación = colores_ubicación,
  Fase      = colores_fase
)

# =========================
# 5) Saneo de la matriz (evitar NA/Inf y varianza 0)
# =========================
mh <- as.matrix(matriz_heat)
storage.mode(mh) <- "numeric"

# Reemplazar no finitos por NA y peque?a imputaci?n por mediana de columna
mh[!is.finite(mh)] <- NA
for (j in seq_len(ncol(mh))) {
  if (anyNA(mh[, j])) {
    med <- suppressWarnings(median(mh[, j], na.rm = TRUE))
    if (is.finite(med)) mh[is.na(mh[, j]), j] <- med
  }
}

# Remover filas/columnas con varianza 0 (constantes)
#zero_var_row <- apply(mh, 1, function(x) sd(x, na.rm = TRUE) == 0)
#zero_var_col <- apply(mh, 2, function(x) sd(x, na.rm = TRUE) == 0)
#mh <- mh[!zero_var_row, !zero_var_col, drop = FALSE]

# Re-alinear anotaciones a lo que qued?
annotation_row <- annotation_row[rownames(mh), , drop = FALSE]
annotation_col <- annotation_col[colnames(mh), , drop = FALSE]

# =========================
# 6) Chequeos ?tiles (opcional)
# =========================
# print(table(annotation_col$Ubicaci?n, useNA = "ifany"))
# print(unique(annotation_row$Fase))
# stopifnot(setequal(rownames(annotation_col), colnames(mh)))
# stopifnot(setequal(rownames(annotation_row), rownames(mh)))

# =========================
library(grid)

pheatmap(
  mh,
  annotation_col    = annotation_col,
  annotation_row    = annotation_row,
  annotation_colors = ann_colors,
  cluster_rows = TRUE,
  cluster_cols = TRUE,
  show_rownames = TRUE,
  show_colnames = TRUE,
  fontsize_row = 4,
  fontsize_col = 6,
  color = colorRampPalette(c("#4876FF", "white", "#FF0000"))(100),
  main = "",   # dejamos vacío para agregar el centrado manualmente
  legend = TRUE,
  annotation_legend = TRUE
)

# 🔹 Título centrado manual
grid.text(
  "Mapa de Calor de CNV con Ubicación Geográfica y Fases",
  x = 0.5, y = 0.97, gp = gpar(fontsize = 14, fontface = "bold")
)
