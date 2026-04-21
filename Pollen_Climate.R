rm(list = ls()) # Clear all
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load libraries
library(openxlsx)
library(dplyr)
library(ggplot2)
library(gridExtra)

pollen_counts <- read.xlsx("EMPD2_count_v1.xlsx", sheet= "counts")

head(pollen_counts)



# =========================
# 0. LIBRERÍAS
# =========================
library(dplyr)
library(tidyr)
library(stringr)
library(tibble)
library(vegan)

# =========================
# 1. COPIA DE SEGURIDAD
# =========================
df <- pollen_counts

# =========================
# 2. LIMPIEZA DE NOMBRES TAXONÓMICOS
# =========================
df <- df %>%
  mutate(
    taxon = acc_varname,
    
    # eliminar abreviaturas tipo "t.", "cf.", etc.
    taxon = str_replace_all(taxon, "\\b(t\\.|cf\\.|aff\\.)\\b", ""),
    
    # eliminar contenido entre paréntesis
    taxon = str_replace_all(taxon, "\\s*\\(.*?\\)", ""),
    
    # quitar espacios extra
    taxon = str_trim(taxon),
    
    # quedarte solo con la primera palabra (género)
    taxon = word(taxon, 1)
  )

# =========================
# 3. AGRUPAR Y SUMAR CONTEOS
# =========================
df_sum <- df %>%
  group_by(SampleName, taxon) %>%
  summarise(count = sum(count), .groups = "drop")

# =========================
# 4. CONVERTIR A FORMATO ANCHO
# =========================
pollen_wide <- df_sum %>%
  pivot_wider(
    names_from = taxon,
    values_from = count,
    values_fill = 0
  )

# =========================
# 5. PASAR A MATRIZ (filas = muestras)
# =========================
pollen_mat <- pollen_wide %>%
  column_to_rownames("SampleName")

# =========================
# 6. ELIMINAR TAXONES MUY RAROS
# (ej. presentes en <2 muestras o <1% total)
# =========================

# frecuencia de ocurrencia
taxa_occ <- colSums(pollen_mat > 0)

# abundancia total
taxa_sum <- colSums(pollen_mat)

# filtrar (ajusta umbrales si quieres)
keep_taxa <- names(taxa_occ[taxa_occ >= 2 & taxa_sum > 0])

pollen_mat <- pollen_mat[, keep_taxa]

# =========================
# 7. CONVERTIR A PORCENTAJES
# =========================
pollen_pct <- decostand(pollen_mat, method = "total") * 100

# =========================
# 8. RESULTADO FINAL
# =========================

# matriz lista para análisis
head(pollen_pct)

# opcional: guardar
 write.csv(pollen_pct, "pollen_percentages.csv")

 
 
#temperatures and precipitationa
 
 
pollen_mat_map <- read.xlsx("EMPD2_count_v1.xlsx", sheet= "climate_npp") 
head(pollen_mat_map)


library(dplyr)
library(tibble)

climate <- pollen_mat_map %>%
  mutate(SampleName = Sample.ID) %>%
  select(SampleName,
         MAT, MAT_winter, MAT_summer,
         MAP, MAP_winter, MAP_summer)

pollen_df <- pollen_pct %>%
  as.data.frame() %>%
  rownames_to_column("SampleName")


dataset_full <- left_join(pollen_df, climate, by = "SampleName")

head(dataset_full)
nrow(dataset_full)
write.csv(dataset_full, "pollen_climate_dataset.csv", row.names = FALSE)






# READ DATA WITH POLLEN PERCENTAGES AND CLIMATE VARIABLES

data_df <- read.csv("pollen_climate_dataset.csv")
head(data_df)
nrow(data_df)

library(dplyr)
library(vegan)
library(analogue)

ncol(data_df)

PercentageSpecies<- data_df[,2:812]
head(PercentageSpecies)

MAT.Dataset<- data_df$MAT
head(MAT.Dataset)

NPP.Dataset<- data_df$NPP
head(NPP.Dataset)

MAP.Dataset<- data_df$MAP

pred.temp <-wa(PercentageSpecies, NPP.Dataset, deshrink= "monotonic")
pred.temp 


pred.prep<-wa(PercentageSpecies, MAP.Dataset, deshrink= "monotonic")
pred.prep






# NOW WITH WA PLS

# 2. Preparar datos de especies (quitar columna de nombres)
comm <- PercentageSpecies

# 3. Eliminar especies sin abundancia (todo ceros)
comm <- comm[, colSums(comm) > 0]

# 4. Convertir a matriz numérica
comm <- as.matrix(comm)

# 5. Variable ambiental (temperatura)
temp <- NPP.Dataset

# 6. Ajustar modelo WA-PLS
pred.temp <- WAPLS(comm, temp, npls = 5)

# 6. Ajustar modelo WA-PLS
pred.prep <- WAPLS(comm, MAP.Dataset, npls = 5)








Clim_df <- read.xlsx("REVEALS_Verdeospesoa.xlsx", rowNames=FALSE,
                     colNames=TRUE, sheet="Climate_c")
head(Clim_df)

Sites <-Clim_df[,1]
Level <-Clim_df[,2]
Sites

ncol(Clim_df)
Dataset <-Clim_df[,3:51]
head(Dataset)
colnames(Dataset)

### MAT

pred.MAT <- predict(pred.temp, Dataset, tol.dw=TRUE)
pred.MAT

reconPlot(pred.MAT, use.labels = TRUE, display = "bars")

head(MAT)
MAT <- as.data.frame(pred.MAT$pred$pred)
MAT <- as.data.frame(pred.MAT$fit)
MAT$Site <- Sites
MAT$Level <- Level
write.xlsx(MAT, "PredictedMAT2.xlsx")


### MAP

pred.MAP <- predict(pred.prep, Dataset, tol.dw=TRUE)
pred.MAP
reconPlot(pred.MAP, use.labels = TRUE, display = "bars")
MAP <- as.data.frame(pred.MAP$pred$pred)
MAP <- as.data.frame(pred.MAP$fit)
MAP
MAP$Site <- Sites
MAP$Level <- Level
write.xlsx(MAP, "PredictedMAP2.xlsx")


pred.MAT$pred$pred
head(MAP)

MAT_plot<- ggplot(MAT, aes(x = Level, y = MAT$`Comp05`)) +
  geom_point() +          # Para hacer el gráfico de puntos
  geom_line()+
  labs(x = "Level", y = "MAT") +
  scale_x_continuous(limits = c(6000, 28000)) + # Puntos en rojo
  theme_minimal()         # Usar un tema limpio


MAP_plot<- ggplot(MAP, aes(x = Level, y = MAP$`Comp05`)) +
  geom_point() +          # Para hacer el gráfico de puntos
  geom_line()+
  labs(x = "Level", y = "MAP") +
  scale_x_continuous(limits = c(6000, 28000)) +
  theme_minimal()         # Usar un tema limpio

gridExtra::grid.arrange(MAT_plot, MAP_plot)







