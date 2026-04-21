rm(list = ls()) # Clear all
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


## CHRONO ####

# Cargar librerías necesarias
library(rbacon)
library(IntCal)

# 1. Eliminar carpeta anterior si existe y crear nueva
unlink("Bacon_runs/Verdeospesoa", recursive = TRUE)
dir.create("Bacon_runs", showWarnings = FALSE)
dir.create("Bacon_runs/Verdeospesoa", showWarnings = FALSE)

# 2. Crear datos ORDENADOS por profundidad (de menor a mayor)
# Nota: 121 cm es más superficial, 225 cm es más profundo
dataciones <- data.frame(
  labID = c( "D5", "D4", "D3", "D2", "D1"),
  age = c( 10530, 11310, 13082, 20829, 24918), # 2940
  error = c(51, 140, 109, 78, 238), #41
  depth = c( 182, 196, 205, 218, 225), #121
  cc = rep(0, 5)  # 1 = IntCal20 (terrestre hemisferio norte)
)

# 3. Verificar que estén ordenadas correctamente
print("Datos ordenados por profundidad (ascendente):")
print(dataciones)

# 4. Verificar consistencia temporal (profundidad mayor = edad mayor)
cat("\nVerificando consistencia temporal:\n")
for(i in 2:nrow(dataciones)) {
  if(dataciones$age[i] < dataciones$age[i-1]) {
    cat("¡ADVERTENCIA! Posible reversión en profundidad", 
        dataciones$depth[i], "cm\n")
  }
}

# 5. Guardar archivo .csv ordenado
write.csv(dataciones, "Bacon_runs/Verdeospesoa/Verdeospesoa.csv", 
          row.names = FALSE)

# 6. Crear archivo con todas las profundidades (opcional pero útil)
write.table(data.frame(depth = seq(121, 225, by = 1)), 
            "Bacon_runs/Verdeospesoa/Verdeospesoa_depths.txt", 
            row.names = FALSE, col.names = FALSE)

# 7. Primera ejecución: dejar que Bacon sugiera parámetros
cat("\n=== PRIMERA EJECUCIÓN: OBTENER SUGERENCIAS DE PARÁMETROS ===\n")
Bacon(
  core = "Verdeospesoa",
  coredir = "Bacon_runs",
  #acc.mean = 600,
  suggest = TRUE,  # Que sugiera parámetros óptimos
  ask = TRUE  ,     # Revisar sugerencias antes de ejecutar
cc=0)




#### n-alanes ####

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(openxlsx)
library(dplyr)
library(ggplot2)
library(gridExtra)

n_akanes_df <- read.xlsx("D:/2026/nalkanes-Verdeospesoa/ANALYSES_.xlsx", sheet= "R")


head(n_akanes_df)



n_akanes_df$Period <- ifelse(n_akanes_df$CalAge >= 11700, "Late Pleistocene", "Early Holocene")





Summary_df <- n_akanes_df %>%
  group_by(Period) %>%
  summarise(
    CPI_mean = mean(CPI, na.rm = TRUE),
    CPI_sd   = sd(CPI, na.rm = TRUE),
    CPI_min  = min(CPI, na.rm = TRUE),
    CPI_max  = max(CPI, na.rm = TRUE),
    
    ACL_mean = mean(ACL, na.rm = TRUE),
    ACL_sd   = sd(ACL, na.rm = TRUE),
    ACL_min  = min(ACL, na.rm = TRUE),
    ACL_max  = max(ACL, na.rm = TRUE),
    
    PAQ_mean = mean(PAQ, na.rm = TRUE),
    PAQ_sd   = sd(PAQ, na.rm = TRUE),
    PAQ_min  = min(PAQ, na.rm = TRUE),
    PAQ_max  = max(PAQ, na.rm = TRUE),
    
    TAR_mean = mean(TAR, na.rm = TRUE),
    TAR_sd   = sd(TAR, na.rm = TRUE),
    TAR_min  = min(TAR, na.rm = TRUE),
    TAR_max  = max(TAR, na.rm = TRUE),
    
    ratio_mean = mean(`c31/c29`, na.rm = TRUE),
    ratio_sd   = sd(`c31/c29`, na.rm = TRUE),
    ratio_min  = min(`c31/c29`, na.rm = TRUE),
    ratio_max  = max(`c31/c29`, na.rm = TRUE),
    
    c31c27_mean = mean(`c31/c27`, na.rm = TRUE),
    c31c27_sd   = sd(`c31/c27`, na.rm = TRUE),
    c31c27_min  = min(`c31/c27`, na.rm = TRUE),
    c31c27_max  = max(`c31/c27`, na.rm = TRUE)
  )


View(Summary_df)

wilcox.test(CPI ~ Period, data = n_akanes_df)
wilcox.test(ACL ~ Period, data = n_akanes_df)
wilcox.test(PAQ ~ Period, data = n_akanes_df)
wilcox.test(TAR ~ Period, data = n_akanes_df)
wilcox.test(`c31/c29` ~ Period, data = n_akanes_df)
wilcox.test(`c31/c27` ~ Period, data = n_akanes_df)









library(ggplot2)



# Transformar datos a formato largo
library(tidyr)
library(ggplot2)

# Asegurar el orden de los factores: Pleistocene primero, Holocene después
n_akanes_df$Period <- factor(n_akanes_df$Period, 
                             levels = c("Late Pleistocene", "Early Holocene"))

# Definir paleta de colores profesional
colores_periodos <- c(
  "Late Pleistocene" = "#2E86AB",
  "Early Holocene" = "#F18F01"
)

# Transformar a formato largo
n_akanes_long <- n_akanes_df %>%
  pivot_longer(cols = c(CPI, ACL, PAQ, TAR, `c31/c29`, `c31/c27`),
               names_to = "Indice",
               values_to = "Valor")

# Renombrar para mejor presentación
n_akanes_long$Indice <- factor(n_akanes_long$Indice,
                               levels = c("CPI", "ACL", "PAQ", "TAR", "c31/c29", "c31/c27"),
                               labels = c("CPI", "ACL", "PAQ", "TAR", "C[31]/C[29]", "C[31]/C[27]"))

# Plot con facetas - CORREGIDO
p_facet <- ggplot(n_akanes_long, aes(x = Period, y = Valor, fill = Period)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA, width = 0.6) +
  geom_jitter(width = 0.15, alpha = 0.4, size = 1) +
  scale_fill_manual(values = colores_periodos) +
  facet_wrap(~Indice, scales = "free_y", labeller = label_parsed) +
  labs(
    title = " ",
    x = "",
    y = " "
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    strip.text = element_text(face = "bold", size = 12),
    strip.background = element_rect(fill = "gray95", color = NA),
    # ETIQUETAS HORIZONTALES (sin ángulo)
    axis.text.x = element_text(face = "bold", size = 11, angle = 0, hjust = 0.5),
    axis.title.y = element_text(face = "bold"),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.border = element_rect(fill = NA, color = "gray80", linewidth = 0.5),
    legend.title = element_blank()
  ) +
  guides(fill = guide_legend(title = NULL))

print(p_facet)

#




NGRIP_df <- read.xlsx("D:/2026/nalkanes-Verdeospesoa/ANALYSES_.xlsx", sheet = "NGRIP", detectDates = FALSE)
head(NGRIP_df)

# Create df with events (for visual purposes)
events <- data.frame(
  Event = c("H3", "H2", "LGM", "H1",  "Younger Dryas"),
  start = c(31000, 24500, 21500, 17000,  12900),
  end   = c(29000, 23000, 19000, 15000, 11700)
)



n_akanes_df 


# -----------------------------
# Events
# -----------------------------
events <- data.frame(
  Event = c("H2", "LGM", "H1", "YD"),
  start_age = c(24500, 21500, 17000, 12900),
  end_age   = c(23000, 19000, 15000, 11700)
)

get_depth_for_age <- function(age) {
  approx(n_akanes_df$CalAge,
         n_akanes_df$Depth,
         xout = age)$y
}

event_rects <- events %>%
  rowwise() %>%
  mutate(
    depth_start = get_depth_for_age(start_age),
    depth_end   = get_depth_for_age(end_age)
  )


# Crear breaks para la edad (cada 5 ka)
age_breaks <- seq(
  from = floor(min(n_akanes_df$CalAge, na.rm = TRUE)/5000)*5000,
  to = ceiling(max(n_akanes_df$CalAge, na.rm = TRUE)/5000)*5000,
  by = 5000
)

# Encontrar las profundidades correspondientes a las edades más cercanas
find_closest_depth <- function(age_break) {
  closest_idx <- which.min(abs(n_akanes_df$CalAge - age_break))
  n_akanes_df$Depth[closest_idx]
}

depth_labels <- sapply(age_breaks, find_closest_depth)

# Crear plot con Depth en eje X
C31_C29<- ggplot(n_akanes_df, aes(x = Depth)) +
  geom_rect(data = event_rects,
            aes(xmin = depth_end, xmax = depth_start,
                ymin = -Inf, ymax = Inf),
            inherit.aes = FALSE,
            fill = "grey80", alpha = 0.3) +
  geom_point(aes(y = `c31/c29`), color = "#FF7F00", size = 2, shape = 16, alpha = 0.8) +
  geom_line(data = na.omit(n_akanes_df[, c("Depth", "c31/c29")]), 
            aes(y = `c31/c29`), color = "#FF7F00", linewidth = 1, alpha = 0.7) +
  scale_x_reverse(
    sec.axis = sec_axis(
      ~ .,
      breaks = depth_labels,
      labels = age_breaks/1000
    )
  ) +
  labs(y = "C31/C29 ratio", , x = "") +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major = element_line(color = "gray90", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(fill = NA, color = "gray70", linewidth = 0.8),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 13)
  )

C31_C29


C31_C27<- ggplot(n_akanes_df, aes(x = Depth)) +
  geom_rect(data = event_rects,
            aes(xmin = depth_end, xmax = depth_start,
                ymin = -Inf, ymax = Inf),
            inherit.aes = FALSE,
            fill = "grey80", alpha = 0.3) +
  geom_point(aes(y = `c31/c27`), color = "darkorange4", size = 2, shape = 16, alpha = 0.8) +
  geom_line(data = na.omit(n_akanes_df[, c("Depth", "c31/c27")]), 
            aes(y = `c31/c27`), color = "darkorange4", linewidth = 1, alpha = 0.7) +
  scale_x_reverse(
    sec.axis = sec_axis(
      ~ .,
      breaks = depth_labels,
      labels = age_breaks/1000
    )
  ) +
  labs(y = "C31/C27 ratio",  x = "") +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major = element_line(color = "gray90", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(fill = NA, color = "gray70", linewidth = 0.8),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 13)
  )

C31_C27

CPI<- ggplot(n_akanes_df, aes(x = Depth)) +
  geom_rect(data = event_rects,
            aes(xmin = depth_end, xmax = depth_start,
                ymin = -Inf, ymax = Inf),
            inherit.aes = FALSE,
            fill = "grey80", alpha = 0.3) +
  geom_point(aes(y = CPI), color = "black", size = 2, shape = 16, alpha = 0.8) +
  geom_line(data = na.omit(n_akanes_df[, c("Depth", "CPI")]), 
            aes(y = CPI), color = "black", linewidth = 1, alpha = 0.7) +
  scale_x_reverse(
    sec.axis = sec_axis(
      ~ .,
      breaks = depth_labels,
      labels = age_breaks/1000
    )
  ) +
  labs(y = "CPI", x = "") +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major = element_line(color = "gray90", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(fill = NA, color = "gray70", linewidth = 0.8),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 13)
  )
CPI


ACL<- ggplot(n_akanes_df, aes(x = Depth)) +
  geom_rect(data = event_rects,
            aes(xmin = depth_end, xmax = depth_start,
                ymin = -Inf, ymax = Inf),
            inherit.aes = FALSE,
            fill = "grey80", alpha = 0.3) +
  geom_point(aes(y = ACL), color = "blue", size = 2, shape = 16, alpha = 0.8) +
  geom_line(data = na.omit(n_akanes_df[, c("Depth", "ACL")]), 
            aes(y = ACL), color = "blue", linewidth = 1, alpha = 0.7) +
  scale_x_reverse(
    sec.axis = sec_axis(
      ~ .,
      
      breaks = depth_labels,
      labels = age_breaks/1000
    )
  ) +
  labs(y = "ACL", x = "") +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major = element_line(color = "gray90", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(fill = NA, color = "gray70", linewidth = 0.8),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 13)
  )


PAQ<- ggplot(n_akanes_df, aes(x = Depth)) +
  geom_rect(data = event_rects,
            aes(xmin = depth_end, xmax = depth_start,
                ymin = -Inf, ymax = Inf),
            inherit.aes = FALSE,
            fill = "grey80", alpha = 0.3) +
  geom_point(aes(y = PAQ), color = "red", size = 2, shape = 16, alpha = 0.8) +
  geom_line(data = na.omit(n_akanes_df[, c("Depth", "PAQ")]), 
            aes(y = PAQ), color = "red", linewidth = 1, alpha = 0.7) +
  scale_x_reverse(
    sec.axis = sec_axis(
      ~ .,
      breaks = depth_labels,
      labels = age_breaks/1000
    )
  ) +
  labs(y = "PAQ", x = "") +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major = element_line(color = "gray90", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(fill = NA, color = "gray70", linewidth = 0.8),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 13)
  )


# There is an outlier in TAR (sample at 215 cm depth = 86.9) (see previous boxplot), so this removed from this plot:
out_df<- subset(n_akanes_df, TAR < 86)

TAR<- ggplot(out_df, aes(x = Depth)) +
  geom_rect(data = event_rects,
            aes(xmin = depth_end, xmax = depth_start,
                ymin = -Inf, ymax = Inf),
            inherit.aes = FALSE,
            fill = "grey80", alpha = 0.3) +
  geom_point(aes(y = TAR), color = "darkred", size = 2, shape = 16, alpha = 0.8) +
  geom_line(data = na.omit(out_df[, c("Depth", "TAR")]), 
            aes(y = TAR), color = "darkred", linewidth = 1, alpha = 0.7) +
  scale_x_reverse(
    sec.axis = sec_axis(
      ~ .,
      breaks = depth_labels,
      labels = age_breaks/1000
    )
  ) +
  labs(y = "TAR", x = "") +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major = element_line(color = "gray90", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(fill = NA, color = "gray70", linewidth = 0.8),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 13)
  )






head(event_rects)


NGRIP_plot <- ggplot(NGRIP_df, aes(x = Age * 1000)) +
  geom_rect(data = event_rects,
            aes(xmin = start_age, xmax = end_age,  # Use age columns, not depth
                ymin = -Inf, ymax = Inf),
            inherit.aes = FALSE,
            fill = "grey80", alpha = 0.3) +
  geom_line(data = na.omit(NGRIP_df[, c("Age", "d18O")]), 
            aes(y = d18O), color = "darkblue", linewidth = 1, alpha = 0.7) +
  scale_x_reverse( limits =c(25000, 10000),
    sec.axis = sec_axis(
      ~ .,
      breaks = depth_labels,
      labels = age_breaks/1000
    )
  ) +
  labs(y = "d18O", x = "") +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major = element_line(color = "gray90", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(fill = NA, color = "gray70", linewidth = 0.8),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 13)
  )



grid.arrange(NGRIP_plot, MAT, MAP, CPI, C31_C29, ACL, PAQ, TAR, C31_C27, ncol= 1)




colnames(out_df)



#ACL, PAQ,



# Stable isotopes ####

colnames(n_akanes_df)

Summary_df <- n_akanes_df %>%
  group_by(Period) %>%
  summarise(
    dC23_ = mean(dC23, na.rm = TRUE),
    dC23_sd   = sd(dC23, na.rm = TRUE),
    dC25_ = mean(dC25, na.rm = TRUE),
    dC25_sd   = sd(dC25, na.rm = TRUE),
    dC27_ = mean(dC27, na.rm = TRUE),
    dC27_sd   = sd(dC27, na.rm = TRUE),
    dC29_ = mean(dC29, na.rm = TRUE),
    dC29_sd   = sd(dC29, na.rm = TRUE),
    dC31_ = mean(dC31, na.rm = TRUE),
    dC31_sd   = sd(dC31, na.rm = TRUE),
    
    
    D21_ = mean(D21, na.rm = TRUE),
    D21_sd   = sd(D21, na.rm = TRUE),
    D23_ = mean(D23, na.rm = TRUE),
    D23_sd   = sd(D23, na.rm = TRUE),
    D25_ = mean(D25, na.rm = TRUE),
    D25_sd   = sd(D25, na.rm = TRUE),
    D27_ = mean(D27, na.rm = TRUE),
    D27_sd   = sd(D27, na.rm = TRUE),
    D29_ = mean(D29, na.rm = TRUE),
    D29_sd   = sd(D29, na.rm = TRUE),
    D31_ = mean(D31, na.rm = TRUE),
    D31_sd   = sd(D31, na.rm = TRUE)
  )


View(Summary_df)

wilcox.test(dC23 ~ Period, data = n_akanes_df)
wilcox.test(dC25 ~ Period, data = n_akanes_df)
wilcox.test(dC27 ~ Period, data = n_akanes_df)
wilcox.test(dC29 ~ Period, data = n_akanes_df)
wilcox.test(dC31 ~ Period, data = n_akanes_df)

wilcox.test(D23 ~ Period, data = n_akanes_df)
wilcox.test(D25 ~ Period, data = n_akanes_df)
wilcox.test(D27 ~ Period, data = n_akanes_df)
wilcox.test(D29 ~ Period, data = n_akanes_df)
wilcox.test(D31 ~ Period, data = n_akanes_df)





write.csv(Summary_df, "Summary_df.csv")

colnames(n_akanes_df)



dC21<- ggplot(n_akanes_df, aes(x = Depth)) +
  geom_rect(data = event_rects,
            aes(xmin = depth_end, xmax = depth_start,
                ymin = -Inf, ymax = Inf),
            inherit.aes = FALSE,
            fill = "grey80", alpha = 0.3) +
  geom_point(aes(y = dC21), color = "red", size = 2, shape = 16, alpha = 0.8) +
  geom_errorbar(aes(ymin = dC21 - dC21SD, ymax = dC21 + dC21SD),
                width = 0, color = "red", alpha = 0.5) +
  geom_line(data = na.omit(n_akanes_df[, c("Depth", "dC21")]), 
            aes(y = dC21), color = "red", linewidth = 1, alpha = 0.7) +
  scale_x_reverse(
  ) +
  labs(y = "dC21", x = "") +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major = element_line(color = "gray90", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(fill = NA, color = "gray70", linewidth = 0.8),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 13)
  )
dC21


dC23<- ggplot(n_akanes_df, aes(x = Depth)) +
  geom_rect(data = event_rects,
            aes(xmin = depth_end, xmax = depth_start,
                ymin = -Inf, ymax = Inf),
            inherit.aes = FALSE,
            fill = "grey80", alpha = 0.3) +
  geom_point(aes(y = dC23), color = "red", size = 2, shape = 16, alpha = 0.8) +
  geom_errorbar(aes(ymin = dC23 - dC23SD, ymax = dC23 + dC23SD),
                width = 0, color = "red", alpha = 0.5) +
  geom_line(data = na.omit(n_akanes_df[, c("Depth", "dC23")]), 
            aes(y = dC23), color = "red", linewidth = 1, alpha = 0.7) +
  scale_x_reverse(
  ) +
  labs(y = "dC23", x = "") +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major = element_line(color = "gray90", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(fill = NA, color = "gray70", linewidth = 0.8),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 13)
  )
dC23



dC25<- ggplot(n_akanes_df, aes(x = Depth)) +
  geom_rect(data = event_rects,
            aes(xmin = depth_end, xmax = depth_start,
                ymin = -Inf, ymax = Inf),
            inherit.aes = FALSE,
            fill = "grey80", alpha = 0.3) +
  geom_point(aes(y = dC25), color = "red", size = 2, shape = 16, alpha = 0.8) +
  geom_errorbar(aes(ymin = dC25 - dC25SD, ymax = dC25 + dC25SD),
                width = 0, color = "red", alpha = 0.5) +
  geom_line(data = na.omit(n_akanes_df[, c("Depth", "dC25")]), 
            aes(y = dC25), color = "red", linewidth = 1, alpha = 0.7) +
  scale_x_reverse(
  ) +
  labs(y = "dC23", x = "") +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major = element_line(color = "gray90", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(fill = NA, color = "gray70", linewidth = 0.8),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 13)
  )
dC25




dC27<- ggplot(n_akanes_df, aes(x = Depth)) +
  geom_rect(data = event_rects,
            aes(xmin = depth_end, xmax = depth_start,
                ymin = -Inf, ymax = Inf),
            inherit.aes = FALSE,
            fill = "grey80", alpha = 0.3) +
  geom_point(aes(y = dC27), color = "red", size = 2, shape = 16, alpha = 0.8) +
  geom_errorbar(aes(ymin = dC27 - dC27SD, ymax = dC27 + dC27SD),
                width = 0, color = "red", alpha = 0.5) +
  geom_line(data = na.omit(n_akanes_df[, c("Depth", "dC27")]), 
            aes(y = dC27), color = "red", linewidth = 1, alpha = 0.7) +
  scale_x_reverse(
  ) +
  labs(y = "dC27", x = "") +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major = element_line(color = "gray90", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(fill = NA, color = "gray70", linewidth = 0.8),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 13)
  )
dC27



dC29<- ggplot(n_akanes_df, aes(x = Depth)) +
  geom_rect(data = event_rects,
            aes(xmin = depth_end, xmax = depth_start,
                ymin = -Inf, ymax = Inf),
            inherit.aes = FALSE,
            fill = "grey80", alpha = 0.3) +
  geom_point(aes(y = dC29), color = "red", size = 2, shape = 16, alpha = 0.8) +
  geom_errorbar(aes(ymin = dC29 - dC29SD, ymax = dC29 + dC29SD),
                width = 0, color = "red", alpha = 0.5) +
  geom_line(data = na.omit(n_akanes_df[, c("Depth", "dC29")]), 
            aes(y = dC29), color = "red", linewidth = 1, alpha = 0.7) +
  scale_x_reverse(
  ) +
  labs(y = "dC29", x = "") +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major = element_line(color = "gray90", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(fill = NA, color = "gray70", linewidth = 0.8),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 13)
  )
dC29

dC31<- ggplot(n_akanes_df, aes(x = Depth)) +
  geom_rect(data = event_rects,
            aes(xmin = depth_end, xmax = depth_start,
                ymin = -Inf, ymax = Inf),
            inherit.aes = FALSE,
            fill = "grey80", alpha = 0.3) +
  geom_point(aes(y = dC31), color = "red", size = 2, shape = 16, alpha = 0.8) +
  geom_errorbar(aes(ymin = dC31 - dC31SD, ymax = dC31 + dC31SD),
                width = 0, color = "red", alpha = 0.5) +
  geom_line(data = na.omit(n_akanes_df[, c("Depth", "dC31")]), 
            aes(y = dC31), color = "red", linewidth = 1, alpha = 0.7) +
  scale_x_reverse(
  ) +
  labs(y = "dC31", x = "") +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major = element_line(color = "gray90", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(fill = NA, color = "gray70", linewidth = 0.8),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 13)
  )
dC31



grid.arrange(dC21, dC23, dC25, dC27, dC29, dC31, ncol =3)















# D21 plot
D21 <- ggplot(n_akanes_df, aes(x = Depth)) +
  geom_rect(data = event_rects,
            aes(xmin = depth_end, xmax = depth_start,
                ymin = -Inf, ymax = Inf),
            inherit.aes = FALSE,
            fill = "grey80", alpha = 0.3) +
  geom_point(aes(y = D21), color = "blue", size = 2, shape = 16, alpha = 0.8) +
  geom_errorbar(aes(ymin = D21 - D21SD, ymax = D21 + D21SD),
                width = 0, color = "blue", alpha = 0.5) +
  geom_line(data = na.omit(n_akanes_df[, c("Depth", "D21")]), 
            aes(y = D21), color = "blue", linewidth = 1, alpha = 0.7) +
  scale_x_reverse() +
  labs(y = "δD C21 (‰)", x = "") +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major = element_line(color = "gray90", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(fill = NA, color = "gray70", linewidth = 0.8)
  )
D21


# D23 plot
D23 <- ggplot(n_akanes_df, aes(x = Depth)) +
  geom_rect(data = event_rects,
            aes(xmin = depth_end, xmax = depth_start,
                ymin = -Inf, ymax = Inf),
            inherit.aes = FALSE,
            fill = "grey80", alpha = 0.3) +
  geom_point(aes(y = D23), color = "blue", size = 2, shape = 16, alpha = 0.8) +
  geom_errorbar(aes(ymin = D23 - D23SD, ymax = D23 + D23SD),
                width = 0, color = "blue", alpha = 0.5) +
  geom_line(data = na.omit(n_akanes_df[, c("Depth", "D23")]), 
            aes(y = D23), color = "blue", linewidth = 1, alpha = 0.7) +
  scale_x_reverse() +
  labs(y = "δD C23 (‰)", x = "") +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major = element_line(color = "gray90", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(fill = NA, color = "gray70", linewidth = 0.8)
  )

# D25 plot
D25 <- ggplot(n_akanes_df, aes(x = Depth)) +
  geom_rect(data = event_rects,
            aes(xmin = depth_end, xmax = depth_start,
                ymin = -Inf, ymax = Inf),
            inherit.aes = FALSE,
            fill = "grey80", alpha = 0.3) +
  geom_point(aes(y = D25), color = "blue", size = 2, shape = 16, alpha = 0.8) +
  geom_errorbar(aes(ymin = D25 - D25SD, ymax = D25 + D25SD),
                width = 0, color = "blue", alpha = 0.5) +
  geom_line(data = na.omit(n_akanes_df[, c("Depth", "D25")]), 
            aes(y = D25), color = "blue", linewidth = 1, alpha = 0.7) +
  scale_x_reverse() +
  labs(y = "δD C25 (‰)", x = "") +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major = element_line(color = "gray90", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(fill = NA, color = "gray70", linewidth = 0.8)
  )

# D27 plot
D27 <- ggplot(n_akanes_df, aes(x = Depth)) +
  geom_rect(data = event_rects,
            aes(xmin = depth_end, xmax = depth_start,
                ymin = -Inf, ymax = Inf),
            inherit.aes = FALSE,
            fill = "grey80", alpha = 0.3) +
  geom_point(aes(y = D27), color = "blue", size = 2, shape = 16, alpha = 0.8) +
  geom_errorbar(aes(ymin = D27 - D27SD, ymax = D27 + D27SD),
                width = 0, color = "blue", alpha = 0.5) +
  geom_line(data = na.omit(n_akanes_df[, c("Depth", "D27")]), 
            aes(y = D27), color = "blue", linewidth = 1, alpha = 0.7) +
  scale_x_reverse() +
  labs(y = "δD C27 (‰)", x = "") +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major = element_line(color = "gray90", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(fill = NA, color = "gray70", linewidth = 0.8)
  )

# D29 plot
D29 <- ggplot(n_akanes_df, aes(x = Depth)) +
  geom_rect(data = event_rects,
            aes(xmin = depth_end, xmax = depth_start,
                ymin = -Inf, ymax = Inf),
            inherit.aes = FALSE,
            fill = "grey80", alpha = 0.3) +
  geom_point(aes(y = D29), color = "blue", size = 2, shape = 16, alpha = 0.8) +
  geom_errorbar(aes(ymin = D29 - D29SD, ymax = D29 + D29SD),
                width = 0, color = "blue", alpha = 0.5) +
  geom_line(data = na.omit(n_akanes_df[, c("Depth", "D29")]), 
            aes(y = D29), color = "blue", linewidth = 1, alpha = 0.7) +
  scale_x_reverse() +
  labs(y = "δD C29 (‰)", x = "") +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major = element_line(color = "gray90", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(fill = NA, color = "gray70", linewidth = 0.8)
  )

# D31 plot
D31 <- ggplot(n_akanes_df, aes(x = Depth)) +
  geom_rect(data = event_rects,
            aes(xmin = depth_end, xmax = depth_start,
                ymin = -Inf, ymax = Inf),
            inherit.aes = FALSE,
            fill = "grey80", alpha = 0.3) +
  geom_point(aes(y = D31), color = "blue", size = 2, shape = 16, alpha = 0.8) +
  geom_errorbar(aes(ymin = D31 - D31SD, ymax = D31 + D31SD),
                width = 0, color = "blue", alpha = 0.5) +
  geom_line(data = na.omit(n_akanes_df[, c("Depth", "D31")]), 
            aes(y = D31), color = "blue", linewidth = 1, alpha = 0.7) +
  scale_x_reverse() +
  labs(y = "δD C31 (‰)", x = "") +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major = element_line(color = "gray90", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(fill = NA, color = "gray70", linewidth = 0.8)
  )

# Arrange all deuterium plots
grid.arrange(D21, D23, D25, D27, D29, D31, ncol = 3)






















# Long format + classification
df_long <- n_akanes_df %>%
  pivot_longer(cols = c(D23, D25, D27, D29, D31),
               names_to = "alkane",
               values_to = "dC") %>%
  mutate(chain_length = ifelse(alkane %in% c("D23", "D25"),
                               "Mid-chain", "Long-chain")) %>%
  arrange(alkane, Depth) %>%
  filter(!is.na(dC))

# Plot
ALL_D <- ggplot(df_long, aes(x = Depth, y = dC,
                             group = alkane)) +
  
  geom_rect(data = event_rects,
            aes(xmin = depth_end, xmax = depth_start,
                ymin = -Inf, ymax = Inf),
            inherit.aes = FALSE,
            fill = "grey80", alpha = 0.3) +
  
  geom_line(aes(color = chain_length,
                linetype = chain_length),
            linewidth = 0.7) +
  
  geom_point(aes(color = chain_length,
                 shape = alkane),
             size = 3.5) +
  
  scale_color_manual(values = c(
    "Mid-chain" = "red",   # warm (stands out)
    "Long-chain" = "black"   # cool
  )) +
  
  scale_linetype_manual(values = c(
    "Mid-chain" = "solid",
    "Long-chain" = "dashed"
  )) +
  
  scale_shape_manual(values = c(
    "D23" = 16,
    "D25" = 17,
    "D27" = 15,
    "D29" = 18,
    "D31" = 8
  )) +
  
  scale_x_reverse(
    sec.axis = sec_axis(
      ~ .,
      breaks = depth_labels,
      labels = age_breaks / 1000
    )
  ) +
  
  labs(
    y = expression(delta^{2}*H~("\u2030")),
    x = "",
    color = "Chain length",
    shape = "n-alkane",
    linetype = "Chain length"
  )+
  
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    panel.grid.major = element_line(color = "gray90", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(fill = NA, color = "gray70", linewidth = 0.8)
  )

ALL_D









colnames(n_akanes_df)

df_long <- n_akanes_df %>%
  pivot_longer(cols = c(dC23, dC25, dC27, dC29, dC31),
               names_to = "alkane",
               values_to = "dC") %>%
  mutate(chain_length = ifelse(alkane %in% c("dC23", "dC25"),
                               "Mid-chain", "Long-chain")) %>%
  arrange(alkane, Depth) %>%
  filter(!is.na(dC))

# Plot
ALL_C <- ggplot(df_long, aes(x = Depth, y = dC,
                             group = alkane)) +
  
  geom_rect(data = event_rects,
            aes(xmin = depth_end, xmax = depth_start,
                ymin = -Inf, ymax = Inf),
            inherit.aes = FALSE,
            fill = "grey80", alpha = 0.3) +
  
  geom_line(aes(color = chain_length,
                linetype = chain_length),
            linewidth = 0.7) +
  
  geom_point(aes(color = chain_length,
                 shape = alkane),
             size = 3.5) +
  
  scale_color_manual(values = c(
    "Mid-chain" = "red",   # warm (stands out)
    "Long-chain" = "black"   # cool
  )) +
  
  scale_linetype_manual(values = c(
    "Mid-chain" = "solid",
    "Long-chain" = "dashed"
  )) +
  
  scale_shape_manual(values = c(
    "dC23" = 16,
    "dC25" = 17,
    "dC27" = 15,
    "dC29" = 18,
    "dC31" = 8
  )) +
  
  scale_x_reverse(
    sec.axis = sec_axis(
      ~ .,
      breaks = depth_labels,
      labels = age_breaks / 1000
    )
  ) +
  
  labs(
    y = expression(delta^{13}*C~("\u2030")),
    x = "",
    color = "Chain length",
    shape = "n-alkane",
    linetype = "Chain length"
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    panel.grid.major = element_line(color = "gray90", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(fill = NA, color = "gray70", linewidth = 0.8)
  )

ALL_C

grid.arrange(ALL_C, ALL_D, ncol = 1)


clim_df<- read.xlsx("D:/2026/nalkanes-Verdeospesoa/Script_PollenClimate/PredictedMAT2.xlsx", 
                    sheet = "Sheet 1")

head(clim_df)



MAT<- ggplot(clim_df, aes(x = Level)) +
  geom_rect(data = event_rects,
            aes(xmin = end_age, xmax = start_age ,
                ymin = -Inf, ymax = Inf),
            inherit.aes = FALSE,
            fill = "grey80", alpha = 0.3) +
  geom_point(aes(y = MAT), color = "#FF7F00", size = 2, shape = 16, alpha = 0.8) +
  geom_line(data = na.omit(clim_df), 
            aes(y = MAT), color = "#FF7F00", linewidth = 1, alpha = 0.7) +
  scale_x_reverse(
    sec.axis = sec_axis(
      ~ .,
      breaks = depth_labels,
      labels = age_breaks/1000
    )
  ) +
  labs(y = "MAT",  x = "") +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major = element_line(color = "gray90", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(fill = NA, color = "gray70", linewidth = 0.8),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 13)
  )

MAT




clim_df<- read.xlsx("D:/2026/nalkanes-Verdeospesoa/Script_PollenClimate/PredictedMAP2.xlsx", 
                    sheet = "Sheet 1")

head(clim_df)



MAP<- ggplot(clim_df, aes(x = Level)) +
  geom_rect(data = event_rects,
            aes(xmin = end_age, xmax = start_age ,
                ymin = -Inf, ymax = Inf),
            inherit.aes = FALSE,
            fill = "grey80", alpha = 0.3) +
  geom_point(aes(y = MAP), color = "blue", size = 2, shape = 16, alpha = 0.8) +
  geom_line(data = na.omit(clim_df), 
            aes(y = MAP), color = "blue", linewidth = 1, alpha = 0.7) +
  scale_x_reverse(
    sec.axis = sec_axis(
      ~ .,
      breaks = depth_labels,
      labels = age_breaks/1000
    )
  ) +
  labs(y = "MAP",  x = "") +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major = element_line(color = "gray90", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(fill = NA, color = "gray70", linewidth = 0.8),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 13)
  )

MAP


grid.arrange(ALL_C, ALL_D, MAT, MAP, ncol = 1)


