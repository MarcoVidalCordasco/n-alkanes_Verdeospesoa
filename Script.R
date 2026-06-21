rm(list = ls()) # Clear all
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

getwd()
## CHRONO ####

library(rbacon)
library(IntCal)

# 1. Remove previous folder and create a new one
unlink("Bacon_runs/Verdeospesoa", recursive = TRUE)
dir.create("Bacon_runs", showWarnings = FALSE)
dir.create("Bacon_runs/Verdeospesoa", showWarnings = FALSE)

# 2. Calibrated radiocarbon data:

dates <- data.frame(
  labID = c( "D5", "D4", "D3", "D2", "D1", "D0"),
  age = c( 10530, 11310, 13082, 20829, 23518, 24918), # 2940
  error = c(51, 140, 109, 78, 140, 238), #41
  depth = c( 182, 196, 206, 214, 218, 225), #121
  cc = rep(0, 6)  # 1 = IntCal20
)
#
#
print(dates)

# 3. Check oldest dates, deeper in the sequence
for(i in 2:nrow(dates)) {
  if(dates$age[i] < dates$age[i-1]) {
    cat("Possible chronological issue", 
        dates$depth[i], "cm\n")
  }
}

# 4. Save
write.csv(dates, "Bacon_runs/Verdeospesoa/Verdeospesoa.csv", 
          row.names = FALSE)

# 5. Create file with all depths

write.table(data.frame(depth = seq(121, 225, by = 1)), 
            "Bacon_runs/Verdeospesoa/Verdeospesoa_depths.txt", 
            row.names = FALSE, col.names = FALSE)

# 6. Run Bacon to get suggestions for parameters
cat("\n=== PARAMETERS SUGGESTIONS ===\n")
Bacon(
  core = "Verdeospesoa",
  coredir = "Bacon_runs",
  acc.mean = 300,
  suggest = TRUE,  
  ask = TRUE  ,     
cc=0)




#### n-alanes ####

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(openxlsx)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(tidyr)
library(tidyverse)
library(patchwork)
library(knitr) 


pd_df <- read.xlsx("Data.xlsx", sheet= "Present_day")

# Scale factor to align precipitation with temperature axis
pd_df$M <- factor(pd_df$M,
  levels = c("January","February","March","April","May","June",
             "July","August","September","October","November","December"))

scale_factor <- max(pd_df$`P.Hm3`, na.rm = TRUE) /
                max(pd_df$T, na.rm = TRUE)

ggplot(pd_df, aes(x = M)) +
  geom_bar(aes(y = `P.Hm3` / scale_factor),
           stat = "identity",
           fill = "blue", alpha = 0.5)+
  geom_line(aes(y = T, group = 1), color = "red", linewidth = 1) +
  scale_y_continuous(
    name = "Temperature (°C)",
    sec.axis = sec_axis(~ . * scale_factor,
                        name = "Precipitation (P.Hm³)")
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Month")




n_akanes_df <- read.xlsx("Data.xlsx", sheet= "R")


head(n_akanes_df)



n_akanes_df$Period <- ifelse(n_akanes_df$CalAge >= 11700, "Late Pleistocene", "Early Holocene")

n_akanes_df$Period 



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
write.xlsx(Summary_df, "Summary_df.xlsx")

wilcox.test(CPI ~ Period, data = n_akanes_df)
wilcox.test(ACL ~ Period, data = n_akanes_df)
wilcox.test(PAQ ~ Period, data = n_akanes_df)
wilcox.test(TAR ~ Period, data = n_akanes_df)
wilcox.test(`c31/c29` ~ Period, data = n_akanes_df)
wilcox.test(`c31/c27` ~ Period, data = n_akanes_df)






# Boxplot Pleistocene vs Holocene
n_akanes_df$Period <- factor(n_akanes_df$Period, 
                             levels = c("Late Pleistocene", "Early Holocene"))


colores_periodos <- c(
  "Late Pleistocene" = "#2E86AB",
  "Early Holocene" = "#F18F01"
)


n_akanes_long <- n_akanes_df %>%
  pivot_longer(cols = c(CPI, ACL, PAQ, TAR, `c31/c29`, `c31/c27`),
               names_to = "Indice",
               values_to = "Valor")


n_akanes_long$Indice <- factor(n_akanes_long$Indice,
                               levels = c("CPI", "ACL", "PAQ", "TAR", "c31/c29", "c31/c27"),
                               labels = c("CPI", "ACL", "PAQ", "TAR", "C[31]/C[29]", "C[31]/C[27]"))


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






# Define n-alkane columns
alkane_columns <- paste0("C", 15:36)
colnames(n_akanes_df)
# Create long format data
n_akanes_long <- n_akanes_df %>%
  select(Depth_range,  all_of(alkane_columns)) %>%
  filter(!if_all(all_of(alkane_columns), is.na)) %>%
  pivot_longer(
    cols = all_of(alkane_columns),
    names_to = "Alkane",
    values_to = "Fraction"
  ) %>%
  mutate(
    Carbon_Number = as.numeric(gsub("C", "", Alkane)),
    Depth_range = factor(Depth_range, levels = unique(sort(Depth_range)))  # Keep depth order
  )




sort(unique(n_akanes_df$Depth_range))
sort(unique(n_akanes_long$Depth_range))


# Ensure Depth is a factor with levels in reverse order
n_akanes_long <- n_akanes_long %>%
  mutate(
    Depth = factor(Depth_range, levels = rev(sort(unique(Depth_range))))
  )

# Calculate percentage (relative abundance) for each depth
# Add a column to identify odd vs even carbon numbers
n_akanes_percentage <- n_akanes_long %>%
  group_by(Depth_range) %>%
  mutate(
    Percentage = (Fraction / sum(Fraction, na.rm = TRUE)) * 100
  ) %>%
  ungroup() %>%
  mutate(
    Parity = ifelse(Carbon_Number %% 2 == 0, "Even", "Odd")
  )

# Create faceted plot with different colors for odd/even


n_akanes_percentage$Depth_range <- factor(
  n_akanes_percentage$Depth_range,
  levels = rev(sort(unique(n_akanes_percentage$Depth_range)))
)

percentage_plot <- ggplot(n_akanes_percentage, aes(x = Carbon_Number, y = Percentage, fill = Parity)) +
  geom_col(alpha = 0.8, width = 0.8) +
  facet_wrap(~ Depth_range , ncol = 4) +
  labs(
    x = "Carbon Number",
    y = "Relative Abundance (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
    plot.subtitle = element_text(hjust = 0.5, size = 9),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 7),
    axis.text.y = element_text(size = 7),
    strip.text = element_text(face = "bold", size = 8),
    strip.background = element_rect(fill = "gray90"),
    panel.spacing = unit(0.5, "lines"),
    panel.grid.major.x = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(face = "bold")
  ) +
  scale_x_continuous(
    breaks = 15:36,
    labels = paste0("C", 15:36),
    expand = c(0.02, 0.02)
  ) +
  scale_fill_manual(
    values = c("Even" = "#FF9800", "Odd" ="#1E88E5" ),  # Blue for odd, orange for even
    labels = c("Even", "Odd")
  )

# Display the plot
print(percentage_plot)












NGRIP_df <- read.xlsx("Data.xlsx", sheet = "NGRIP", detectDates = FALSE)
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

dating_df <- data.frame(
  age = c(10530, 11310, 13082, 20829, 23518, 24918),
  error = c(51, 140, 109, 78, 140, 238),
  depth = c(182, 196, 206, 214, 218, 225)
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


# Create breaks
age_breaks <- seq(
  from = floor(min(n_akanes_df$CalAge, na.rm = TRUE)/5000)*5000,
  to = ceiling(max(n_akanes_df$CalAge, na.rm = TRUE)/5000)*5000,
  by = 5000
)

# Find depths correponding with the closes ages
find_closest_depth <- function(age_break) {
  closest_idx <- which.min(abs(n_akanes_df$CalAge - age_break))
  n_akanes_df$Depth[closest_idx]
}

depth_labels <- sapply(age_breaks, find_closest_depth)
ncol(n_akanes_df)

C31_C29<- ggplot(n_akanes_df, aes(x = Depth)) +
  geom_rect(data = event_rects,
            aes(xmin = depth_end, xmax = depth_start,
                ymin = -Inf, ymax = Inf),
            inherit.aes = FALSE,
            fill = "grey80", alpha = 0.3) +
  geom_point(aes(y = `c31/c29`), color = "#FF7F00", size = 2, shape = 16, alpha = 0.8) +
  geom_line(data = na.omit(n_akanes_df[, c("Depth", "c31/c29")]), 
            aes(y = `c31/c29`), color = "#FF7F00", linewidth = 1, alpha = 0.7) +
  
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



grid.arrange(NGRIP_plot,  CPI,  C31_C29, ACL, PAQ, TAR, C31_C27, ncol= 1)



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



# Prepare data for short- and mid-chain alkanes (C19, C21, C23, C25)
df_short_mid <- n_akanes_df %>%
  pivot_longer(cols = c(dC19, dC21, dC23, dC25),
               names_to = "alkane",
               values_to = "dC") %>%
  mutate(alkane_clean = case_when(
    alkane == "dC19" ~ "C₁₉",
    alkane == "dC21" ~ "C₂₁",
    alkane == "dC23" ~ "C₂₃",
    alkane == "dC25" ~ "C₂₅"
  )) %>%
  arrange(alkane, Depth) %>%
  filter(!is.na(dC))

# Plot
SHORT_MID_C <- ggplot(df_short_mid, aes(x = Depth, y = dC, group = alkane)) +
  
  geom_rect(data = event_rects,
            aes(xmin = depth_end, xmax = depth_start,
                ymin = -Inf, ymax = Inf),
            inherit.aes = FALSE,
            fill = "grey80", alpha = 0.3) +
  
  geom_line(aes(color = alkane_clean, linetype = alkane_clean),
            linewidth = 0.8) +
  
  geom_point(aes(color = alkane_clean, shape = alkane_clean),
             size = 3) +
  
  scale_color_manual(name = "n-alkane",
                     values = c(
                       "C₁₉" = "blue",
                       "C₂₁" = "red",
                       "C₂₃" = "orange",
                       "C₂₅" = "purple"
                     )) +
  
  scale_linetype_manual(name = "n-alkane",
                        values = c(
                          "C₁₉" = "dotted",
                          "C₂₁" = "solid",
                          "C₂₃" = "dashed",
                          "C₂₅" = "dotdash"
                        )) +
  
  scale_shape_manual(name = "n-alkane",
                     values = c(
                       "C₁₉" = 16,
                       "C₂₁" = 17,
                       "C₂₃" = 15,
                       "C₂₅" = 18
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
    x = "Depth (cm)",
    title = expression("δ"^{13}*"C of short- and mid-chain n-alkanes")
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    panel.grid.major = element_line(color = "gray90", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(fill = NA, color = "gray70", linewidth = 0.8)
  )

print(SHORT_MID_C)





# Prepare data for long-chain alkanes (C27, C29, C31)
df_long <- n_akanes_df %>%
  pivot_longer(cols = c(dC27, dC29, dC31),
               names_to = "alkane",
               values_to = "dC") %>%
  mutate(alkane_clean = case_when(
    alkane == "dC27" ~ "C27",
    alkane == "dC29" ~ "C29",
    alkane == "dC31" ~ "C31"
  )) %>%
  arrange(alkane, Depth) %>%
  filter(!is.na(dC))

# Plot
LONG_C <- ggplot(df_long, aes(x = Depth, y = dC, group = alkane)) +
  
  geom_rect(data = event_rects,
            aes(xmin = depth_end, xmax = depth_start,
                ymin = -Inf, ymax = Inf),
            inherit.aes = FALSE,
            fill = "grey80", alpha = 0.3) +
  
  geom_line(aes(color = alkane_clean, linetype = alkane_clean),
            linewidth = 0.8) +
  
  geom_point(aes(color = alkane_clean, shape = alkane_clean),
             size = 3) +
  
  scale_color_manual(name = "n-alkane",
                     values = c(
                       "C27" = "darkgreen",
                       "C29" = "darkorange",
                       "C31" = "brown"
                     )) +
  
  scale_linetype_manual(name = "n-alkane",
                        values = c(
                          "C27" = "solid",
                          "C29" = "dashed",
                          "C31" = "dotted"
                        )) +
  
  scale_shape_manual(name = "n-alkane",
                     values = c(
                       "C27" = 16,
                       "C29" = 17,
                       "C31" = 18
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
    x = "Depth (cm)",
    title = expression("δ"^{13}*"C of long-chain n-alkanes (C27, C29, C31)")
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    panel.grid.major = element_line(color = "gray90", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(fill = NA, color = "gray70", linewidth = 0.8)
  )

print(LONG_C)






# Prepare data for short- and mid-chain alkanes (D21, D23, D25)
df_short_mid_D <- n_akanes_df %>%
  pivot_longer(cols = c(D21, D23, D25),
               names_to = "alkane",
               values_to = "dD") %>%
  mutate(alkane_clean = case_when(
    alkane == "D21" ~ "C21",
    alkane == "D23" ~ "C23",
    alkane == "D25" ~ "C25"
  )) %>%
  arrange(alkane, Depth) %>%
  filter(!is.na(dD))

# Plot
SHORT_MID_D <- ggplot(df_short_mid_D, aes(x = Depth, y = dD, group = alkane)) +
  
  geom_rect(data = event_rects,
            aes(xmin = depth_end, xmax = depth_start,
                ymin = -Inf, ymax = Inf),
            inherit.aes = FALSE,
            fill = "grey80", alpha = 0.3) +
  
  geom_line(aes(color = alkane_clean, linetype = alkane_clean),
            linewidth = 0.8) +
  
  geom_point(aes(color = alkane_clean, shape = alkane_clean),
             size = 3) +
  
  scale_color_manual(name = "n-alkane",
                     values = c(
                       "C21" = "blue",
                       "C23" = "red",
                       "C25" = "purple"
                     )) +
  
  scale_linetype_manual(name = "n-alkane",
                        values = c(
                          "C21" = "solid",
                          "C23" = "dashed",
                          "C25" = "dotdash"
                        )) +
  
  scale_shape_manual(name = "n-alkane",
                     values = c(
                       "C21" = 16,
                       "C23" = 17,
                       "C25" = 18
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
    x = "Depth (cm)",
    title = expression("δ²H of short- and mid-chain n-alkanes (C21, C23, C25)")
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    panel.grid.major = element_line(color = "gray90", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(fill = NA, color = "gray70", linewidth = 0.8)
  )

print(SHORT_MID_D)




# Prepare data for short- and mid-chain alkanes (D21, D23, D25)
df_short_mid_D <- n_akanes_df %>%
  pivot_longer(cols = c(D21, D23, D25),
               names_to = "alkane",
               values_to = "dD") %>%
  mutate(alkane_clean = case_when(
    alkane == "D21" ~ "C21",
    alkane == "D23" ~ "C23",
    alkane == "D25" ~ "C25"
  )) %>%
  arrange(alkane, Depth) %>%
  filter(!is.na(dD))

# Plot
SHORT_MID_D <- ggplot(df_short_mid_D, aes(x = Depth, y = dD, group = alkane)) +
  
  geom_rect(data = event_rects,
            aes(xmin = depth_end, xmax = depth_start,
                ymin = -Inf, ymax = Inf),
            inherit.aes = FALSE,
            fill = "grey80", alpha = 0.3) +
  
  geom_line(aes(color = alkane_clean, linetype = alkane_clean),
            linewidth = 0.8) +
  
  geom_point(aes(color = alkane_clean, shape = alkane_clean),
             size = 3) +
  
  scale_color_manual(name = "n-alkane",
                     values = c(
                       "C21" = "blue",
                       "C23" = "red",
                       "C25" = "purple"
                     )) +
  
  scale_linetype_manual(name = "n-alkane",
                        values = c(
                          "C21" = "solid",
                          "C23" = "dashed",
                          "C25" = "dotdash"
                        )) +
  
  scale_shape_manual(name = "n-alkane",
                     values = c(
                       "C21" = 16,
                       "C23" = 17,
                       "C25" = 18
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
    x = "Depth (cm)",
    title = expression("δ²H of short- and mid-chain n-alkanes (C21, C23, C25)")
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    panel.grid.major = element_line(color = "gray90", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(fill = NA, color = "gray70", linewidth = 0.8)
  )

print(SHORT_MID_D)




# Prepare data for long-chain alkanes (D27, D29, D31)
df_long_D <- n_akanes_df %>%
  pivot_longer(cols = c(D27, D29, D31),
               names_to = "alkane",
               values_to = "dD") %>%
  mutate(alkane_clean = case_when(
    alkane == "D27" ~ "C27",
    alkane == "D29" ~ "C29",
    alkane == "D31" ~ "C31"
  )) %>%
  arrange(alkane, Depth) %>%
  filter(!is.na(dD))

# Plot
LONG_D <- ggplot(df_long_D, aes(x = Depth, y = dD, group = alkane)) +
  
  geom_rect(data = event_rects,
            aes(xmin = depth_end, xmax = depth_start,
                ymin = -Inf, ymax = Inf),
            inherit.aes = FALSE,
            fill = "grey80", alpha = 0.3) +
  
  geom_line(aes(color = alkane_clean, linetype = alkane_clean),
            linewidth = 0.8) +
  
  geom_point(aes(color = alkane_clean, shape = alkane_clean),
             size = 3) +
  
  scale_color_manual(name = "n-alkane",
                     values = c(
                       "C27" = "darkgreen",
                       "C29" = "darkorange",
                       "C31" = "brown"
                     )) +
  
  scale_linetype_manual(name = "n-alkane",
                        values = c(
                          "C27" = "solid",
                          "C29" = "dashed",
                          "C31" = "dotted"
                        )) +
  
  scale_shape_manual(name = "n-alkane",
                     values = c(
                       "C27" = 16,
                       "C29" = 17,
                       "C31" = 18
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
    x = "Depth (cm)",
    title = expression("δ²H of long-chain n-alkanes (C₂₇, C₂₉, C₃₁)")
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    panel.grid.major = element_line(color = "gray90", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(fill = NA, color = "gray70", linewidth = 0.8)
  )

print(LONG_D)



grid.arrange(LONG_C, LONG_D, SHORT_MID_C,  SHORT_MID_D, ncol = 2)





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





## REGIONAL PALEOCLIMATE
## 

#Load data


df <- read.xlsx("Data.xlsx", sheet= "Regional_Paleoclimate")
head(df)

plot_d13C<-ggplot(df, aes(x=Age6, y=d13C))+
  geom_rect(data = event_rects,
            aes(xmin = start_age , xmax = end_age ,
                ymin = -Inf, ymax = Inf),
            inherit.aes = FALSE,
            fill = "grey80", alpha = 0.3) +
  geom_point(color = "black", size = 2, shape = 16, alpha = 0.8)+
  geom_line(aes(y=d13C), color = "black", size = 1, shape = 16, alpha = 0.8)+
  theme_minimal(base_size = 12)+
  scale_x_reverse(limits = c(9000, 25000))

plot_d13C



plot_hydro<-ggplot(df, aes(x=Age5, y=hydro_hygrophytic))+
  geom_rect(data = event_rects,
            aes(xmin = start_age , xmax = end_age ,
                ymin = -Inf, ymax = Inf),
            inherit.aes = FALSE,
            fill = "grey80", alpha = 0.3) +
  geom_point(color = "blue", size = 2, shape = 16, alpha = 0.8)+
  geom_line(aes(y=hydro_hygrophytic), color = "blue", size = 1, shape = 16, alpha = 0.8)+
  theme_minimal(base_size = 12)+
  scale_x_reverse(limits = c(9000, 25000))

plot_hydro

plot_MAT<-ggplot(df, aes(x=Age4, y=MAT))+
  geom_rect(data = event_rects,
            aes(xmin = start_age , xmax = end_age ,
                ymin = -Inf, ymax = Inf),
            inherit.aes = FALSE,
            fill = "grey80", alpha = 0.3) +
  geom_point(color = "orange", size = 2, shape = 16, alpha = 0.8)+
  geom_line(aes(y=MAT), color = "orange", size = 1, shape = 16, alpha = 0.8)+
  theme_minimal(base_size = 12)+
  scale_x_reverse(limits = c(9000, 25000))

plot_MAT


plot_W<- ggplot(df, aes(x=Age1))+
  geom_rect(data = event_rects,
            aes(xmin = start_age , xmax = end_age ,
                ymin = -Inf, ymax = Inf),
            inherit.aes = FALSE,
            fill = "grey80", alpha = 0.3) +
                  geom_line(aes(y=W), color = "red", size = 2, shape = 16, alpha = 0.8)+
  theme_minimal(base_size = 12)+
  scale_x_reverse(limits = c(9000, 25000))
                  
plot_W            
                  
plot_Pindal<- ggplot( data=df, aes(x=Age3, y=d18OPindal))+
  geom_rect(data = event_rects,
            aes(xmin = start_age , xmax = end_age ,
                ymin = -Inf, ymax = Inf),
            inherit.aes = FALSE,
            fill = "grey80", alpha = 0.3) +
  geom_point(color = "darkblue", size = 2, shape = 16, alpha = 0.8)+
  geom_line()+
  theme_minimal(base_size = 12)+
  scale_x_reverse(limits = c(9000, 25000))+
  scale_y_reverse()

plot_Pindal



plot_Globerina <- ggplot(
  data = df,
  aes(x = Age2, y = `δ¹⁸O.(PDB).Globigerina.bulloides`)
) +
  geom_rect(data = event_rects,
            aes(xmin = start_age , xmax = end_age ,
                ymin = -Inf, ymax = Inf),
            inherit.aes = FALSE,
            fill = "grey80", alpha = 0.3) +
  geom_point(color = "darkgreen", size = 2, shape = 16, alpha = 0.8) +
  theme_minimal(base_size = 12) +
  scale_x_reverse(limits = c(25000, 9000))+
  geom_line() 

plot_Globerina





grid.arrange(plot_W, plot_Pindal,plot_d13C, plot_Globerina,plot_MAT,plot_hydro, ncol = 1)                  



### POLLEN-BASED PALEOCLIMATE RECONSTRUCTIONS
### 
#
#
rm(list = ls()) # Clear all
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(dplyr)
library(tidyr)
library(stringr)
library(tibble)
library(vegan)
library(openxlsx)
library(ggplot2)
library(gridExtra)
library(analogue)
library(rioja)
# READ DATA WITH POLLEN PERCENTAGES AND CLIMATE VARIABLES

data_df <- read.csv("EMPD.csv")
head(data_df)
nrow(data_df)
ncol(data_df)


ncol(data_df)

PercentageSpecies<- data_df[,2:552]
head(PercentageSpecies)


PercentageSpecies_clean <- PercentageSpecies[, colSums(PercentageSpecies) > 0]

MAT <- data_df$MAT
MAP <- data_df$MAP

# =========================
# MODEL
# =========================
model_mat <- WAPLS(PercentageSpecies_clean, MAT, npls = 10, CV = "loo")
model_mat
# 4
model_map <- WAPLS(PercentageSpecies_clean, MAP, npls = 10, CV = "loo")
model_map
# 5




# PREDICT


Clim_df <- read.xlsx("Data.xlsx", rowNames=FALSE,
                     colNames=TRUE, sheet="Climate_c")
head(Clim_df)
Clim_df$Age

Sites <-Clim_df[,1]
Level <-Clim_df[,2]
Sites

ncol(Clim_df)
Dataset <-Clim_df[,3:51]
head(Dataset)
colnames(Dataset)

### MAT

pred.MAT <- predict(model_mat, Dataset, tol.dw=TRUE)
pred <- as.numeric(pred.MAT$fit[,4])
rmse <- 4.2788  
CI_low <- pred - 1.96 * rmse
CI_high <- pred + 1.96 * rmse

# =========================
# RESULTADOS
# =========================
head(pred)
head(CI_low)
head(CI_high)


results_df <- data.frame(
  Prediction = pred,
  CI_low = CI_low,
  CI_high = CI_high
)

# =========================
# EXPORT
# =========================
write.xlsx(results_df,
           file = "WA_PLS_results.xlsx",
           rowNames = FALSE)






### MAT

pred.MAP <- predict(model_map, Dataset, tol.dw=TRUE)
pred <- as.numeric(pred.MAP$fit[,5])
rmse <- 310.0428  
CI_low <- pred - 1.96 * rmse
CI_high <- pred + 1.96 * rmse

# =========================
# RESULTS
# =========================
head(pred)
head(CI_low)
head(CI_high)


results_df <- data.frame(
  Prediction = pred,
  CI_low = CI_low,
  CI_high = CI_high
)

# =========================
# EXPORTAR A EXCEL
# =========================
write.xlsx(results_df,
           file = "WA_PLS_resultsMAP.xlsx",
           rowNames = FALSE)



# PLOT
df <- as.data.frame(pred.MAT$fit)
MAT_plot<- ggplot(df, aes(x = Level, y = df$`Comp05`)) +
  geom_point() +          
  geom_line()+
  labs(x = "Level", y = "MAT") +
  scale_x_continuous(limits = c(6000, 28000)) + 
  theme_minimal()  

df <- as.data.frame(pred.MAP$fit)
MAP_plot<- ggplot(df, aes(x = Level, y = df$`Comp05`)) +
  geom_point() +          
  geom_line()+
  labs(x = "Level", y = "MAP") +
  scale_x_continuous(limits = c(6000, 28000)) +
  theme_minimal()         

gridExtra::grid.arrange(MAT_plot, MAP_plot)