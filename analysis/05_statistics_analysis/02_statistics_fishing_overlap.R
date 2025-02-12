
# --------------------------------------------------------------
# 3D Habitat Use of Loggerhead Turtles

# Fishing overlap statistics results
# by Javier Menéndez-Blázquez | @jmenblaz

# Using volumes/area  percentage and difference values calculated previously


# 0) path
source("setup")

output_data <- paste0(output_dir,"/05_statistics_results")
if (!dir.exists(output_data)) dir.create(output_data, recursive = TRUE)

# 1) Statisticla analysis  -----------------------------------------------------
# 1) Load data
df2d <- read.csv(paste0(output_dir,"/03_fishing_2d_overlap/2d_kde_fishing_overlap_results.csv"))
df3d <- read.csv(paste0(output_dir,"/03_fishing_3d_overlap/3d_kde_fishing_overlap_results.csv"))

# Normality test - Shapiro test
shapiro.test((df2d$ud95_intersect_percentage))
shapiro.test((df3d$udvol95_intersect_percentage))
# NO normality p < 0.05


# Both NO normal distribution -> Wilcoxon test signed-rank test
wilcox.test(df2d$ud95_intersect_percentage, df3d$udvol95_intersect_percentage, 
            paired = TRUE)

summary(df2d$ud95_intersect_percentage)
summary(df3d$udvol95_intersect_percentage)

# results:
    # Wilcoxon signed rank test with continuity correction

    # data:  df2d$ud95_intersect_percentage and df3d$udvol95_intersect_percentage
    # V = 1593, p-value = 9.124e-11
    # alternative hypothesis: true location shift is not equal to 0

# correlation test
cor.test(df2d$ud95_intersect_percentage, df3d$udvol95_intersect_percentage, 
       method = "spearman")
#   There is a positive correlation between the percentage of intersection
#   If a individual present high percentage of intersection in 2D, it will have too in 3D (in general)






# 1.1) For potential differences based in diferent fishing gears --------------------
# 2D y 3D consistence withint groups (LL y TW)

#   1.1.1) for drifting longlines (LL) --------------------------
df2dLL <- df2d %>% filter(fishing_gear == "LL")
df3dLL <- df3d %>% filter(fishing_gear == "LL")

# UD 95 --------------------------------------------------------
# Normality test - Shapiro test
shapiro.test((df2dLL$ud95_intersect_percentage))
shapiro.test((df3dLL$udvol95_intersect_percentage))

# Both NO normal distribution -> Wilcoxon test signed-rank test
wilcox.test(df2dLL$ud95_intersect_percentage, df3dLL$udvol95_intersect_percentage, 
            paired = TRUE)
# results: 
    # Wilcoxon signed rank exact test
    # 
    # data:  df2dLL$ud95_intersect_percentage and df3dLL$udvol95_intersect_percentage
    # V = 405, p-value = 1.49e-08
    # alternative hypothesis: true location shift is not equal to 0

summary(df2dLL$ud95_intersect_percentage)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 8.511  32.602  46.216  46.804  58.103  95.152

summary(df3dLL$udvol95_intersect_percentage)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 6.34   14.15   18.73   23.60   30.95   63.11

# correlation test
cor.test(df2dLL$ud95_intersect_percentage, df3dLL$udvol95_intersect_percentage, 
         method = "spearman")
#   There is a positive correlation between the percentage of intersection
#   If a individual present high percentage of intersection in 2D, it will have too in 3D (in general)



# UD 50 --------------------------------------------------------
# Normality test - Shapiro test
shapiro.test((df2dLL$ud50_intersect_percentage)) # Normal
shapiro.test((df3dLL$udvol50_intersect_percentage)) # No Normal

# Both NO normal distribution -> Wilcoxon test signed-rank test
wilcox.test(df2dLL$ud50_intersect_percentage, df3dLL$udvol50_intersect_percentage, 
            paired = TRUE)
# results: 
# Wilcoxon signed rank exact test
# 
# data:  df2dLL$ud50_intersect_percentage and df3dLL$udvol50_intersect_percentage
# V = 405, p-value = 1.49e-08
# alternative hypothesis: true location shift is not equal to 0

summary(df2dLL$ud50_intersect_percentage)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 5.842  34.886  43.871  51.108  72.176 100.000
summary(df3dLL$udvol50_intersect_percentage)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.759   4.924   7.830  10.302  12.228  25.041

# correlation test
cor.test(df2dLL$ud50_intersect_percentage, df3dLL$udvol50_intersect_percentage, 
         method = "spearman")








#   1.1.2) for trawlers (TW) ---------------------------------------------------

df2dTW <- df2d %>% filter(fishing_gear == "TW")
df3dTW <- df3d %>% filter(fishing_gear == "TW")

# UD 95 results --------------------------
# Normality test - Shapiro test
shapiro.test((df2dTW$ud95_intersect_percentage))
shapiro.test((df3dTW$udvol95_intersect_percentage))

# One normla and the other NO normal distribution -> Wilcoxon test signed-rank test
wilcox.test(df2dTW$ud95_intersect_percentage, df3dTW$udvol95_intersect_percentage, 
            paired = TRUE)


summary(df2dTW$ud95_intersect_percentage)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 8.511  22.883  33.674  32.773  39.513  67.015 

summary(df3dTW$udvol95_intersect_percentage)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 2.270   3.086   4.137   5.923   7.409  20.000

# correlation test
cor.test(df2dTW$ud95_intersect_percentage, df3dTW$udvol95_intersect_percentage, 
         method = "spearman")



# UD 50 results -------------------

# Normality test - Shapiro test
shapiro.test((df2dTW$ud50_intersect_percentage))
shapiro.test((df3dTW$udvol50_intersect_percentage))

# One normla and the other NO normal distribution -> Wilcoxon test signed-rank test
wilcox.test(df2dTW$ud50_intersect_percentage, df3dTW$udvol50_intersect_percentage, 
            paired = TRUE)


summary(df2dTW$ud50_intersect_percentage)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 9.091  21.054  30.525  33.799  46.271  63.351 

summary(df3dTW$udvol50_intersect_percentage)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.759   2.456   2.927   4.505   3.992  21.48

# correlation test
cor.test(df2dTW$ud50_intersect_percentage, df3dTW$udvol50_intersect_percentage, 
         method = "spearman")

# Same results in Wilcoxon tests
# # check some proof:
# summary(df2dTW$ud50_intersect_percentage)
# summary(df3dTW$udvol50_intersect_percentage)
# 
# summary(df2dTW$ud95_intersect_percentage)
# summary(df3dTW$udvol95_intersect_percentage)
# 
# all(df2dTW$ud50_intersect_percentage == df2dTW$ud95_intersect_percentage)
# all(df3dTW$udvol50_intersect_percentage == df3dTW$udvol95_intersect_percentage)
# 
# diff_50 <- df2dTW$ud50_intersect_percentage - df3dTW$udvol50_intersect_percentage
# diff_95 <- df2dTW$ud95_intersect_percentage - df3dTW$udvol95_intersect_percentage
# 
# summary(diff_50)
# summary(diff_95)
# 
# wilcox.test(diff_50, diff_95, paired = TRUE)
# 
# hist(diff_50, breaks = 10, col = rgb(1, 0, 0, 0.5), main = "Diferencias", xlab = "Valor")
# hist(diff_95, breaks = 10, col = rgb(0, 0, 1, 0.5), add = TRUE)
# legend("topright", legend = c("50%", "95%"), fill = c(rgb(1, 0, 0, 0.5), rgb(0, 0, 1, 0.5)))






# ------------------------------------------------------------------------------
# 2) ANOVA analysis between 2D and 3D and the fishing gear
# prepare data:

# UD95 -----------------------------------------------------------------

df2d_long <- df2d %>%
  select(organismID, fishing_gear, ud95_intersect_percentage) %>%
  mutate(Dimension = "2D", Intersection = ud95_intersect_percentage) %>%
  select(-ud95_intersect_percentage)

df3d_long <- df3d %>%
  select(organismID, fishing_gear, udvol95_intersect_percentage) %>%
  mutate(Dimension = "3D", Intersection = udvol95_intersect_percentage) %>%
  select(-udvol95_intersect_percentage)

# combine dfs
df_longUD95 <- bind_rows(df2d_long, df3d_long)
# dimension as factor
df_longUD95$Dimension <- factor(df_longUD95$Dimension, levels = c("2D", "3D"))

# ANOVA test ---------------------------------------------
aov_result <- aov(Intersection ~ Dimension * fishing_gear, data = df_longUD95)

# Resumen de los resultados del ANOVA
summary(aov_result)

# results of ANOVA
    # > summary(aov_result)
    #                         Df  Sum Sq Mean Sq F value Pr(>F)    
    # Dimension                1  17538   17538  82.646 5.41e-15 ***
    # fishing_gear             1   7039    7039  33.169 8.05e-08 ***
    # Dimension:fishing_gear   1     93      93   0.439    0.509    
    # Residuals              108  22918     212                     
    # ---
    #   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


# interpretation:
# El efecto de la variable Dimension (2D vs 3D) es altamente significativo (p < 0.001). 
# Esto indica que, en promedio, existe una diferencia considerable en la variable 
# dependiente (Intersection) entre las dos dimensiones. 
# Es decir, el porcentaje de intersección difiere significativamente entre 2D y 3D.

# El tipo de fishing_gear también muestra un efecto altamente significativo 
# sobre la variable Intersection (p < 0.001). 
# Esto significa que los diferentes tipos de artes de pesca presentan diferencias 
# significativas en el porcentaje de intersección.

# La interacción entre Dimension y fishing_gear no es significativa (p = 0.509). 
# Esto indica que la diferencia en el porcentaje de intersección entre 2D y 3D es 
# similar para los distintos tipos de fishing gear. 
# En otras palabras, el efecto de la dimensión es consistente 
# a lo largo de los diferentes tipos de artes de pesca

# Existe una diferencia muy significativa en el porcentaje de intersección entre 2D y 3D.
# El tipo de fishing gear también influye significativamente en el porcentaje de intersección.
# Sin embargo, la diferencia entre dimensiones no varía significativamente según el tipo de fishing gear (no hay interacción significativa

# En resumen, aunque cada grupo muestra diferencias significativas entre 2D y 3D, 
# la ANOVA sugiere que no hay una diferencia estadísticamente significativa 
# en la magnitud del cambio entre los distintos fishing gear. 
# Esto se traduce en que el efecto del cambio de dimensión se aplica de manera 
# consistente en todos los tipos analizados.


# Interpretation
# The effect of the Dimension variable (2D vs 3D) is highly significant (p < 0.001). 
# This indicates that, on average, there is a considerable difference in 
# the dependent variable (Intersection) between the two dimensions. 
# In other words, the intersection percentage differs significantly between 2D and 3D.
# 
# The type of fishing gear also shows a highly significant effect on the 
# Intersection variable (p < 0.001). This means that different types of fishing gear 
# present significant differences in the intersection percentage.
# 
# The interaction between Dimension and fishing gear is not significant (p = 0.509). 
# This indicates that the difference in intersection percentage between 2D and 3D 
# is similar for different types of fishing gear. In other words, 
# the effect of dimension is consistent across the various types of fishing gear.
# 
# There is a very significant difference in intersection percentage between 2D and 3D. 
# The type of fishing gear also significantly influences the intersection percentage. 
# However, the difference between dimensions does not vary significantly according 
# to the type of fishing gear (no significant interaction).
# 
# In summary, although each group shows significant differences between 2D and 3D, 
# the ANOVA suggests that there is no statistically significant difference in 
# the magnitude of the change between the different fishing gears. 
# This means that the effect of the dimensional change is consistently 
# applied across all analyzed types.




# UD50 ----------------------------------------------

df2d_long <- df2d %>%
  select(organismID, fishing_gear, ud50_intersect_percentage) %>%
  mutate(Dimension = "2D", Intersection = ud50_intersect_percentage) %>%
  select(-ud50_intersect_percentage)

df3d_long <- df3d %>%
  select(organismID, fishing_gear, udvol50_intersect_percentage) %>%
  mutate(Dimension = "3D", Intersection = udvol50_intersect_percentage) %>%
  select(-udvol50_intersect_percentage)

# combine dfs
df_longUD50 <- bind_rows(df2d_long, df3d_long)
# dimension as factor
df_longUD50$Dimension <- factor(df_longUD50$Dimension, levels = c("2D", "3D"))

# ANOVA test ---------------------------------------------
aov_result <- aov(Intersection ~ Dimension * fishing_gear, data = df_longUD50)

# Resumen de los resultados del ANOVA
summary(aov_result)

# > summary(aov_result)
#                           Df Sum Sq Mean Sq F value   Pr(>F)    
#   Dimension                1  34398   34398 137.729  < 2e-16 ***
#   fishing_gear             1   3737    3737  14.963 0.000188 ***
#   Dimension:fishing_gear   1    928     928   3.715 0.056563 .  
#   Residuals              108  26974     250                     











# -------------------------------------------------------------------------------
# 3) Exploratory plots (final version in fig script)

# UD 95 --------------------------------------------------------

# 3.1) Boxplot for Wilcoxon test result, diferences in the 

# These allow for a clear comparison of the distribution of the Intersection variable 
# between 2D and 3D for each fishing gear type, highlighting the median, 
# quartiles, and potential outliers. It's ideal for visualizing the differences observed in the Wilcoxon test.


ggplot(df_longUD95, aes(x = Dimension, y = Intersection, fill = Dimension)) +
  geom_boxplot() +
  facet_wrap(~ fishing_gear) +
  labs(title = "Comparation between 2D y 3D per Fishing Gear",
       x = "",
       y = "Overlap between 95UD and fishing effort (%)") +
  theme_minimal()

# 3.2) Bar plot for ANOVA results ------------------------

# # Bar Plots with Error Bars (Mean ± Error)
# # These are very useful for highlighting the differences in the means between 2D and 3D, 
# as well as between the different fishing gears, complementing the information from the ANOVA. 
# The error bars make it easier to visualize the variability in the data.

# Supplementary Figure (sup fig)
p_barplot95 <- ggplot(df_longUD95, aes(x = Dimension, y = Intersection, fill = interaction(fishing_gear, Dimension))) +
        stat_summary(fun = mean, geom = "bar", position = "dodge") +
        stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.2, position = position_dodge(0.9)) +
        facet_wrap(~ fishing_gear) +
        labs(title = "",
             x = "",
             y = "Overlap between UD595 and fishing effort (%)") +
        ylim(0,100) + 
        # theme
        theme_bw() +
        theme(axis.title.y = element_text(size = 11, margin = margin(r = 10)),  # space in title
              axis.text.y = element_text(size = 11),
              axis.text.x = element_text(vjust = -2, size = 12.5, face = "bold"),
              axis.ticks = element_line(size = 1),
              axis.ticks.length = unit(0, "pt"),  # longitudes negativas -> ticks dentro del plot
              # Leyenda
              legend.title = element_blank(),
              legend.position = 'none',
              legend.text = element_text(size = 11),
              # Panel
              panel.grid.major.y = element_line(color = "grey95"),
              panel.grid.major.x = element_blank(),
              panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
              # facet_wrap adjustments
              strip.background = element_blank(),  # remove facet background
              strip.text = element_text(size = 12),  # adjust facet labels size
        ) +
        scale_fill_manual(values = c("TW.2D" = "#FF9678", "TW.3D" = "#41436A",  # Lighter and darker for TW
                                     "LL.2D" = "#FF9678", "LL.3D" = "#41436A")) +  # Lighter and darker for LLL
        geom_jitter(width = 0.2, size = 2, color = "black", alpha = 0.5)  # Add outliers (jittered points)



# 3.3) Interaction plot --------------------

# # Interaction Plot 
# # Although the interaction was not significant, 
# this plot is valuable for illustrating that the effect of changing dimension is consistent across the different fishing gear types. 
# It visually shows how Intersection varies (or not) when transitioning from 2D to 3D for each group.

# Aunque la interacción no fue significativa, 
# este gráfico es valioso para ilustrar que el efecto del cambio de dimensión (2D/3D) es
# consistente entre los diferentes tipos de fishing gear. 
# Permite visualizar, de forma gráfica, cómo varía (o no) la Intersection entre el fishing effort y el home range
# al pasar de 2D a 3D en cada grupo

# Supplementary Figure (sup fig)
# png(paste0(output_data,"/UD95_interaction_plot.png"), width = 2100, height = 2100, res = 300)
# interaction.plot(x.factor = df_longUD95$Dimension, 
#                  trace.factor = df_longUD95$fishing_gear, 
#                  response = df_longUD95$Intersection,
#                  fun = mean, 
#                  xlab = "", 
#                  ylab = "Overlap between 95UD and fishing effort (%)",
#                  trace.label = "Fishing Gear",
#                  main = "")
# 
# dev.off()


# For Supplementary material:
# Calculatte mean by combination of dimension and fishing gear
df_summary <- df_longUD95 %>%
  group_by(Dimension, fishing_gear) %>%
  summarise(Mean_Intersection = mean(Intersection, na.rm = TRUE), .groups = "drop")

p_interaction95 <- ggplot(df_summary, aes(x = Dimension, y = Mean_Intersection, color = fishing_gear, group = fishing_gear)) +
  geom_line(size = 2) +    # Line that conect points
  geom_point(size = 3) +   # points
  labs(x = "", 
       y = "Overlap between UD95 and fishing effort (%)",
       color = "Fishing Gear") +
  # values color
  scale_color_manual(values = c("TW" = "#FF9678", "LL" = "#41436A")) +
  # theme
  theme_bw() +
  theme(axis.title.y = element_text(size = 12, margin = margin(r = 8)),  # space in title
        axis.text.y = element_text(size = 11),
        axis.text.x = element_text(vjust = -1, size = 12.5, face = "bold"),
        axis.ticks = element_line(size = 1),
        axis.ticks.length = unit(0, "pt"),  # longitudes negativas -> ticks dentro del plot
        # Leyenda
        legend.title = element_blank(),
        legend.position = "top",
        legend.text = element_text(size = 11),
        # Panel
        # panel.grid.major.y = element_line(color = "grey95"),
        panel.grid.major.x = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1)
  )

p_interaction95


# 
# p_png <- paste0(output_fig,"/","sup_fig_interaction_plot_UD95.png")
# p_svg <- paste0(output_fig,"/","sup_fig_interaction_plot_UD95.svg")
# ggsave(p_png, p_interaction95, width=12, height=13, units="cm", dpi=350, bg="white")
# ggsave(p_svg, p_interaction95, width=12, height=13, units="cm", dpi=350, bg="white")

 


# UD 50 -------------------------------------------------------------------------------
# 3) Exploratory plots (final version in fig script)

# 3.1) Boxplot for Wilcoxon test result, diferences in the 

ggplot(df_longUD50, aes(x = Dimension, y = Intersection, fill = Dimension)) +
  geom_boxplot() +
  facet_wrap(~ fishing_gear) +
  labs(title = "Comparation between 2D y 3D per Fishing Gear",
       x = "",
       y = "Overlap between 50UD and fishing effort (%)") +
  theme_minimal()

# 3.2) Bar plot for ANOVA results ------------------------

# Supplementary Figure (sup fig)
p_barplot50 <- ggplot(df_longUD50, aes(x = Dimension, y = Intersection, fill = interaction(fishing_gear, Dimension))) +
                  stat_summary(fun = mean, geom = "bar", position = "dodge") +
                  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.2, position = position_dodge(0.9)) +
                  facet_wrap(~ fishing_gear) +
                  labs(title = "",
                       x = "",
                       y = "Overlap between UD50 and fishing effort (%)") +
                  # theme
                  theme_bw() +
                  theme(axis.title.y = element_text(size = 11, margin = margin(r = 8)),  # space in title
                        axis.text.y = element_text(size = 11),
                        axis.text.x = element_text(vjust = -1, size = 12.5, face = "bold"),
                        axis.ticks = element_line(size = 1),
                        axis.ticks.length = unit(0, "pt"),  # longitudes negativas -> ticks dentro del plot
                        # Leyenda
                        legend.title = element_blank(),
                        legend.position = 'none',
                        legend.text = element_text(size = 11),
                        # Panel
                        # panel.grid.major.y = element_line(color = "grey95"),
                        panel.grid.major.x = element_blank(),
                        panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
                        # facet_wrap adjustments
                        strip.background = element_blank(),  # remove facet background
                        strip.text = element_text(size = 12),  # adjust facet labels size
                  ) +
                  scale_fill_manual(values = c("TW.2D" = "#FF9678", "TW.3D" = "#41436A",  # Lighter and darker for TW
                                               "LL.2D" = "#FF9678", "LL.3D" = "#41436A")) +  # Lighter and darker for LL
                  geom_jitter(width = 0.2, size = 2, color = "black", alpha = 0.5)  # Add outliers (jittered points)




# 3.3) Interaction plot --------------------

# Supplementary Figure (sup fig)
# Calculatte mean by combination of dimension and fishing gear
df_summary <- df_longUD50 %>%
  group_by(Dimension, fishing_gear) %>%
  summarise(Mean_Intersection = mean(Intersection, na.rm = TRUE), .groups = "drop")

p_interaction50 <- ggplot(df_summary, aes(x = Dimension, y = Mean_Intersection, color = fishing_gear, group = fishing_gear)) +
  geom_line(size = 2) +    # Line that conect points
  geom_point(size = 3) +   # points
  labs(x = "", 
       y = "Overlap between UD50 and fishing effort (%)",
       color = "Fishing Gear") +
  # values color
  scale_color_manual(values = c("TW" = "#FF9678", "LL" = "#41436A")) +
  # theme
  theme_bw() +
  theme(axis.title.y = element_text(size = 12, margin = margin(r = 10)),  # space in title
        axis.text.y = element_text(size = 11),
        axis.text.x = element_text(vjust = -2, size = 12.5, face = "bold"),
        axis.ticks = element_line(size = 1),
        axis.ticks.length = unit(0, "pt"),  # longitudes negativas -> ticks dentro del plot
        # Leyenda
        legend.title = element_blank(),
        legend.position = "top",
        legend.text = element_text(size = 11),
        # Panel
        # panel.grid.major.y = element_line(color = "grey95"),
        panel.grid.major.x = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1)
  )

p_interaction50

# export / save plot
# output_fig <- paste0(output_dir,"/fig")
# 
# p_png <- paste0(output_fig,"/","sup_fig_interaction_plot_UD50.png")
# p_svg <- paste0(output_fig,"/","sup_fig_interaction_plot_UD50.svg")
# ggsave(p_png, p_interaction50, width=12, height=13, units="cm", dpi=350, bg="white")
# ggsave(p_svg, p_interaction50, width=12, height=13, units="cm", dpi=350, bg="white")







# # export / save plot
output_fig <- paste0(output_dir,"/fig")


# combine UD50 y UD95 plots   -------------------------------------------------
# barplot plots --------------------------------------------------
p <- grid.arrange(p_barplot50, p_barplot95, nrow = 2)
p

# export / save plot
p_png <- paste0(output_fig,"/","sup_fig_barplot_UD50-95.png")
p_svg <- paste0(output_fig,"/","sup_fig_barplot_UD50-95.svg")
ggsave(p_png, p, width=16, height=22, units="cm", dpi=350, bg="white")
ggsave(p_svg, p, width=16, height=22, units="cm", dpi=350, bg="white")




# interaction  plots --------------------------------------------------
p <- grid.arrange(p_interaction50, p_interaction95, ncol = 2)
p


# export / save plot
p_png <- paste0(output_fig,"/","sup_fig_interaction_plot_UD50-95.png")
p_svg <- paste0(output_fig,"/","sup_fig_interaction_plot_UD50-95.svg")
ggsave(p_png, p, width=23, height=13, units="cm", dpi=350, bg="white")
ggsave(p_svg, p, width=23, height=13, units="cm", dpi=350, bg="white")


