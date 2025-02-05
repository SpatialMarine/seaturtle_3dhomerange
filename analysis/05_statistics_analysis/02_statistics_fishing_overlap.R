
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

#   1.1.1) for drifting longlines (LL)
df2dLL <- df2d %>% filter(fishing_gear == "LL")
df3dLL <- df3d %>% filter(fishing_gear == "LL")

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

# correlation test
cor.test(df2dLL$ud95_intersect_percentage, df3dLL$udvol95_intersect_percentage, 
         method = "spearman")
#   There is a positive correlation between the percentage of intersection
#   If a individual present high percentage of intersection in 2D, it will have too in 3D (in general)



#   1.1.2) for trawlers (TW)
df2dTW <- df2d %>% filter(fishing_gear == "TW")
df3dTW <- df3d %>% filter(fishing_gear == "TW")


# Normality test - Shapiro test
shapiro.test((df2dTW$ud95_intersect_percentage))
shapiro.test((df3dTW$udvol95_intersect_percentage))

# One normla and the other NO normal distribution -> Wilcoxon test signed-rank test
wilcox.test(df2dTW$ud95_intersect_percentage, df3dTW$udvol95_intersect_percentage, 
            paired = TRUE)

# correlation test
cor.test(df2dTW$ud95_intersect_percentage, df3dTW$udvol95_intersect_percentage, 
         method = "spearman")




# UD50 results
############################################
############################################
############################################
############################################








# ------------------------------------------------------------------------------
# 2) ANOVA analysis between 2D and 3D and the fishing gear
# prepare data:

# UD95
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





################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
# RUN RESULTS ---------------------------------------------------------------
# UD50
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

################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################










# -------------------------------------------------------------------------------
# 3) Exploratory plots (final version in fig script)

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
ggplot(df_longUD95, aes(x = Dimension, y = Intersection, fill = Dimension)) +
  stat_summary(fun = mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.2, position = position_dodge(0.9)) +
  facet_wrap(~ fishing_gear) +
  labs(title = "Comparación de Medias de Intersección entre 2D y 3D por Fishing Gear (UD95)",
       x = "Dimensión",
       y = "Promedio de Porcentaje de Intersección UD95") +
  theme_minimal()


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
interaction.plot(x.factor = df_longUD95$Dimension, 
                 trace.factor = df_longUD95$fishing_gear, 
                 response = df_longUD95$Intersection,
                 fun = mean, 
                 xlab = "Dimensión", 
                 ylab = "Porcentaje de Intersección",
                 trace.label = "Fishing Gear",
                 main = "Interacción entre Dimensión y Fishing Gear UD95")




