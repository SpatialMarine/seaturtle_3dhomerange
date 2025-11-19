
# --------------------------------------------------------------
# 3D Habitat Use of Loggerhead Turtles

# 3. Fishing overlap DAY/NIGHT statistics results
# by Javier Menéndez-Blázquez | @jmenblaz

# Using volumes/area  percentage and difference values calculated previously

# Day / Night values.

library(dplyr)

# 0) path
source("setup.R")

output_data <- paste0(output_dir,"/05_statistics_results")
if (!dir.exists(output_data)) dir.create(output_data, recursive = TRUE)



#  Data
df2d <- read.csv(paste0(output_dir,"/03_fishing_2d_overlap_daynight/2d_kde_fishing_overlap_results_daynight.csv"))
df3d <- read.csv(paste0(output_dir,"/03_fishing_3d_overlap_daynight/3d_kde_fishing_overlap_results_daynight.csv"))

# split into day/night - 2D/3D
df2d_day   <- df2d[df2d$daynight == "day", ]
df2d_night <- df2d[df2d$daynight == "night", ]

df3d_day   <- df3d[df3d$daynight == "day", ]
df3d_night <- df3d[df3d$daynight == "night", ]





# 1.1) Diferences between day/night 2D and 3D ---------------------------------

# -------------------
# 1.1.1) UD95 - UD95 - UD95 

# Normality test - Shapiro test
# -- NO normality ==  p < 0.05

shapiro.test(df2d_day$ud95_intersect_percentage)
shapiro.test(df3d_day$udvol95_intersect_percentage)

shapiro.test(df2d_night$ud95_intersect_percentage)
shapiro.test(df3d_night$udvol95_intersect_percentage)



# Both NO normal distribution -> Wilcoxon test signed-rank test

# Day 2D/3D --------------
wilcox.test(df2d_day$ud95_intersect_percentage,
            df3d_day$udvol95_intersect_percentage,
            paired = TRUE)

summary(df2d_day$ud95_intersect_percentage)
summary(df3d_day$udvol95_intersect_percentage)

sd(df2d_day$ud95_intersect_percentage)
sd(df3d_day$udvol95_intersect_percentage)

# results: 
  #     Wilcoxon signed rank test with continuity correction
  # 
  #     data:  df2d_day$ud95_intersect_percentage and 
  #            df3d_day$udvol95_intersect_percentage
  #     V = 1415, p-value = 4.934e-07 *** p < 0.05
  #     alternative hypothesis: true location shift is not equal to 0
  #     Note: difference between data is not 0 and significant

#  ✔  Higher values of % of intersection in 2D during the DAY than 3D for UD95


# correlation test
cor.test(df2d_day$ud95_intersect_percentage, df3d_day$udvol95_intersect_percentage, 
         method = "spearman")
#  ✔  There is a positive correlation between the percentage of intersection
#  ✔  If a individual present high percentage of intersection in 2D, it will have too in 3D (in general)
#  ✔ Individuos que presentan un mayor solapamiento en el espacio 2D, 
#   también tienden a presentar mayor solapamiento en 3D durante el día.

# Night 2D/3D --------- 
wilcox.test(df2d_night$ud95_intersect_percentage,
                         df3d_night$udvol95_intersect_percentage,
                         paired = TRUE)

# results:
    # Wilcoxon signed rank test with continuity correction
    # V = 1330, p-value = 1.454e-05
    # alternative hypothesis: true location shift is not equal to 0

#  ✔  Higher values of % of intersection in 2D during the NIGHT than 3D for UD95

summary(df2d_night$ud95_intersect_percentage)
summary(df3d_night$udvol95_intersect_percentage)

sd(df2d_night$ud95_intersect_percentage)
sd(df3d_night$udvol95_intersect_percentage)


# correlation test
cor.test(df2d_night$ud95_intersect_percentage, df3d_night$udvol95_intersect_percentage, 
         method = "spearman")
#   There is a positive correlation between the percentage of intersection
#   ✔  If a individual present high percentage of intersection in 2D,
#   ✔   t will have too in 3D  for the night (and for UD95)



# ---------------------------------------------------------------
# 1.2.1)    UD 50    -----------------

# UD50 - Day 2D/3D ---------------------------

# Normality test - Shapiro test
shapiro.test((df2d_day$ud50_intersect_percentage))
shapiro.test((df3d_day$udvol50_intersect_percentage))
# NO normality  ==  p < 0.05

# Both NO normal distribution -> Wilcoxon test signed-rank test
wilcox.test(df2d_day$ud50_intersect_percentage, df3d_day$udvol50_intersect_percentage, 
            paired = TRUE)
# results:
# V = 1389, p-value = 1.459e-06
# ✔  Higher values of % of intersection in 2D during the  DAY than 3D for UD50

summary(df2d_day$ud50_intersect_percentage)
summary(df3d_day$udvol50_intersect_percentage)

sd(df2d_day$ud50_intersect_percentage)
sd(df3d_day$udvol50_intersect_percentage)

# correlation test
cor.test(df2d_day$ud50_intersect_percentage, df3d_day$udvol50_intersect_percentage, 
         method = "spearman")

#   There is a positive correlation between the percentage of intersection
#   ✔  If a individual present high percentage of intersection in 2D,
#   ✔   t will have too in 3D  for the night (and for UD95)



# UD50 - Night 2D/3D ---------------------------
# Normality test - Shapiro test
shapiro.test((df2d_night$ud50_intersect_percentage))
shapiro.test((df3d_night$udvol50_intersect_percentage))
# NO normality p < 0.05


# Both NO normal distribution -> Wilcoxon test signed-rank test
wilcox.test(df2d_night$ud50_intersect_percentage, df3d_night$udvol50_intersect_percentage, 
            paired = TRUE)
# results:
# V = 1275, p-value = 0.0001015

summary(df2d_night$ud50_intersect_percentage)
summary(df3d_night$udvol50_intersect_percentage)

sd(df2d_night$ud50_intersect_percentage)
sd(df3d_night$udvol50_intersect_percentage)

# correlation test
cor.test(df2d_night$ud50_intersect_percentage, df3d_night$udvol50_intersect_percentage, 
         method = "spearman")

#   ✔  There is a positive correlation between the percentage of intersection

#------------------------------------------------------------------------------








# -----------------------------------------------------------------------------
# 1.2) For potential differences based in diferent fishing gears ---------------
# 2D y 3D consistence within groups (LL y TW) for day and night

#   1.2.1) for drifting longlines (LL) --------------------------
# DAY 
df2dLL_day <- df2d_day %>% filter(fishing_gear == "LL")
df3dLL_day <- df3d_day %>% filter(fishing_gear == "LL")



# UD 95 --------------------------------------------------------
# Normality test - Shapiro test
shapiro.test((df2dLL_day$ud95_intersect_percentage))  # Normal
shapiro.test((df3dLL_day$udvol95_intersect_percentage))  # Normal
# NORMALITY p > 0.05

# Both NORMAL distribution -> t.test paried
t.test(df2dLL_day$ud95_intersect_percentage, df3dLL_day$udvol95_intersect_percentage,
       paired = TRUE)

# results: 
    # Paired t-test
    # t = 7.6102, df = 27, p-value = 3.473e-08

# ✔  Higher values of % of intersection in 2D during the DAY than 3D for UD95
#    for LL drinfting longlines

summary(df2dLL_day$ud95_intersect_percentage)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 2.829  13.881  19.666  19.739  25.839  36.585
summary(df3dLL_day$udvol95_intersect_percentage)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 4.107   7.840  11.873  12.943  15.931  28.077

# correlation test
cor.test(df2dLL_day$ud95_intersect_percentage, df3dLL_day$udvol95_intersect_percentage, 
         method = "spearman")

#   ✔  There is a positive correlation between the percentage of intersection
#       If a individual present high percentage of intersection in 2D, 
#       it will have too in 3D (in general)



# UD 50 --------------------------------------------------------
# Normality test - Shapiro test
shapiro.test((df2dLL_day$ud50_intersect_percentage)) # Normal
shapiro.test((df3dLL_day$udvol50_intersect_percentage)) # No Normal

# Normal and NO normal --> upuesto importante es la normalidad de las diferencias,
diff <- df2dLL_day$ud50_intersect_percentage - df3dLL_day$udvol50_intersect_percentage
shapiro.test(diff) # Normal: W = 0.96555, p-value = 0.4676

# Normality of the difference --> Normal t-test paired -- No normal -> Wilcoxon
t.test(df2dLL_day$ud50_intersect_percentage, df3dLL_day$udvol50_intersect_percentage,
       paired = TRUE)

# results: 
# t = 6.8493, df = 27, p-value = 2.336e-07

# ✔  Higher values of % of intersection in 2D during the DAY than 3D for UD95
#    for LL drinfting longlines

summary(df2dLL_day$ud50_intersect_percentage)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 2.40   14.19   21.07   21.94   30.59   49.66 
summary(df3dLL_day$udvol50_intersect_percentage)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 2.101   3.910   7.372   9.331  11.567  33.554

# correlation test
cor.test(df2dLL_day$ud50_intersect_percentage, df3dLL_day$udvol50_intersect_percentage, 
         method = "spearman")
#   ✔  There is a positive correlation between the percentage of intersection




#   NIGHT  ----------------------------------------------------
df2dLL_night <- df2d_night %>% filter(fishing_gear == "LL")
df3dLL_night <- df3d_night %>% filter(fishing_gear == "LL")

# UD 95 ------------------------------------------
# Normality test - Shapiro test
shapiro.test((df2dLL_night$ud95_intersect_percentage))  # NO normal
shapiro.test((df3dLL_night$udvol95_intersect_percentage))  # Normal

diff <- df2dLL_night$ud95_intersect_percentage - df3dLL_night$udvol95_intersect_percentage
shapiro.test(diff) # Normal p > 0.05 : W = 0.97709, p-value = 0.776

# Both NORMAL distribution -> t.test paried
t.test(df2dLL_night$ud95_intersect_percentage, df3dLL_night$udvol95_intersect_percentage,
       paired = TRUE)

# results: 
# Paired t-test
# t = 7.5806, df = 27, p-value = 3.735e-08

# ✔  Higher values of % of intersection in 2D during the DAY than 3D for UD95
#    for LL drinfting longlines

summary(df2dLL_night$ud95_intersect_percentage)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 2.519   9.827  16.704  17.527  21.592  37.838

summary(df3dLL_night$udvol95_intersect_percentage)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 2.522   5.104   7.197   9.339  11.941  38.063

# correlation test
cor.test(df2dLL_night$ud95_intersect_percentage, df3dLL_night$udvol95_intersect_percentage, 
         method = "spearman")

#   ✔  There is a positive correlation between the percentage of intersection
#       If a individual present high percentage of intersection in 2D, 
#       it will have too in 3D (in general)



# UD 50 --------------------------------------------------------
# Normality test - Shapiro test
shapiro.test((df2dLL_night$ud50_intersect_percentage)) # Normal
shapiro.test((df3dLL_night$udvol50_intersect_percentage)) # No Normal

# Normal and NO normal --> upuesto importante es la normalidad de las diferencias,
diff <- df2dLL_night$ud50_intersect_percentage - df3dLL_night$udvol50_intersect_percentage
shapiro.test(diff) # Normal p > 0.05: W = 0.9711, p-value = 0.6105

# Normality of the difference --> Normal t-test paired -- No normal -> Wilcoxon
t.test(df2dLL_night$ud50_intersect_percentage, df3dLL_night$udvol50_intersect_percentage,
       paired = TRUE)

# results: 
# t = 6.3324, df = 27, p-value = 8.855e-07

# ✔  Higher values of % of intersection in 2D during the DAY than 3D for UD95
#    for LL drinfting longlines

summary(df2dLL_night$ud50_intersect_percentage)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.000   9.041  15.558  18.761  29.321  45.455
summary(df3dLL_night$udvol50_intersect_percentage)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.694   2.814   3.859   5.327   6.184  25.466

# correlation test
cor.test(df2dLL_day$ud50_intersect_percentage, df3dLL_day$udvol50_intersect_percentage, 
         method = "spearman")
#   ✔  There is a positive correlation between the percentage of intersection

#---------------- 
# LL ----------------------------------------






# ----------------------------------------------
#   1.2.2) for trawlers (TW) ---------------------------------------------------

df2dTW_day <- df2d_day %>% filter(fishing_gear == "TW")
df3dTW_day <- df3d_day %>% filter(fishing_gear == "TW")

# DAY----------------------------------------------------------------------

# UD 95 results --------------------------
# Normality test - Shapiro test
shapiro.test((df2dTW_day$ud95_intersect_percentage)) # No normal
shapiro.test((df3dTW_day$udvol95_intersect_percentage)) # No normal

# Both No normal -> Wilcox
wilcox.test(df2dTW_day$ud95_intersect_percentage, df3dTW_day$udvol95_intersect_percentage, 
            paired = TRUE)

# results:
# V = 315, p-value = 0.009536
# ✔  Higher values of % of intersection in 2D during the DAY than 3D for UD9

summary(df2dTW_day$ud95_intersect_percentage)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.1664  3.2299  7.7949  8.5330 10.8208 28.992

summary(df3dTW_day$udvol95_intersect_percentage)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.935   3.016   3.980   5.477   6.127  20.000

# correlation test
cor.test(df2dTW_day$ud95_intersect_percentage, df3dTW_day$udvol95_intersect_percentage, 
         method = "spearman")

#   X Not positive correlation between the percentage of intersection X



# UD 50 results -------------------

# Normality test - Shapiro test
shapiro.test((df2dTW_day$ud50_intersect_percentage)) # No normal
shapiro.test((df3dTW_day$udvol50_intersect_percentage)) # No normal

# One normla and the other NO normal distribution -> Wilcoxon test signed-rank test
wilcox.test(df2dTW_day$ud50_intersect_percentage, df3dTW_day$udvol50_intersect_percentage, 
            paired = TRUE)
# results:
#   V = 295, p-value = 0.03575
# ✔  Higher values of % of intersection in 2D during the DAY than 3D for UD9

summary(df2dTW_day$ud50_intersect_percentage)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.000   3.352   5.431   7.123  10.004  26.437

summary(df3dTW_day$udvol50_intersect_percentage)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.773   2.403   2.873   4.712   3.981  22.424 

sd(df2dTW_day$ud50_intersect_percentage)
sd(df3dTW_day$udvol50_intersect_percentage)

# correlation test
cor.test(df2dTW_day$ud50_intersect_percentage, df3dTW_day$udvol50_intersect_percentage, 
         method = "spearman")

#   X Not positive correlation between the percentage of intersection X





# NIGHT ----------------------------------------------------------------------

df2dTW_night <- df2d_night %>% filter(fishing_gear == "TW")
df3dTW_night <- df3d_night %>% filter(fishing_gear == "TW")

# night----------------------------------------------------------------------

# UD 95 results --------------------------
# Normality test - Shapiro test
shapiro.test((df2dTW_night$ud95_intersect_percentage)) # No normal
shapiro.test((df3dTW_night$udvol95_intersect_percentage)) # No normal

# Both No normal -> Wilcox
wilcox.test(df2dTW_night$ud95_intersect_percentage, df3dTW_night$udvol95_intersect_percentage, 
            paired = TRUE)

# results NOT SIGNIFICANT p > 0.05
# V = 250, p-value = 0.2946
# X X X  Similar values of % of intersection in 2D during the night than 3D for UD95 
# for trawlers during the night

summary(df2dTW_night$ud95_intersect_percentage)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.000   1.308   5.177   6.972   9.244  25.781 

summary(df3dTW_night$udvol95_intersect_percentage)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.902   2.863   4.008   5.910   6.752  21.068 

# correlation test
cor.test(df2dTW_night$ud95_intersect_percentage, df3dTW_night$udvol95_intersect_percentage, 
         method = "spearman")

#   X Not positive correlation between the percentage of intersection X




# UD 50 results -------------------

# Normality test - Shapiro test
shapiro.test((df2dTW_night$ud50_intersect_percentage)) # No normal
shapiro.test((df3dTW_night$udvol50_intersect_percentage)) # No normal

# One normla and the other NO normal distribution -> Wilcoxon test signed-rank test
wilcox.test(df2dTW_night$ud50_intersect_percentage, df3dTW_night$udvol50_intersect_percentage, 
            paired = TRUE)
# results:
#   V = 210, p-value = 0.8842
# X X X  Similar values of % of intersection in 2D during the night than 3D for UD95 
# for trawlers during the night

summary(df2dTW_night$ud50_intersect_percentage)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.000   1.253   3.068   4.732   5.126  21.795

summary(df3dTW_night$udvol50_intersect_percentage)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.694   2.404   2.752   4.499   4.155  22.026  

sd(df2dTW_night$ud50_intersect_percentage)
sd(df3dTW_night$udvol50_intersect_percentage)

# correlation test
cor.test(df2dTW_night$ud50_intersect_percentage, df3dTW_night$udvol50_intersect_percentage, 
         method = "spearman")


#   X Not positive correlation between the percentage of intersection X
















# ------------------------------------------------------------------------------
# 2) ANOVA analysis between 2D and 3D and the fishing gear
# prepare data:

# UD95 -----------------------------------------------------------------

df2d_long <- df2d %>%
  select(organismID, fishing_gear, daynight, ud95_intersect_percentage) %>%
  mutate(Dimension = "2D", Intersection = ud95_intersect_percentage) %>%
  select(-ud95_intersect_percentage)

df3d_long <- df3d %>%
  select(organismID, fishing_gear, daynight, udvol95_intersect_percentage) %>%
  mutate(Dimension = "3D", Intersection = udvol95_intersect_percentage) %>%
  select(-udvol95_intersect_percentage)

# combine dfs
df_longUD95 <- bind_rows(df2d_long, df3d_long)

# categorical variables as factor - dimension as factor
df_longUD95 <- df_longUD95 %>%
  mutate(
    Dimension = factor(Dimension, levels = c("2D", "3D")),
    fishing_gear = factor(fishing_gear),
    period = factor(daynight, levels = c("day", "night"))
  )


# ANOVA test ---------------------------------------------
aov_result <- aov(Intersection ~ Dimension * fishing_gear * daynight, data = df_longUD95)

# Resumen de los resultados del ANOVA
summary(aov_result)

# > summary(aov_result)
# Df Sum Sq Mean Sq F value   Pr(>F)    
#   Dimension                         1   1277    1277  26.308 6.47e-07 ***
#   fishing_gear                      1   3733    3733  76.897 5.43e-16 ***
#   daynight                          1    169     169   3.476   0.0636 .  
#   Dimension:fishing_gear            1    413     413   8.512   0.0039 ** 
#   Dimension:daynight                1      1       1   0.026   0.8718    
#   fishing_gear:daynight             1     77      77   1.584   0.2095    
#   Dimension:fishing_gear:daynight   1     40      40   0.827   0.3641    
#   Residuals                       216  10485      49                     
# ---                   






# interpretation:
#El efecto de la variable Dimension (2D vs 3D) es altamente significativo (p < 0.001).

# Esto indica que, en promedio, existe una diferencia sustancial en el porcentaje 
# de intersección con la actividad pesquera entre las dos dimensiones, 
# siendo los valores significativamente menores en 3D que en 2D.
# 
# El tipo de fishing gear también muestra un efecto altamente significativo (p < 0.001) 
# sobre la variable de intersección. Esto significa que los distintos tipos de artes de pesca 
# (longlines y trawlers) presentan diferencias significativas 
# en el porcentaje de solapamiento con las áreas de utilización.
# 
# La variable daynight (día vs noche) presenta un efecto marginalmente 
# significativo (p = 0.0636), lo que sugiere una posible diferencia entre periodos 
# diurnos y nocturnos, aunque esta diferencia no es estadísticamente 
# concluyente al nivel de significancia estándar.
# 
# La interacción entre Dimension y fishing gear resulta significativa (p = 0.0039), 
# lo que indica que la diferencia entre 2D y 3D depende del tipo de arte de pesca. 
# En otras palabras, el efecto del cambio de dimensión no es igual para trawlers y longlines, 
# lo que sugiere que la ganancia (o pérdida) de información al incorporar 
# la tercera dimensión varía según el tipo de pesquería.
# 
# Por el contrario, las interacciones Dimension × daynight, fishing gear × daynight, 
# y la triple interacción (Dimension × fishing gear × daynight)
# no fueron significativas (p > 0.2). Esto indica que el efecto de la dimensión y 
# del arte de pesca no cambia significativamente entre el día y la noche, 
# y no hay evidencia de efectos combinados complejos entre las tres variables.


# Interpretation (UD95)
  
# The effect of the Dimension variable (2D vs 3D) is highly significant (p < 0.001).
# This indicates that, on average, there is a substantial difference 
# in the percentage of overlap with fishing activity between the two dimensions, 
# with 3D estimates showing significantly lower overlap than 2D ones.
# 
# The type of fishing gear also has a highly significant effect (p < 0.001) 
# on overlap values. This means that longlines and trawlers differ significantly 
# in the percentage of spatial overlap with turtle utilization distributions.
# 
# The variable daynight (day vs night) shows a marginally significant 
# effect (p = 0.0636), suggesting a possible difference between daytime 
# and nighttime overlaps, although this difference does not reach conventional statistical significance.
# 
# The interaction between Dimension and fishing gear is significant (p = 0.0039), 
# indicating that the difference between 2D and 3D estimates depends 
# on the type of fishing gear. In other words, the effect of including depth 
# varies between trawlers and longlines, suggesting that the benefit (or reduction)
# in overlap when using 3D UDs is not uniform across gear types.
# 
# In contrast, the interactions Dimension × daynight, 
# Fishing gear × daynight, and the three-way interaction (Dimension × Fishing gear × Day/Night) 
# are not statistically significant (p > 0.2). 
# This implies that the effects of dimension and gear type do not significantly 
# differ between day and night, and there is no evidence for complex combined 
# effects between all three variables.






# UD50 ----------------------------------------------

df2d_long <- df2d %>%
  select(organismID, fishing_gear, daynight, ud50_intersect_percentage) %>%
  mutate(Dimension = "2D", Intersection = ud50_intersect_percentage) %>%
  select(-ud50_intersect_percentage)

df3d_long <- df3d %>%
  select(organismID, fishing_gear, daynight, udvol50_intersect_percentage) %>%
  mutate(Dimension = "3D", Intersection = udvol50_intersect_percentage) %>%
  select(-udvol50_intersect_percentage)

# combine dfs
df_longUD50 <- bind_rows(df2d_long, df3d_long)

# dimension as factor
df_longUD50$Dimension <- factor(df_longUD50$Dimension, levels = c("2D", "3D"))

# ANOVA test ---------------------------------------------
aov_result <- aov(Intersection ~ Dimension * fishing_gear * daynight, data = df_longUD50)

# Resumen de los resultados del ANOVA
summary(aov_result)

# > summary(aov_result)
# Df Sum Sq Mean Sq F value   Pr(>F)    
# Dimension                         1   2880    2880  43.598 3.08e-10 ***
# fishing_gear                      1   4116    4116  62.303 1.46e-13 ***
# daynight                          1    335     335   5.075   0.0253 *  
# Dimension:fishing_gear            1   1916    1916  29.006 1.88e-07 ***
# Dimension:daynight                1      6       6   0.097   0.7557    
# fishing_gear:daynight             1     73      73   1.110   0.2932    
# Dimension:fishing_gear:daynight   1     32      32   0.478   0.4903    
# Residuals                       216  14269      66                     



# Interpration

# The effect of Dimension (2D vs 3D) is highly significant (p < 0.001), 
# indicating a substantial difference in overlap between the two types of utilization 
# distributions. Specifically, 3D estimates result in significantly lower 
# overlap with fishing activity compared to 2D estimates within the core areas (50% UD).
# 
# The effect of fishing gear is also highly significant (p < 0.001), 
# meaning that longlines and trawlers differ notably in how much their 
# fishing activity overlaps with turtle core areas.
# 
# The variable daynight (day vs night) has a significant effect (p = 0.025), 
# suggesting that overlap with fishing activity differs between day and night. 
# This result implies a temporal component in how turtle core areas intersect 
# with fisheries, regardless of dimension or gear.
# 
# The interaction between Dimension and fishing gear is again highly significant 
# (p < 0.001), showing that the magnitude of the difference between 
# 2D and 3D varies depending on the fishing gear. 
# 
# This reinforces that depth-related refinement in UDs affects 
# overlap differently for longlines and trawlers.
# 
# On the other hand, the interactions Dimension × daynight, 
# Fishing gear × daynight, and the three-way interaction (Dimension × Fishing gear × Day/Night) 
# are not statistically significant (p > 0.2), suggesting that 
# the effects of dimension and fishing gear are consistent across day and night periods.


# In summary:

# There is a highly significant reduction in overlap when using 3D UDs vs 2D.
# Fishing gear type significantly affects the degree of overlap.
# Overlap differs between day and night, indicating a temporal effect.
# The impact of using 3D UDs varies by gear type (strong interaction).
# No significant interactions with day/night were detected, suggesting
# the dimensional and gear effects are consistent across time of day.






# -------------------------------------------------------------------------------
# 3) Exploratory plots (final version in fig script)

# UD 95 --------------------------------------------------------

# 3.1) Boxplot for Wilcoxon t-test result, diferences in the 

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
# The error bars visualize the variability in the data.

# Supplementary Figure (sup fig)
# p_barplot95 <- ggplot(df_longUD95, aes(x = Dimension, y = Intersection, fill = interaction(fishing_gear, Dimension))) +
#         stat_summary(fun = mean, geom = "bar", position = "dodge") +
#         stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.2, position = position_dodge(0.9)) +
#         facet_wrap(~ fishing_gear) +
#         labs(title = "",
#              x = "",
#              y = "Overlap between UD595 and fishing effort (%)") +
#         ylim(0,100) + 
#         # theme
#         theme_bw() +
#         theme(axis.title.y = element_text(size = 11, margin = margin(r = 10)),  # space in title
#               axis.text.y = element_text(size = 11),
#               axis.text.x = element_text(vjust = -2, size = 12.5, face = "bold"),
#               axis.ticks = element_line(size = 1),
#               axis.ticks.length = unit(0, "pt"),  # longitudes negativas -> ticks dentro del plot
#               # Leyenda
#               legend.title = element_blank(),
#               legend.position = 'none',
#               legend.text = element_text(size = 11),
#               # Panel
#               panel.grid.major.y = element_line(color = "grey95"),
#               panel.grid.major.x = element_blank(),
#               panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
#               # facet_wrap adjustments
#               strip.background = element_blank(),  # remove facet background
#               strip.text = element_text(size = 12),  # adjust facet labels size
#         ) +
#         scale_fill_manual(values = c("TW.2D" = "#FF9678", "TW.3D" = "#41436A",  # Lighter and darker for TW
#                                      "LL.2D" = "#FF9678", "LL.3D" = "#41436A")) +  # Lighter and darker for LLL
#         geom_jitter(width = 0.2, size = 2, color = "black", alpha = 0.5)  # Add outliers (jittered points)
# 


# p_barplot95 <- ggplot(df_longUD95, aes(x = Dimension, y = Intersection, fill = interaction(fishing_gear, Dimension))) +
#   stat_summary(fun = mean, geom = "bar", position = "dodge") +
#   stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.2, position = position_dodge(0.9)) +
#   facet_grid(daynight ~ fishing_gear) +
#   labs(
#     x = "",
#     y = "Overlap between 95% UD and fishing effort (%)"
#   ) +
#   ylim(0, 100) +
#   scale_fill_manual(values = c("TW.2D" = "#FF9678", "TW.3D" = "#41436A",
#                                "LL.2D" = "#FF9678", "LL.3D" = "#41436A")) +
#   geom_jitter(width = 0.2, size = 2, color = "black", alpha = 0.5) +
#   theme_bw() +
#   theme(
#     axis.title.y = element_text(size = 11, margin = margin(r = 10)),
#     axis.text.y = element_text(size = 11),
#     axis.text.x = element_text(vjust = -2, size = 12.5, face = "bold"),
#     axis.ticks = element_line(size = 1),
#     axis.ticks.length = unit(0, "pt"),
#     legend.title = element_blank(),
#     legend.position = 'none',
#     legend.text = element_text(size = 11),
#     panel.grid.major.y = element_line(color = "grey95"),
#     panel.grid.major.x = element_blank(),
#     panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
#     strip.background = element_blank(),
#     strip.text = element_text(size = 12)
#   )
# 
# p_barplot95








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

# # Supplementary Figure (sup fig)
# p_barplot50 <- ggplot(df_longUD50, aes(x = Dimension, y = Intersection, fill = interaction(fishing_gear, Dimension))) +
#                   stat_summary(fun = mean, geom = "bar", position = "dodge") +
#                   stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.2, position = position_dodge(0.9)) +
#                   facet_wrap(~ fishing_gear) +
#                   labs(title = "",
#                        x = "",
#                        y = "Overlap between UD50 and fishing effort (%)") +
#                   # theme
#                   theme_bw() +
#                   theme(axis.title.y = element_text(size = 11, margin = margin(r = 8)),  # space in title
#                         axis.text.y = element_text(size = 11),
#                         axis.text.x = element_text(vjust = -1, size = 12.5, face = "bold"),
#                         axis.ticks = element_line(size = 1),
#                         axis.ticks.length = unit(0, "pt"),  # longitudes negativas -> ticks dentro del plot
#                         # Leyenda
#                         legend.title = element_blank(),
#                         legend.position = 'none',
#                         legend.text = element_text(size = 11),
#                         # Panel
#                         # panel.grid.major.y = element_line(color = "grey95"),
#                         panel.grid.major.x = element_blank(),
#                         panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
#                         # facet_wrap adjustments
#                         strip.background = element_blank(),  # remove facet background
#                         strip.text = element_text(size = 12),  # adjust facet labels size
#                   ) +
#                   scale_fill_manual(values = c("TW.2D" = "#FF9678", "TW.3D" = "#41436A",  # Lighter and darker for TW
#                                                "LL.2D" = "#FF9678", "LL.3D" = "#41436A")) +  # Lighter and darker for LL
#                   geom_jitter(width = 0.2, size = 2, color = "black", alpha = 0.5)  # Add outliers (jittered points)
# 
# 


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



# 
# 
# # # export / save plot
# output_fig <- paste0(output_dir,"/fig")
# 
# 
# # combine UD50 y UD95 plots   -------------------------------------------------
# # barplot plots --------------------------------------------------
# p <- grid.arrange(p_barplot50, p_barplot95, nrow = 2)
# p
# 
# # export / save plot
# p_png <- paste0(output_fig,"/","sup_fig_barplot_UD50-95.png")
# p_svg <- paste0(output_fig,"/","sup_fig_barplot_UD50-95.svg")
# ggsave(p_png, p, width=16, height=22, units="cm", dpi=350, bg="white")
# ggsave(p_svg, p, width=16, height=22, units="cm", dpi=350, bg="white")
# 
# 
# 
# 
# # interaction  plots --------------------------------------------------
# p <- grid.arrange(p_interaction50, p_interaction95, ncol = 2)
# p
# 
# 
# # export / save plot
# p_png <- paste0(output_fig,"/","sup_fig_interaction_plot_UD50-95.png")
# p_svg <- paste0(output_fig,"/","sup_fig_interaction_plot_UD50-95.svg")
# ggsave(p_png, p, width=23, height=13, units="cm", dpi=350, bg="white")
# ggsave(p_svg, p, width=23, height=13, units="cm", dpi=350, bg="white")


