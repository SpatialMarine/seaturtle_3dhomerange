

# --------------------------------------------------------------
# 3D Habitat Use of Loggerhead Turtles

# Javier Menéndez-Blázquez | @jmenblaz


# 1) Raincloud and boxplot (numerical results in final figures for the study in 05_tatistics_analysis/01.R)
#   for Wilcoxon test differences between fishing overlap and % of overlap in 2D and 3D.



# 1) prepare data --------------------------------------------------------------
df2d <- read.csv(paste0(output_dir,"/03_fishing_2d_overlap/2d_kde_fishing_overlap_results.csv"))
df3d <- read.csv(paste0(output_dir,"/03_fishing_3d_overlap/3d_kde_fishing_overlap_results.csv"))

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



# ------------------------------------------------------------------------------
# 2) Raincloud and boxplot        ------------------------------------------

# UD 95 --------------------------------------------------------------

ggplot(df_longUD95, aes(x = Dimension, y = Intersection, fill = Dimension)) +
  geom_boxplot() +
  facet_wrap(~ fishing_gear) +
  labs(title = "Comparation between 2D y 3D per Fishing Gear",
       x = "",
       y = "Overlap between 95UD and fishing effort (%)") +
  theme_minimal()




ggplot(df_longUD95, aes(x = Dimension, y = Intersection, fill = Dimension)) +
  # Raincloud (half-eye)
  ggdist::stat_halfeye(aes(color = Dimension),
                       trim = FALSE,
                       adjust = 0.4, 
                       width = 0.8, 
                       .width = 0,
                       alpha = 0.25, 
                       justification = -0.3, 
                       point_color = NA) + 
  # Jitter points
  geom_jitter(aes(color = Dimension), 
              alpha = 0.2, size = 2.7,
              # width = 0.2,
    position = position_nudge(x = - 0.25)
  ) +
  # Boxplot
  geom_boxplot(width = 0.3, outlier.shape = 16, outlier.color = "grey10", alpha = 0.7, color = "grey10") +
  
  # Colors
  scale_fill_manual(values = c("2D" = "deepskyblue3", "3D" = "darkorange2")) +
  scale_color_manual(values = c("2D" = "deepskyblue3", "3D" = "darkorange2")) +
  # Theme
  theme_bw() +
  theme(axis.text.y = element_text(size = 11, family = "Arial"),
        axis.text.x = element_text(size = 12, vjust = -1, face = "bold"),
        axis.title.y = element_text(size = 12, vjust = 1.2, family = "Arial"),
        axis.title.x = element_blank(), 
        axis.ticks = element_line(size = 0.5),
        axis.ticks.length.y = unit(7, "pt"),
        axis.ticks.length.x = unit(-8, "pt"),
        plot.title = element_blank(),
        legend.position = "none",
        panel.grid = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
        # strip
        strip.background = element_rect(fill = "white", color = "transparent")) +
  # Labels
  labs(title = "Comparison between 2D and 3D per Fishing Gear",
       y = "Overlap between 95UD and fishing effort (%)") +
  facet_wrap(~ fishing_gear)



# Daltonismo (Okabe-Ito)
# scale_fill_manual(values = c("2D" = "#E69F00", "3D" = "#56B4E9")) +
# scale_color_manual(values = c("2D" = "#E69F00", "3D" = "#56B4E9"))



# UD 50 -------------------------------------------------------------
# 3) Exploratory plots (final version in fig script)

# 3.1) Boxplot for Wilcoxon test result, diferences in the 

ggplot(df_longUD50, aes(x = Dimension, y = Intersection, fill = Dimension)) +
  geom_boxplot() +
  facet_wrap(~ fishing_gear) +
  labs(title = "Comparation between 2D y 3D per Fishing Gear",
       x = "",
       y = "Overlap between 50UD and fishing effort (%)") +
  theme_minimal()





