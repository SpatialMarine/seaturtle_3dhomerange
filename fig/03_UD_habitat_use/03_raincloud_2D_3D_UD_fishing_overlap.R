

# --------------------------------------------------------------
# 3D Habitat Use of Loggerhead Turtles

# Javier Menéndez-Blázquez | @jmenblaz


# 1) Raincloud and boxplot (numerical results in final figures for the study in 05_tatistics_analysis/01.R)
#   for Wilcoxon test differences between fishing overlap and % of overlap in 2D and 3D.


library(ggpubr)

# 0) Paths

# # export / save plot
output_fig <- paste0(output_dir,"/fig")


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

p <- ggplot(df_longUD95, aes(x = Dimension, y = Intersection, fill = Dimension)) +
        # Raincloud (half-eye)
        ggdist::stat_halfeye(aes(color = Dimension),
                             trim = FALSE,
                             adjust = 0.4, 
                             width = 0.5, 
                             .width = 0,
                             alpha = 0.5, 
                             justification = -0.0, 
                             point_color = NA) + 
        # Jitter points
        geom_jitter(aes(color = Dimension),
                    shape = 21,  # Permite definir color de borde y relleno
                    # color = "grey10",  # Color del borde
                    stroke = 0.9,  # Grosor del bor
                    alpha = 0.35, size = 3,
                    # width = 0.2,
          position = position_nudge(x = - 0.07)
        ) +
        # Boxplot
        geom_boxplot(width = 0.27, outlier.shape = 16, outlier.color = "grey5", alpha = 0.95, color = "grey5", size = 0.65) +
        # Colors
        scale_fill_manual(values = c("2D" = "#FF9678", "3D" = "#41436A")) +
        scale_color_manual(values = c("2D" = "#FF9678", "3D" = "#41436A")) +
        # labels and breaks Y axi
        scale_y_continuous(limits = c(0, 110), breaks = seq(0, 100, by = 25)) +
        # stats
        stat_compare_means(
          comparisons = list(c("2D", "3D")), # Comparaciones de pares
          label = "p.signif", # labeling "*" or p-value
          method = "wilcox.test",
          size = 4.5,
          paired = TRUE,
          bracket.size = 0.5,
          #vjust = -1 
          )   + # statistic method)
        # ****: p <= 0.0001
  
        # Theme
        theme_bw() +
        theme(axis.text.y = element_text(size = 11, family = "Arial", color = "grey10"),
              axis.text.x = element_text(size = 12, vjust = -1, face = "bold", color = "grey10"),
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

p

# export / save plot
p_png <- paste0(output_fig,"/","fig_raincloud_overlap_UD95.png")
p_svg <- paste0(output_fig,"/","fig_raincloud_overlap_UD95.svg")
ggsave(p_png, p, width=20, height=13, units="cm", dpi=350, bg="white")
ggsave(p_svg, p, width=20, height=13, units="cm", dpi=350, bg="white")


  






# UD 50 -------------------------------------------------------------

p <- ggplot(df_longUD50, aes(x = Dimension, y = Intersection, fill = Dimension)) +
  # Raincloud (half-eye)
  ggdist::stat_halfeye(aes(color = Dimension),
                       trim = FALSE,
                       adjust = 0.4, 
                       width = 0.5, 
                       .width = 0,
                       alpha = 0.5, 
                       justification = -0.0, 
                       point_color = NA) + 
  # Jitter points
  geom_jitter(aes(color = Dimension),
              shape = 21,  # Permite definir color de borde y relleno
              # color = "grey10",  # Color del borde
              stroke = 0.9,  # Grosor del bor
              alpha = 0.35, size = 3,
              # width = 0.2,
              position = position_nudge(x = - 0.07)) +
  # Boxplot
  geom_boxplot(width = 0.27, outlier.shape = 16, outlier.color = "grey5", alpha = 0.95, color = "grey5", size = 0.65) +
  # Colors
  scale_fill_manual(values = c("2D" = "#FF9678", "3D" = "#41436A")) +
  scale_color_manual(values = c("2D" = "#FF9678", "3D" = "#41436A")) +
  # labels and breaks Y axi
  scale_y_continuous(limits = c(0, 110), breaks = seq(0, 100, by = 25)) +
  # stats
  stat_compare_means(
    comparisons = list(c("2D", "3D")), 
    label = "p.signif",
    method = "wilcox.test",
    size = 4.5,
    paired = TRUE,
    bracket.size = 0.5) + 
  # Theme
  theme_bw() +
  theme(axis.text.y = element_text(size = 11, family = "Arial", color = "grey10"),
        axis.text.x = element_text(size = 12, vjust = -1, face = "bold", color = "grey10"),
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
       y = "Overlap between 50UD and fishing effort (%)") +
  facet_wrap(~ fishing_gear)

p

# export / save plot
p_png <- paste0(output_fig,"/","fig_raincloud_overlap_UD50.png")
p_svg <- paste0(output_fig,"/","fig_raincloud_overlap_UD50.svg")
ggsave(p_png, p, width=20, height=13, units="cm", dpi=350, bg="white")
ggsave(p_svg, p, width=20, height=13, units="cm", dpi=350, bg="white")


