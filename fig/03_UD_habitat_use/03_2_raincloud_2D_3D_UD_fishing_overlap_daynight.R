

# --------------------------------------------------------------
# 3D Habitat Use of Loggerhead Turtles

# Javier Menéndez-Blázquez | @jmenblaz


# 1) Raincloud and boxplot (numerical results in final figures for the study in 05_tatistics_analysis/01.R)
#   for Wilcoxon test differences between fishing overlap and % of overlap in 2D and 3D.

# For day and night results


library(ggplot2)
library(dplyr)
library(ggdist)
library(ggpubr)  # para stat_compare_means

# 0) Paths

# # export / save plot
output_fig <- paste0(output_dir,"/fig")

# 1) prepare data --------------------------------------------------------------
df2d <- read.csv(paste0(output_dir,"/03_fishing_2d_overlap_daynight/2d_kde_fishing_overlap_results_daynight.csv"))
df3d <- read.csv(paste0(output_dir,"/03_fishing_3d_overlap_daynight/3d_kde_fishing_overlap_results_daynight.csv"))

# UD95 -------------------------------------------------------------------------
df2d_long <- df2d %>%
  select(organismID, fishing_gear, daynight, ud95_intersect_percentage) %>%
  mutate(Dimension = "2D", Intersection = ud95_intersect_percentage) %>%
  select(-ud95_intersect_percentage)

df3d_long <- df3d %>%
  select(organismID, fishing_gear, daynight, udvol95_intersect_percentage) %>%
  mutate(Dimension = "3D", Intersection = udvol95_intersect_percentage) %>%
  select(-udvol95_intersect_percentage)

df_longUD95 <- bind_rows(df2d_long, df3d_long)
df_longUD95$Dimension <- factor(df_longUD95$Dimension, levels = c("2D", "3D"))
df_longUD95$UD <- "95UD"

# UD50 -------------------------------------------------------------------------
df2d_long <- df2d %>%
  select(organismID, fishing_gear, daynight, ud50_intersect_percentage) %>%
  mutate(Dimension = "2D", Intersection = ud50_intersect_percentage) %>%
  select(-ud50_intersect_percentage)

df3d_long <- df3d %>%
  select(organismID, fishing_gear, daynight, udvol50_intersect_percentage) %>%
  mutate(Dimension = "3D", Intersection = udvol50_intersect_percentage) %>%
  select(-udvol50_intersect_percentage)

df_longUD50 <- bind_rows(df2d_long, df3d_long)
df_longUD50$Dimension <- factor(df_longUD50$Dimension, levels = c("2D", "3D"))
df_longUD50$UD <- "50UD"

# 2) Combinar y limpiar --------------------------------------------------------

df_combined <- bind_rows(df_longUD50, df_longUD95)

df_combined <- df_combined %>%
  mutate(
    UD = factor(UD, levels = c("50UD", "95UD")),
    fishing_gear = factor(fishing_gear, levels = c("LL", "TW")),
    daynight = factor(daynight, levels = c("day", "night")),
    Dimension = factor(Dimension, levels = c("2D", "3D"))
  )

# 3) Cálculo de medias y error estándar ----------------------------------------

df_summary <- df_combined %>%
  group_by(UD, fishing_gear, daynight, Dimension) %>%
  summarise(
    mean_intersection = mean(Intersection, na.rm = TRUE),
    se_intersection = sd(Intersection, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

# 4) Gráfico final -------------------------------------------------------------

p <- ggplot(df_combined, aes(x = Dimension, y = Intersection, fill = Dimension)) +
        # # # Raincloud (half-eye)
        # ggdist::stat_halfeye(aes(color = Dimension),
        #                      trim = TRUE,
        #                      adjust = 1,
        #                      width = 1,
        #                      .width = 0,
        #                      alpha = 0.5,
        #                      justification = -0.0,
        #                      point_color = NA) +
        # Jitter points
        geom_jitter(aes(color = Dimension),
                    shape = 21,
                    stroke = 0.9,
                    alpha = 0.15,
                    size = 2,
                    position = position_jitter(width = 0.22)) +
        # Boxplot
        geom_boxplot(width = 0.3,
                     outlier.shape = 16,
                     outlier.color = "grey5",
                     outliers = FALSE,
                     alpha = 0.85,
                     color = "grey5",
                     size = 0.5) +
        # significant
        ggpubr::stat_compare_means(
          comparisons = list(c("2D", "3D")),
          label = "p.signif",
          method = "wilcox.test",
          size = 4.5,
          paired = TRUE,
          bracket.size = 0.5
        ) +
        # colors (points and boxplots)
        scale_fill_manual(values = c("2D" = "#FF9678", "3D" = "#41436A")) +
        scale_color_manual(values = c("2D" = "#FF9678", "3D" = "#41436A")) +
        # scales axis
        scale_y_continuous(limits = c(0, 80), breaks = seq(0, 80, by = 10)) +
        # facets
        facet_grid(UD ~ fishing_gear + daynight, labeller = labeller(
          fishing_gear = c("LL" = "Longlines", "TW" = "Trawlers"),
          daynight = c("day" = "Day", "night" = "Night")
        )) +
        # theme
        theme_bw() +
        theme(
          axis.text.y = element_text(size = 10, family = "Arial", color = "grey10"),
          axis.text.x = element_text(size = 12, vjust = -1, face = "bold", color = "grey10"),
          axis.title.y = element_text(size = 11, vjust = 1.2, family = "Arial"),
          axis.title.x = element_blank(),
          axis.ticks = element_line(size = 0.5),
          axis.ticks.length.y = unit(7, "pt"),
          axis.ticks.length.x = unit(-8, "pt"),
          plot.title = element_blank(),
          legend.position = "none",
          panel.grid = element_blank(),
          panel.border = element_rect(color = "grey15", fill = NA, linewidth = 0.8),
          strip.background = element_rect(fill = "white", color = "transparent"),
          strip.text = element_text(size = 12, color = "black")
        ) +
        # labels
        labs(
          y = "UD – Fishing Overlap (%)"
        )

p


# ggplot(df_summary, aes(x = Dimension, y = mean_intersection, fill = Dimension)) +
#   geom_bar(stat = "identity", position = "dodge", color = "black") +
#   geom_errorbar(aes(ymin = mean_intersection - se_intersection,
#                     ymax = mean_intersection + se_intersection),
#                 width = 0.2, position = position_dodge(0.9)) +
#   facet_grid(UD ~ fishing_gear + daynight, labeller = labeller(
#     fishing_gear = c("LL" = "Longlines", "TW" = "Trawlers"),
#     daynight = c("day" = "Day", "night" = "Night")
#   )) +
#   scale_fill_manual(values = c("2D" = "#FF9678", "3D" = "#41436A")) +
#   labs(x = "", y = "Overlap between UD and fishing effort (%)") +
#   ylim(0, 100) +
#   theme_bw() +
#   theme(
#     strip.background = element_blank(),
#     strip.text = element_text(size = 12, face = "bold"),
#     axis.title.y = element_text(size = 12, margin = margin(r = 10)),
#     axis.text.x = element_text(size = 11, face = "bold"),
#     legend.position = "none",
#     legend.title = element_blank()
#   )




# export / save plot
p_png <- paste0(output_fig,"/","fig_raincloud_overlap_UDs_daynight.png")
p_svg <- paste0(output_fig,"/","fig_raincloud_overlap_UDs_danight.svg")
ggsave(p_png, p, width=22, height=15, units="cm", dpi=350, bg="white")
ggsave(p_svg, p, width=22, height=15, units="cm", dpi=350, bg="white")


