
# --------------------------------------------------------------
# 3D Habitat Use of Loggerhead Turtles

# Javier Menéndez-Blázquez | @jmenblaz


library(ggplot2)
library(dplyr)
library(ggpubr)
library(gridExtra)




# 1) Raincloud and boxplot (result in final figures for the study in 02_3d_process/07_statistics_analysis.R)
# for 2D and 3D

# load data for plot
day3d <- read.csv(paste0(output_dir,"/01_kde_3d/kde_3d_res_day.csv"))
night3d <- read.csv(paste0(output_dir,"/01_kde_3d/kde_3d_res_night.csv"))

day2d <- read.csv(paste0(output_dir,"/02_kde_2d/kde_2d_res_day.csv"))
night2d <- read.csv(paste0(output_dir,"/02_kde_2d/kde_2d_res_night.csv"))

daynight3d <- rbind(day3d, night3d)
daynight2d <- rbind(day2d, night2d)


# 1) Box plot to compare day and night

#add columns with km3
daynight3d$volume.50.km3 <- daynight3d$volume.50/1000000000
daynight3d$volume.95.km3 <- daynight3d$volume.95/1000000000

#add columns with km2
daynight2d$area.50.km2 <- daynight2d$area.50/1000000
daynight2d$area.95.km2 <- daynight2d$area.95/1000000


## box plots
daynight3d$day.night <- as.factor(daynight3d$day.nigh)
daynight2d$day.night <- as.factor(daynight2d$day.nigh)

levels(daynight3d$day.night)


# ------------------------------------------------------------------------------
# Plot 1 -- 2D UD 50%
p1 <- ggplot(daynight2d, aes(x = day.night, y = area.50.km2, fill = day.night)) +
        # violin
        ggdist::stat_halfeye(aes(color = day.night),
                trim = FALSE,
                adjust = .4, 
                width = .3, 
                .width = 0,
                alpha = 0.45, 
                justification = -0.0, 
                point_color = NA) + 
        # Poitns (jitter)
        # geom_jitter(aes(color = day.night), width = 0.8, alpha = 0.6, size = 2, 
        #            ) +
        # geom_point(position = position_nudge(x = -0.2), width = 0.5) +
        # 
        geom_jitter(
          aes(color = day.night), alpha = 0.5, size = 2.5,
          position = position_nudge(x = - 0.05) # Desplazamiento fijo hacia un lado
        ) +
        # Boxplot
        geom_boxplot(width = 0.2, outlier.shape = NA, alpha = 0.85, color = "black") +
        # colors
        scale_fill_manual(values = c("night" = "skyblue4", "day" = "lightgoldenrod2")) +
        scale_color_manual(values = c("night" = "skyblue4", "day" = "gold2")) +
        # statistics 
        # note that that significant statistics is only provide in plot 4 (t.test applied previously in exploratory plot)
        # stat_compare_means(
        #   comparisons = list(c("night", "day")), # Comparaciones de pares
        #   label = "p.signif", # labeling "*" or p-value
        #   method = "t.test")   + # statistic method)
        
        #  Custom legend
        # annotate("point", x = 0.5, y = 47000, color = "lightgoldenrod2", size = 4.5) +
        # annotate("point", x = 0.5, y = 43000, color = "skyblue4", size = 4.5) +
        # annotate("text", x = 0.6, y = 47000, label = "Day", size = 4.5) +
        # annotate("text", x = 0.62, y = 43000, label = "Night", size = 4.5) +
        
        # theme
        theme_bw() +
        theme(axis.text.y = element_text(size = 10, family = "Arial"),
              axis.text.x = element_blank(),
              axis.title.y = element_text(size = 11, vjust = 1.2, family = "Arial"),
              axis.title.x = element_blank(), 
              axis.ticks = element_line(size = 0.5),
              axis.ticks.length.y = unit(7, "pt"),  # negative lenght -> ticks inside the plot
              axis.ticks.length.x = unit(-8, "pt"),
              # plot title
              plot.title = element_text(size = 13, family = "Arial"), 
              # legend
              legend.position = "none",  # Suprimir la leyenda 
              # legend.title = element_blank(),
              # legend.position = c(0.15, 0.1),
              # legend.text = element_text(size = 11),
              # panel
              panel.grid = element_blank(),
              panel.border = element_rect(color = "black", fill = NA, linewidth = 1)) +
              
        # labs and titles
        labs(title = "2D  UD 50%", y = expression("Home range area (km"^2*")"))
        # annotations
        # annotate("text", x = 0.55, y = 45500, label = "(a)", size = 5, family = "Arial")
p1





# Plot 2 -- 2D UD 95% # -------------------------------------------------

p2 <- ggplot(daynight2d, aes(x = day.night, y = area.95.km2, fill = day.night)) +
        # violin
        ggdist::stat_halfeye(aes(color = day.night),
                             trim = FALSE,
                             adjust = .4, 
                             width = .3, 
                             .width = 0,
                             alpha = 0.45, 
                             justification = -0.0, 
                             point_color = NA) + 
        # Poitns (jitter)
        # geom_jitter(aes(color = day.night), width = 0.8, alpha = 0.6, size = 2, 
        #            ) +
        # geom_point(position = position_nudge(x = -0.2), width = 0.5) +
        # 
        geom_jitter(
          aes(color = day.night), alpha = 0.5, size = 2.5,
          position = position_nudge(x = - 0.05) # Desplazamiento fijo hacia un lado
        ) +
        # Boxplot
        geom_boxplot(width = 0.2, outlier.shape = NA, alpha = 0.85, color = "black") +
        # colors
        scale_fill_manual(values = c("night" = "skyblue4", "day" = "lightgoldenrod2")) +
        scale_color_manual(values = c("night" = "skyblue4", "day" = "gold2")) +
        # statistics 
        # note that that significant statistics is only provide in plot 4 (t.test applied previously in exploratory plot)
        # stat_compare_means(
        #   comparisons = list(c("night", "day")), # Comparaciones de pares
        #   label = "p.signif", # labeling "*" or p-value
        #   method = "t.test")   + # statistic method)
        
        #  Custom legend
        # annotate("point", x = 0.9, y = 47000, color = "lightgoldenrod2", size = 4.5) +  
        # annotate("point", x = 0.5, y = 43000, color = "skyblue4", size = 4.5) +
        # annotate("text", x = 1.025, y = 47050, label = "Day", size = 4.5) + 
        # annotate("text", x = 0.62, y = 43000, label = "Night", size = 4.5) + 
        
        # theme
        theme_bw() +
        theme(axis.text.y = element_text(size = 10, family = "Arial"),
              axis.text.x = element_blank(),
              axis.title.y = element_text(size = 11, vjust = 1.2, family = "Arial"),
              axis.title.x = element_blank(), 
              axis.ticks = element_line(size = 0.5),
              axis.ticks.length.y = unit(7, "pt"),  # negative lenght -> ticks inside the plot
              axis.ticks.length.x = unit(-8, "pt"),
              # plot title
              plot.title = element_text(size = 13, family = "Arial"), 
              # legend
              legend.position = "none",  # Suprimir la leyenda 
              # legend.title = element_blank(),
              # legend.position = c(0.15, 0.1),
              # legend.text = element_text(size = 11),
              # panel
              panel.grid = element_blank(),
              panel.border = element_rect(color = "black", fill = NA, linewidth = 1)) +
        
        # labs and titles
        labs(title = "2D  UD 95%", y = expression("Home range area (km"^2*")"))
        # annotations
        # annotate("text", x = 0.55, y = 45500, label = "(a)", size = 5, family = "Arial")
p2


# Plot 3 -- 3D UD 50% -------------------------------------------------------

p3 <- ggplot(daynight3d, aes(x = day.night, y = volume.50.km3  , fill = day.night)) +
        # violin
        ggdist::stat_halfeye(aes(color = day.night),
                             trim = FALSE,
                             adjust = .4, 
                             width = .3, 
                             .width = 0,
                             alpha = 0.45, 
                             justification = -0.0, 
                             point_color = NA) + 
        # Poitns (jitter)
        # geom_jitter(aes(color = day.night), width = 0.8, alpha = 0.6, size = 2, 
        #            ) +
        # geom_point(position = position_nudge(x = -0.2), width = 0.5) +
        # 
        geom_jitter(
          aes(color = day.night), alpha = 0.5, size = 2.5,
          position = position_nudge(x = - 0.05) # Desplazamiento fijo hacia un lado
        ) +
        # Boxplot
        geom_boxplot(width = 0.2, outlier.shape = NA, alpha = 0.85, color = "black") +
        # colors
        scale_fill_manual(values = c("night" = "skyblue4", "day" = "lightgoldenrod2")) +
        scale_color_manual(values = c("night" = "skyblue4", "day" = "gold2")) +
        
        # statistics 
        # note that that significant statistics is only provide in plot 4 (t.test applied previously in exploratory plot)
        # stat_compare_means(
        #   comparisons = list(c("night", "day")), # Comparaciones de pares
        #   label = "p.signif", # labeling "*" or p-value
        #   method = "t.test")   + # statistic method)
        
        #  Custom legend
        # annotate("point", x = 0.9, y = 47000, color = "lightgoldenrod2", size = 4.5) +  
        # annotate("point", x = 0.5, y = 43000, color = "skyblue4", size = 4.5) +
        # annotate("text", x = 1.025, y = 47050, label = "Day", size = 4.5) + 
        # annotate("text", x = 0.62, y = 43000, label = "Night", size = 4.5) + 
        
        # theme
        theme_bw() +
        theme(axis.text.y = element_text(size = 10, family = "Arial"),
              axis.text.x = element_blank(),
              axis.title.y = element_text(size = 11, vjust = 1.2, family = "Arial"),
              axis.title.x = element_blank(), 
              axis.ticks = element_line(size = 0.5),
              axis.ticks.length.y = unit(7, "pt"),  # negative lenght -> ticks inside the plot
              axis.ticks.length.x = unit(-8, "pt"),
              # plot title
              plot.title = element_text(size = 13, family = "Arial"), 
              # legend
              legend.position = "none",  # Suprimir la leyenda 
              # legend.title = element_blank(),
              # legend.position = c(0.15, 0.1),
              # legend.text = element_text(size = 11),
              # panel
              panel.grid = element_blank(),
              panel.border = element_rect(color = "black", fill = NA, linewidth = 1)) +
        
        # labs and titles
        labs(title = "3D  UD 50%", y = expression("Home range volume (km"^3*")"))
        # annotations
        # annotate("text", x = 0.55, y = 45500, label = "(a)", size = 5, family = "Arial")

p3





# Plot 4 -- 3D UD 95% ------------------------------------------------------

p4 <- ggplot(daynight3d, aes(x = day.night, y = volume.95.km3  , fill = day.night)) +
        # violin
        ggdist::stat_halfeye(aes(color = day.night),
                             trim = FALSE,
                             adjust = .4, 
                             width = .3, 
                             .width = 0,
                             alpha = 0.45, 
                             justification = -0.0, 
                             point_color = NA) + 
        # Poitns (jitter)
        # geom_jitter(aes(color = day.night), width = 0.8, alpha = 0.6, size = 2, 
        #            ) +
        # geom_point(position = position_nudge(x = -0.2), width = 0.5) +
        # 
        geom_jitter(
          aes(color = day.night), alpha = 0.5, size = 2.5,
          position = position_nudge(x = - 0.05) # Desplazamiento fijo hacia un lado
        ) +
        # Boxplot
        geom_boxplot(width = 0.2, outlier.shape = NA, alpha = 0.85, color = "black") +
        # colors
        scale_fill_manual(values = c("night" = "skyblue4", "day" = "lightgoldenrod2")) +
        scale_color_manual(values = c("night" = "skyblue4", "day" = "gold2")) +
        
        # statistics 
        # note that that significant statistics is only provide in plot 4 (t.test applied previously in exploratory plot)
        stat_compare_means(
          comparisons = list(c("night", "day")), # Comparaciones de pares
          label = "p.signif", # labeling "*" or p-value
          method = "t.test")   + # statistic method)
        
        #  Custom legend
        # annotate("point", x = 0.9, y = 47000, color = "lightgoldenrod2", size = 4.5) +  
        # annotate("point", x = 0.5, y = 43000, color = "skyblue4", size = 4.5) +
        # annotate("text", x = 1.025, y = 47050, label = "Day", size = 4.5) + 
        # annotate("text", x = 0.62, y = 43000, label = "Night", size = 4.5) + 
        
        # theme
        theme_bw() +
        theme(axis.text.y = element_text(size = 10, family = "Arial"),
              axis.text.x = element_blank(),
              axis.title.y = element_text(size = 11, vjust = 1.2, family = "Arial"),
              axis.title.x = element_blank(), 
              axis.ticks = element_line(size = 0.5),
              axis.ticks.length.y = unit(7, "pt"),  # negative lenght -> ticks inside the plot
              axis.ticks.length.x = unit(-8, "pt"),
              # plot title
              plot.title = element_text(size = 13, family = "Arial"), 
              # legend
              legend.position = "none",  # Suprimir la leyenda 
              # legend.title = element_blank(),
              # legend.position = c(0.15, 0.1),
              # legend.text = element_text(size = 11),
              # panel
              panel.grid = element_blank(),
              panel.border = element_rect(color = "black", fill = NA, linewidth = 1)) +
        
        # labs and titles
        labs(title = "3D  UD 95%", y = expression("Home range volume (km"^3*")"))
        # annotations
        # annotate("text", x = 0.55, y = 45500, label = "(a)", size = 5, family = "Arial")

p4




# combine plots --------------------------------------------------
p <- grid.arrange(p1, p2, p3, p4, ncol = 4)
p



# export plots
output_data <- paste0(output_dir,"/fig")
if (!dir.exists(output_data)) dir.create(output_data, recursive = TRUE)

# export / save plot
p_png <- paste0(output_data,"/","fig_homarange_daynight.png")
p_svg <- paste0(output_data,"/","fig_homerange_daynight.svg")
ggsave(p_png, p, width=33, height=9.5, units="cm", dpi=350, bg="white")
ggsave(p_svg, p, width=33, height=9.5, units="cm", dpi=350, bg="white")
