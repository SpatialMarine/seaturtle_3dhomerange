# ------------------------------------------------------------------------------
# 3D Habitat Use of Loggerhead Turtles
# Vertical habitat use

# by Javier Menéndez-Blázquez | @jmenblaz

# ----------------------------------------------------------
# Plots and figures for output data of analaysis/04_habitat_use.R

# 1) Proportion time per depth by day/night time
# read and summary data
data_proportion <- read.csv(paste0(output_dir,"/04_habitat_use/01_proportion_time_depth.csv"))

stats <- data_proportion %>%
  group_by(depth_range) %>%
  summarise(
    mean = mean(proportion, na.rm = TRUE),
    sd = sd(proportion, na.rm = TRUE),
    n = n()
  )


data_proportion_day <- read.csv(paste0(output_dir,"/04_habitat_use/01_proportion_time_depth_day.csv"))

day_stats <- data_proportion_day %>%
  group_by(depth_range) %>%
  summarise(
    mean = mean(proportion, na.rm = TRUE),
    sd = sd(proportion, na.rm = TRUE),
    n = n()
  )

data_proportion_night <- read.csv(paste0(output_dir,"/04_habitat_use/01_proportion_time_depth_night.csv"))

night_stats <- data_proportion_night %>%
  group_by(depth_range) %>%
  summarise(
    mean = mean(proportion, na.rm = TRUE),
    sd = sd(proportion, na.rm = TRUE),
    n = n()
  )


n_day <- nrow(data_proportion_day)
n_night <- nrow(data_proportion_night)

# add day / night referece for plotting
day_stats$daynight <- "day"
night_stats$daynight <- "night"

# combine df for plotting
combined_stats <- rbind(day_stats, night_stats)
# rename depth-range class into numerical serie for plotting
combined_stats <- combined_stats %>%
  mutate(
    depth_start = as.numeric(sapply(strsplit(depth_range, "-"), function(x) as.numeric(x[1])))
  )

# filter deeps < 120
combined_stats <- combined_stats %>% filter(depth_start < 120)

# calculate goem bars
combined_stats <- combined_stats %>%
  mutate(
    mean_plot = ifelse(daynight == "day", -mean, mean),
    sd_plot = sd
  )

# plot proportion time by depth
# same plot as population pyramid


# read svg icons 
# sun_svg <- svgparser::read_svg(paste0(input_dir,"/other/svg/sun.svg"))
# moon_svg <- svgparser::read_svg(paste0(input_dir,"/other/svg/moon.svg"))


p <- ggplot(combined_stats, aes(x = depth_start, fill = daynight, 
                           y = ifelse(test = daynight == "day", 
                                      yes = -mean, no = mean))) + 
            geom_bar(stat = "identity", color = "grey15") +
            # error bar
            geom_errorbar(aes(ymin = mean_plot - sd_plot, ymax = mean_plot + sd_plot), 
                          width = 1, color = "grey10") +
            scale_x_reverse(labels = abs, 
                            breaks = seq(-0, 120, by = 10),
                            # limits = c(0, 120)
                            ) + 
            geom_hline(yintercept = 0, linetype = "dotted", color = "grey10", size = 0.3, alpha = 0.9) + # Línea horizontal punteada
            # scale_y_continuous(labels = abs, limits = c(-max(combined_stats$mean), max(combined_stats$mean))) +
            # scale_x_continuous(limits = c(0, 100)) + 
            scale_y_continuous(labels = abs, 
                               limits = c(-90, 90),   # Establecer límites de 0 a 75 (invertido para pirámide)
                               breaks = seq(-75, 75, by = 25)) +  # Establecer las divisiones cada 25
            
            labs(x = "Depth (m)", y = "Time proportion (%)") +
            scale_fill_manual(values = c("day" = "lightgoldenrod1", "night" = "skyblue4")) +
            coord_flip() + 
            # add icons day/night - after exportion
            # theme
            theme_bw() +
            theme(axis.text.y = element_text(vjust = -1.1, size = 11),
                  axis.text.x = element_text(size = 10),
                  axis.title.x = element_text(vjust = -0.25), 
                  axis.ticks = element_line(size = 1),
                  axis.ticks.length = unit(0, "pt"),  # negative lenght -> ticks inside the plot
                  # legend
                  legend.title = element_blank(),
                  legend.position = c(0.15, 0.1),
                  legend.text = element_text(size = 11),
                  # panel
                  panel.grid = element_blank(),
                  panel.border = element_rect(color = "black", fill = NA, linewidth = 1)
                  )
p

output_data <- paste0(output_dir,"/fig")
if (!dir.exists(output_data)) dir.create(output_data, recursive = TRUE)

# export / save plot
p_png <- paste0(output_data,"/","fig_proportion_time_depth.png")
p_svg <- paste0(output_data,"/","fig_proportion_time_depth.svg")
ggsave(p_png, p, width=12, height=15, units="cm", dpi=350, bg="white")
ggsave(p_svg, p, width=12, height=15, units="cm", dpi=350, bg="white")
















# For chnage irregular intervarls
# No good results for depths > 120, so use the first version of the plot

# 
# library(dplyr)
# library(ggplot2)
# 
# # Preparar combined_stats
# combined_stats <- combined_stats %>%
#   mutate(
#     depth_start = as.numeric(sapply(strsplit(depth_range, "-"), function(x) x[1])),
#     depth_end = as.numeric(sapply(strsplit(depth_range, "-"), function(x) x[2])),
#     mean_plot = ifelse(daynight == "day", -mean, mean),
#     sd_plot = sd
#   )
# 
# # Plot con geom_rect
# p <- ggplot(combined_stats) +
#   geom_rect(aes(xmin = mean_plot,
#                 xmax = 0,
#                 ymin = depth_start,
#                 ymax = depth_end,
#                 fill = daynight),
#             color = "grey15",
#             alpha = 0.8) +
#   
#   # Barras de error horizontales
#   geom_errorbarh(aes(xmin = mean_plot - sd_plot, 
#                      xmax = mean_plot + sd_plot,
#                      y = (depth_start + depth_end) / 2), 
#                  height = (combined_stats$depth_end - combined_stats$depth_start) * 0.6, 
#                  color = "grey10") +
#   
#   scale_y_reverse(breaks = combined_stats$depth_start, labels = combined_stats$depth_range) +
#   scale_x_continuous(labels = abs, limits = c(-100, 100), breaks = seq(-100, 100, 25)) +
#   scale_fill_manual(values = c("day" = "lightgoldenrod1", "night" = "skyblue4")) +
#   coord_flip() +
#   labs(x = "Time at Depth (%)", y = "Depth (m)") +
#   theme_bw() +
#   theme(
#     axis.text.y = element_text(size = 11),
#     axis.text.x = element_text(size = 10),
#     axis.title.x = element_text(vjust = -0.25), 
#     axis.ticks = element_line(size = 1),
#     axis.ticks.length = unit(0, "pt"),
#     legend.title = element_blank(),
#     legend.position = c(0.15, 0.1),
#     legend.text = element_text(size = 11),
#     panel.grid = element_blank(),
#     panel.border = element_rect(color = "black", fill = NA, linewidth = 1)
#   )
# 
# p
