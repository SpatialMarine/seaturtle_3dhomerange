

# --------------------------------------------------------------
# 3D Habitat Use of Loggerhead Turtles

# Javier Menéndez-Blázquez | @jmenblaz


# 1) Daily Mean and Maximumn depths of sea turtle and mean daily Vertical Movement Rate (VMR)

# 2) Time series of mean vertical movement rate (VMR) for day and nightime for tracked organism

# See vertical habitat use analysis ofr previoulsy created data


library(gridExtra)
library(ggnewscale)

# Panel exploratorio de movimientos verticales (ver si los buceos so n más rapidos, 
# profundos, etc por el dia, noche, meses iluminación de luna (da cara al hábitat modelling). Horton et al. 2025


# 0) Prepare and summary data
# load data (generated in 01_habitat_use_analysis.R)

# Read previously exported data
data <- read.csv(paste0(input_dir,"/tracking/dives/dives_metrics.csv"))

# As factor
data$season <- as.factor(data$season)
data$daynight <- as.factor(data$daynight)
data$moon_bright_class <- as.factor(data$moon_bright_class)

# summary
# mean daily maximum depth (day and night time) and VMR
data$day_of_year <- as.numeric(format(as.Date(data$date), "%j"))

# summary and stats per day and daynight period)
stats <- data %>%
  group_by(day_of_year, daynight) %>%
  summarise(
    mean_maxdep = mean(maxdep, na.rm = TRUE),
    mean_meandep = mean(meandep, na.rm = TRUE),
    mean_VMRd = mean(VMRd, na.rm = TRUE),
    Lower = mean_maxdep - sd(maxdep, na.rm = TRUE), 
    Upper = mean_maxdep + sd(maxdep, na.rm = TRUE)  
  ) %>%
  ungroup()


# summary and stats per day and daynight period)
stats <- data %>%
  group_by(day_of_year, daynight) %>%
  summarise(
    mean_maxdep = mean(maxdep, na.rm = TRUE),
    mean_meandep = mean(meandep, na.rm = TRUE),
    mean_VMRd = mean(VMRd, na.rm = TRUE),
    Lower_maxdep = mean_maxdep - sd(maxdep, na.rm = TRUE), 
    Upper_maxdep = mean_maxdep + sd(maxdep, na.rm = TRUE),
    Lower_meandep = mean_meandep - sd(meandep, na.rm = TRUE),
    Upper_meandep = mean_meandep + sd(meandep, na.rm = TRUE),
    Lower_VMRd = mean_VMRd - sd(VMRd, na.rm = TRUE),
    Upper_VMRd = mean_VMRd + sd(VMRd, na.rm = TRUE)
  ) %>%
  ungroup()

# create a column to disply the days in correct order and center (+15 days)
stats <- stats %>%
  mutate(day_of_year_shifted = (day_of_year + 15) %% 365)  # Desplazar y asegurar el ciclo anual


# calculate number of individual per year day
individuals_per_day_total <- data %>%
  group_by(day_of_year) %>%
  summarise(individuals_count = n_distinct(organismID), .groups = 'drop')

# Unir la información al dataframe de estadísticas
stats <- left_join(stats, individuals_per_day_total, by = "day_of_year")




# 1) Plot mean depths max depths by day and night
pmean <- ggplot(stats, aes(x = day_of_year_shifted, y = mean_meandep, color = daynight)) +
            # Standard deviation
            geom_ribbon(aes(ymin = Lower_meandep, ymax = Upper_meandep, fill = daynight), alpha = 0.25, color = NA) +
            
            # Sombra gris para 'day' y 'night'
            stat_smooth(data = subset(stats, daynight == "day"), aes(color = NULL), 
                        method = "loess", se = FALSE, size = 1.5, color = "#61490e", span = 0.02) +
            stat_smooth(data = subset(stats, daynight == "night"), aes(color = NULL), 
                        method = "loess", se = FALSE, size = 1.5, color = "#003040", span = 0.02) +
            
            # smooth line 
            stat_smooth(method = "loess", se = FALSE, size = 1.1, span = 0.02) +
            # Puntos o línea secundaria
            geom_point(size = 0.5, alpha = 0.8) +
  
            # scale
            scale_x_continuous(breaks = (c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335) + 15),
                               labels = month.abb, expand = c(0,0)) +
          
  
           scale_y_reverse(limits = c(100, -6)) +

            # colors
           scale_fill_manual(values = c("night" = "skyblue4", "day" = "gold3")) +
           scale_color_manual(values = c("night" = "skyblue4", "day" = "gold3")) +
  
            #  # New color scale for tile bar
            #  new_scale_fill() +
            # # tile bar with number of individual per day
            # geom_tile(aes(x = day_of_year_shifted, y = 75, fill = individuals_count), 
            #           width = 1.7, height = 3.2, color = NA) +
            # scale_fill_gradient(low = "#C5C6FF", high = "#333553", 
            #                     guide = guide_colorbar(title = "Tagged sea turtles")) +
  
           theme_bw() +
           theme(axis.title.y = element_text(size = 10),
                  axis.title.x = element_blank(),
                  axis.text = element_text(size = 10),
                  panel.grid = element_blank(),
                  # legend
                  legend.position = "none",
                  # panel
                  panel.border = element_rect(color = "black", fill = NA, linewidth = 1)) +
            # labels
            labs(y = "Mean Depth (m)", x = "", color = "", fill = "")

pmean 


pmax <- ggplot(stats, aes(x = day_of_year_shifted, y = mean_maxdep, color = daynight)) +
  # Standarz deviation
  geom_ribbon(aes(ymin = Lower_maxdep, ymax = Upper_maxdep, fill = daynight), alpha = 0.25, color = NA) +
  
  # Sombra gris para 'day' y 'night'
  stat_smooth(data = subset(stats, daynight == "day"), aes(color = NULL), 
              method = "loess", se = FALSE, size = 1.5, color = "#61490e", span = 0.02) +
  stat_smooth(data = subset(stats, daynight == "night"), aes(color = NULL), 
              method = "loess", se = FALSE, size = 1.5, color = "#003040", span = 0.02) +
  
  # smooth line 
  stat_smooth(method = "loess", se = FALSE, size = 1.1, span = 0.02) +
  # Puntos o línea secundaria
  geom_point(size = 0.5, alpha = 0.8) +
  # scale
  scale_x_continuous(breaks = (c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335) + 15),
                     labels = month.abb, expand = c(0,0)) +
  scale_y_reverse(limits = c(100, -6)) +
  # colors
  scale_fill_manual(values = c("night" = "skyblue4", "day" = "gold3")) +
  scale_color_manual(values = c("night" = "skyblue4", "day" = "gold3")) +
  
   # New color scale for tile bar
   new_scale_fill() +
   # tile bar with number of individual per day
   geom_tile(aes(x = day_of_year_shifted, y = 95, fill = individuals_count), 
            width = 1.7, height = 3.2, color = NA) +
   scale_fill_gradient(low = "#C5C6FF", high = "#333553", 
                      guide = guide_colorbar(title = "Tagged sea turtles")) +
  
  # theme
  theme_bw() +
  theme(axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(),
        axis.text = element_text(size = 10),
        panel.grid = element_blank(),
        # legend
        legend.position = "none",
        # panel
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1)) +
  # labels
  labs(y = "Maximum depth (m)", x = "", color = "", fill = "")


pmax




# pvrm <- ggplot(stats, aes(x = day_of_year_shifted, y = mean_VMRd, color = daynight)) +
#   # Standarz deviation
#   geom_ribbon(aes(ymin = Lower_VMRd, ymax = Upper_VMRd, fill = daynight), alpha = 0.25, color = NA) +
# 
#   # Sombra gris para 'day' y 'night'
#   stat_smooth(data = subset(stats, daynight == "day"), aes(color = NULL),
#               method = "loess", se = FALSE, size = 1.5, color = "#61490e", span = 0.02) +
#   stat_smooth(data = subset(stats, daynight == "night"), aes(color = NULL),
#               method = "loess", se = FALSE, size = 1.5, color = "#003040", span = 0.02) +
# 
#   # smooth line
#   stat_smooth(method = "loess", se = FALSE, size = 1.1, span = 0.02) +
#   # Puntos o línea secundaria
#   geom_point(size = 0.5, alpha = 0.8) +
#   # scale
#   scale_x_continuous(breaks = (c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335) + 15),
#                      labels = month.abb, expand = c(0,0)) +
#   # scale_y_reverse() +
#   # colors
#   scale_fill_manual(values = c("night" = "skyblue3", "day" = "gold3")) +
#   scale_color_manual(values = c("night" = "skyblue4", "day" = "gold3")) +
#   theme_bw() +
#   theme(axis.title.y = element_text(size = 10),
#         axis.title.x = element_blank(),
#         axis.text = element_text(size = 10),
#         panel.grid = element_blank(),
#         # legend
#         legend.position = "none",
#         # panel
#         panel.border = element_rect(color = "black", fill = NA, linewidth = 1)) +
#   # labels
#   labs(y = expression(VMRd~"- Vertical Movement Rate dive (m min-1)"),
#        x = "D", color = "", fill = "")
# 
# 
# pvrm



# Plot para "day"
pvrm_day <- ggplot(subset(stats, daynight == "day"), aes(x = day_of_year_shifted, y = mean_VMRd, color = mean_VMRd)) +
  # SD
  geom_ribbon(aes(ymin = Lower_VMRd, ymax = Upper_VMRd, fill = daynight), alpha = 0.25, color = NA) +
  
  # gradiente line
  geom_line(size = 1.5) +
  
  # points
  geom_point(size = 0.5, alpha = 0.8) +
  
  # escala para eje X (meses centrados)
  scale_x_continuous(breaks = (c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335) + 15),
                     labels = month.abb, expand = c(0,0)) +
  # axi limit
  ylim(c(-1.5, 8.35)) +
  # Colores del gradiente (de naranja a rojo)
  scale_color_gradient(low = "gold", high = "#7D0112") +
  scale_fill_manual(values = c("night" = "skyblue3", "day" = "gold3")) +
  
  theme_bw() +
  theme(axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(),
        axis.text = element_text(size = 10),
        panel.grid = element_blank(),
        legend.position = "none",
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1)) +
  
  labs(y = expression(VMRd~"- Vertical Movement Rate dive (m min"^-1*")"), 
       x = "D", color = "", fill = "")

pvrm_day




# Plot para "night"
pvrm_night <- ggplot(subset(stats, daynight == "night"), aes(x = day_of_year_shifted, y = mean_VMRd, color = mean_VMRd)) +
  # Banda de confianza
  geom_ribbon(aes(ymin = Lower_VMRd, ymax = Upper_VMRd, fill = daynight), alpha = 0.25, color = NA) +
  
  # Línea con gradiente de color (de azul a gris)
  geom_line(size = 1.5) +
  
  # Puntos o línea secundaria
  geom_point(size = 0.5, alpha = 0.8) +
  
  # escala para eje X (meses centrados)
  scale_x_continuous(breaks = (c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335) + 15),
                     labels = month.abb, expand = c(0,0)) +
  ylim(c(-1.5, 8.35)) +
  # Colores del gradiente (de azul a gris)
  scale_color_gradient(low = "skyblue", high = "#091320") +
  scale_fill_manual(values = c("night" = "skyblue3", "day" = "gold3")) +
  
  theme_bw() +
  theme(axis.title.y = element_text(size = 10),
        axis.title.x = element_blank(),
        axis.text = element_text(size = 10),
        panel.grid = element_blank(),
        legend.position = "none",
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1)) +
  # label
  labs(y = expression(VMRd~"- Vertical Movement Rate dive (m min"^-1*")"), 
       x = "D", color = "", fill = "")


# combine
pvrm <- grid.arrange(pvrm_day, pvrm_night, nrow = 2)

# combine plots
# combine plots --------------------------------------------------
p <- grid.arrange(pmean, pmax, pvrm, nrow = 3)
p



# Export / save plots
output_fig <- paste0(output_dir,"/fig")

p_png <- paste0(output_fig,"/","fig_vertial_habitat_use_panel.png")
p_svg <- paste0(output_fig,"/","fig_vertial_habitat_use_panel.svg")
ggsave(p_png, p, width=22, height=24, units="cm", dpi=400, bg="white")
ggsave(p_svg, p, width=22, height=24, units="cm", dpi=400, bg="white")
































