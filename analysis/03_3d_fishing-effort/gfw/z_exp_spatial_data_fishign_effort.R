


# use same names of Global FIshing Watch data
gears <- c("TRAWLERS","DRIFTING_LONGLINES")

# check files names produce in previously script or daynight time description
daynight <- c("day","night")


g <- "TRAWLERS"
dt <- "night"


# day night - fishing event
file_name <- tolower(paste0(g, "_fishing_effort_", dt, ".tif"))    # lowerc
r2 <- terra::rast(paste0(input_dir,"/gfw/",file_name))

r_event <- app(c(r1, r2), fun = sum, na.rm = TRUE)
r_event[r_event > 0] <- 1 

# apparent fishing effort (API)
file_name <- tolower(paste0(g, "_fishing_effort.tif"))    # lowerc
r_api <- terra::rast(paste0(input_dir,"/gfw/",file_name))

# day night
# plot(r1)
# plot(r2)
# sum
# r <- app(c(r1, r2), fun = sum, na.rm = TRUE)
r_api[r_api == 0] <- NA 
r_api[r_api > 0] <- 1 

plot(r_api)
plot(r_event)


r_api <- resample(r_api, r_event, method = "near")

r_event <- aggregate(r_event, fact = 10, fun = max, na.rm = TRUE)
r_api <- aggregate(r_api, fact = 10, fun = max, na.rm = TRUE)

plot(r_event, col = "deepskyblue")
plot(r_api, col = "deepskyblue")

r_diff <- app(c(r_event, r_api), fun = sum, na.rm = TRUE)
plot(r_diff)


# and export summary plot -----------------
# extension of raster
e <- ext(r_diff)   #  also check that is WGS84 == rnaturalearth

# Make zoomed in map 
# land mask
world <- rnaturalearth::ne_countries(scale = "large", returnclass = "sf")

# as dataframe to plot in ggplot2
r_diff <- as.data.frame(r_diff, xy = TRUE)
names(r_diff)[3] <- "fishing_effort" # column name change

r_diff$fishing_effort <- factor(r_diff$fishing_effort, levels = c(1,2))


# Create a ggplot object
p <- ggplot() +
  # add raster
  # geom_raster(data = r, aes(x = x, y = y, fill = fishing_effort)) +
  
  # geom_raster(data = r_diff, aes(x = x, y = y), fill = "skyblue1") +
  # diff
  geom_raster(data = r_diff, aes(x = x, y = y, fill = fishing_effort)) +  # <- dentro de aes()
  scale_fill_manual(
    values = c("1" = "red",   # solo uno
               "2" = "deepskyblue4")           # ambos
  ) +
  
  # land mask
  # geom_sf(data = world, fill = "#3A4354", color = "#73829D") +
  # black tones
  geom_sf(data = world, fill = "grey20", color = 'grey25') +
  
  # Set spatial bounds or extention of map
  # extract and plot bounder of coordinates
  coord_sf(
    xlim = c(e[1], e[2]),  # xmin, xmax
    ylim = c(e[3], e[4]),  # ymin, ymax
    expand = FALSE
  ) +
  
  # scale_fill_gradientn(
  #   colours = option_color, 
  #   trans = "log10",
  #   guide = guide_colorbar(frame.colour = "grey5", ticks.colour = "grey5")
  # ) +
  # 
  

  # theme if we use custom ramp color
  theme_bw() +
  # Customize legend and axis titles
  labs(fill = "Fishing effort (Hours)",  # legend title
  ) +
  theme_bw() +
  theme(panel.background = element_rect(fill = 'grey10'), # change the color of sea in this case
        panel.border = element_rect(color = "grey5", fill = NA, linewidth = 1.1),
        panel.grid = element_blank(),
        # axis
        axis.title = element_blank(),
        axis.text.y = element_text(size = 11),
        axis.text.x = element_text(size = 11),
        axis.ticks = element_line(size = 0.75),
        axis.ticks.length = unit(6, "pt"),  # negative lenght -> ticks inside the plot
        # legend
        legend.title = element_text(size = 9, margin = margin(b = 8.5)),
        legend.text = element_text(size = 8.5),
  )

p




















