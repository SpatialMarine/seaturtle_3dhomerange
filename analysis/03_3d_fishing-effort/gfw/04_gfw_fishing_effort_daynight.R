

# Title: Mapping GFW fishhing event by gear and daynight time

#-------------------------------------------------------------------------------
# 04. GFW day / night apparent fishing effort map
#-------------------------------------------------------------------------------

# Javier Menéndez-Blázquez | @jmenblaz

# Combien differenrt processed .tif into a sigle one day and night raster of 
# apparent fishing effort by fishing gear

# 13 years of fishing effort

# ------------------------------------------------------------------------------

library(sf)
library(ggplot2)
library(terra)
library(paletteer)

# check dir
input_dir

# use same names of Global FIshing Watch data
gears <- c("TRAWLERS","DRIFTING_LONGLINES")

# check files names produce in previously script or daynight time description
daynight <- c("day","night")

# 1) list filter of interest
# by daynight time
for (dt in daynight){
  
  # and by fishing gear
  for (g in gears) {
    
    # list all .tif files of this specific time and gear
    files <- list.files(paste0(input_dir,"/gfw/daynight"), pattern = paste0(dt,"_",g,".tif"), full.names = TRUE)
    # read rasters as raster stack and sum
    rstack <- terra::rast(files)
    # check 
    # plot(rstack)
    r <- mean(rstack)
    
    # values 0 as NA
    r[r <= 0] <- NA
    
    # plot(r)
    
    # export
    file_name <- tolower(paste0(g, "_fishing_effort_", dt, ".tif"))    # lowercase
    writeRaster(r, filename = paste0(input_dir,"/gfw/",file_name), overwrite = TRUE)
    
    
    # and export summary plot -----------------
    # extension of raster
    e <- ext(r)   #  also check that is WGS84 == rnaturalearth
    
    # Make zoomed in map 
    # land mask
    world <- rnaturalearth::ne_countries(scale = "large", returnclass = "sf")
    
    # as dataframe to plot in ggplot2
    r <- as.data.frame(r, xy = TRUE)
    names(r)[3] <- "fishing_effort" # column name change
    
    

    
    # day night color pallets
    if (dt == "day"){ 
      map_fill_color <- "#e4e3e1"
      map_line_color <- "grey80"
      background_color <- "#70889b"
      # viridis colors
      option_color <- c('#FFEC8B','#FFA07A','#8B3626','#130705') }
    
    if  (dt == "night"){ 
      map_fill_color <- "#11161D"
      map_line_color <- "grey10"
      background_color <- "#1e2632"
        # viridis colors
      option_color <- c('#0c0f13', '#8B3626', '#FFA07A', '#FFEC8B') }

  
      #################### CAMBIAR PALETA COLORES DIA Y NOCHE
      
  
    # Create a ggplot object
    p <- ggplot() +
      # land mask
      # geom_sf(data = world, fill = "#3A4354", color = "#73829D") +
      # black tones
      geom_sf(data = world, fill = map_fill_color, color = map_line_color) +
      
      # add raster
      geom_raster(data = r, aes(x = x, y = y, fill = fishing_effort)) +
      
      # Set spatial bounds or extention of map
      # extract and plot bounder of coordinates
      coord_sf(
        xlim = c(e[1], e[2]),  # xmin, xmax
        ylim = c(e[3], e[4]),  # ymin, ymax
        expand = FALSE
      ) +
      
      
      # ramp color (virids)
      # scale_fill_gradientn(colors = color_ramp, trans = "log10", 
      #                      name = "Fishing effort (Hours)") +
      # scale_fill_viridis_c(option = option_color, trans="log10",
      #                      # dark legend border or guidebar
      #                      guide = guide_colorbar(frame.colour = "grey5", 
      #                                             ticks.colour = "grey5")) +
      
      scale_fill_gradientn(
        colours = option_color, 
        trans = "log10",
        guide = guide_colorbar(frame.colour = "grey5", ticks.colour = "grey5")
      ) +
    
      
      # theme
      theme_bw() +
      # Customize legend and axis titles
      labs(fill = "Fishing effort (Hours)",  # legend title
      ) +
      theme_bw() +
      theme(panel.background = element_rect(fill = background_color), # change the color of sea in this case
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
    
    # export plot as png:
    png_file <- tolower(paste0(g, "_fishing_effort_summary_", dt, ".png"))
    p_png <- paste0(input_dir,"/gfw/", png_file)
    
    ggsave(p_png, p, width=23, height=11, units="cm", dpi=350)
    # ggsave(p_svg, p, width=23, height=18, units="cm", dpi=350)
    
    

    
    

  
  }
}

