

# Title: Mapping GFW fishhing event by gear and daynight time

#-------------------------------------------------------------------------------
# 04. GFW day / night apparent fishing effort map
#-------------------------------------------------------------------------------

# Javier Menéndez-Blázquez | @jmenblaz

# Combien differenrt processed .tif into a sigle one day and night raster of 
# apparent fishing effort by fishing gear

# 13 years of fishing effort (10x10 km2)

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
    
    # ---------------------------------------------
    # resample to 10x10 km ----------------------------------------------
      rstack <- terra::project(rstack, "EPSG:3035")
      
      # reference raster 10x10 km 
      r_reference <- terra::rast(rstack, res = 10000) # meters
      
      # resample raster
      r <- terra::resample(rstack, r_reference, method = "bilinear")
      r <- mean(r)
      
      r[r <= 0] <- NA
          
    # plot(r) # -----------------------------------------
    
    # export resample fishing effort file
    file_name <- tolower(paste0(g, "_fishing_effort_10x10", dt, ".tif"))    # lowercase
    writeRaster(r, filename = paste0(input_dir,"/gfw/",file_name), overwrite = TRUE)
    
    # read again
    r <- terra::rast(paste0(input_dir,"/gfw/",file_name))
    
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
      map_fill_color <- "#11161D"
      map_line_color <- "grey10"
      background_color <- "#1e2632"
      # viridis colors
      option_color <- "F"
      # option_color <- c("#f0efed", '#D9D8C5', '#C9C8B1', '#FFA07A', '#CC6A4F', '#8B3626', '#4A1B13', '#060202')
      }

    
    if  (dt == "night"){ 
      map_fill_color <- "#11161D"
      map_line_color <- "grey10"
      background_color <- "#1e2632"
        # viridis colors
      option_color <- "F"
      # custom color
      # option_color <- c("#11161D","#14324A","#1C4E4D","#84B03C","#EAD94C","#fffee6")}
      # option_color <-c("#1e2632", "#2a4261", "#416192", "#5B8FC5", "#72B5E5", "#A0D9FF", "#e5f5ff") 
      }
      
    
  
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
      scale_fill_viridis_c(option = option_color, trans="log10",
                           # dark legend border or guide bar,
                           # legend scale fixed
                           # limits = c(1e-2, 1e+3),  # de 0.1 a 100
                           # breaks = c(1e-1, 1e+0, 1e+1, 1e+2),  # 0.1, 1, 10, 100, 1000
                           labels = scales::label_scientific(),
                           # legend theme
                           guide = guide_colorbar(frame.colour = "grey5",
                                                  ticks.colour = "grey5")) +
      
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
    png_file <- tolower(paste0(g, "_fishing_effort_summary_10x10_", dt, ".png"))
    p_png <- paste0(input_dir,"/gfw/", png_file)
    
    ggsave(p_png, p, width=23, height=11, units="cm", dpi=350)
    # ggsave(p_svg, p, width=23, height=18, units="cm", dpi=350)
    
  }
}

