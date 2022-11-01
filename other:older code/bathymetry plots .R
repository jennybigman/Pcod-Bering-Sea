# bathmetry to ggplots 
	library(marmap)
	
	# other packages needed
	ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
	}

	packages <- c("tidyr", "rgdal", "dplyr", "ggplot2", "ggmap", "lattice", "stringr", "MASS",  "PerformanceAnalytics", "pvclust", "visreg", "Hmisc", "tibble", "ade4", "vegan", "ellipse", "FactoMineR","leaps", "readxl", "sf","maps", "mapdata", "sp", "gstat", "mapplots", "raster", "leaflet", "mapview", "splancs", "marmap")
	ipak(packages) 

	# load bs regions
	bsregions <- readOGR("./other:older code/Mapping Code - Bigman/Ortiz Regions", layer="BSIERP_regions_2012")

	# get bathymetry data
	b = getNOAA.bathy(lon1 = -178, lon2 = -158, lat1 = 56, lat2 = 67, 
                    resolution = 1)
	## Querying NOAA database ...
	## This may take seconds to minutes, depending on grid size
	## Building bathy matrix ...

	# convert bathymetry to data frame
	bf = fortify.bathy(b)

	contour_plot <- ggplot() +
  geom_contour(data = bf, 
               aes(x=x, y=y, z=z),
              # breaks=c(-50), # add 50m contour
               size=c(0.3),
               colour="grey")+
  geom_contour(data = bf, 
               aes(x=x, y=y, z=z),
               breaks=c(-100),  # add 100m contour
               size=c(0.3),
               colour="grey")+
  geom_contour(data = bf, 
               aes(x=x, y=y, z=z),
               breaks=c(-200),   # add 200m contour
               size=c(0.3),
               colour="grey")

	
	######
	bf2 <- bf %>% filter(., z == -200)

	contour_plot <- ggplot() +
  geom_contour(data = bf2, 
               aes(x=x, y=y, z=z),
               size=c(0.3),
               colour="grey")

	# does geom_sf and geom_contour talk? 
	bf3 <- bf %>% filter(., z == -200) %>%
		rename(latitude = y,
					 long_not_360 = x,
					 depth = z) %>%
  	st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326, remove = FALSE)
	
	bf_line <- bf3 %>%
		st_cast("LINESTRING") %>%
		st_cast("MULTILINESTRING")

	test_plot <- 
    	  	ggplot() +
					#geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
					geom_sf(data = bf_line, aes(color = depth)) +
					coord_sf(crs = 3338) +
					scale_color_viridis_c() +
 					scale_x_continuous(
 						breaks = c(-170, -160),
 						labels = c("-170˚", "-160˚"),
 						name = "Longitude",
 						limits = c(-1400000, -150000)
 					) +
 					scale_y_continuous(
 						breaks = c(55, 60),
 						limits = c(470000, 1900000),
 						name = "Latitude",
 					) +
    	  	labs(colour = "Spawning\nhabitat\nsuitability") +
					theme_bw() +
 					theme(
 						panel.border = element_rect(color = "#666666"),
 						legend.position = "none",
 						axis.text = element_text(size = 8,  color = "#666666"),	
  					axis.title = element_text(size = 10,  color = "#666666"),
 						axis.ticks = element_line(color = "#666666"))
    	  
	test_plot + geom_sf(data = bf_sf, aes(color = depth))
