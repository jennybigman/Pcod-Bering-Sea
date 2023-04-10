	# map of Bering Sea showing locations

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
	b = getNOAA.bathy(lon1 = -179, lon2 = -158, lat1 = 52, lat2 = 66, 
                    resolution = 1)
	## Querying NOAA database ...
	## This may take seconds to minutes, depending on grid size
	## Building bathy matrix ...

	# convert bathymetry to data frame
	bf = fortify.bathy(b)
	
	# get regional polygons
	reg = map_data("world2Hires")
	reg = subset(reg, region %in% c('USSR', 'USA'))
	
	# convert lat longs
	reg$long = (360 - reg$long)*-1
	
	# set map limits
	lons = c(-179, -160)
	lats = c(52, 65)
	
	# make plot
	bering_sea_map <- 
		ggplot() +
  	geom_contour(data = bf, 
              	 aes(x=x, y=y, z=z),
              	 breaks=c(-50),
              	 size=c(0.2),
              	 colour="#bbddff")+
  	geom_contour(data = bf, 
  	             aes(x=x, y=y, z=z),
  	             breaks=c(-100),
  	             size=c(0.2),
  							 alpha = 0.7,
  	             colour="#1e90ff")+
  	geom_contour(data = bf, 
  	             aes(x=x, y=y, z=z),
  	             breaks=c(-180),
  	             size=c(0.2),
  							 alpha = 0.7,
  	             colour="#0c3966")+
  	geom_polygon(data = reg, aes(x = long, y = lat, group = group), 
  	            fill = "darkgrey", color = NA) + 
		coord_map(xlim = lons, ylim = lats) +
		scale_x_continuous(
			name = "Longitude",
			breaks = c(-180, -170, -160),
			labels = c("180", "-170", "-160")
		) +
		scale_y_continuous(
			name = "Latitude",
			breaks = c(55, 60, 65),
			labels = c("55", "60", "65")
		) +
  	theme_bw() +
  	theme(
			axis.text = element_text(size = 6, colour = "grey50"),
			axis.title = element_text(size = 8, color = "grey50"),
			panel.border = element_rect(fill = NA, color = "grey50"),
			axis.line = element_blank())
	


	Figure1 <- bering_sea_map +
		annotate("text", x = -171, y = 53.5, label = "Aleutian Islands", size = 2, angle = 20) +
		annotate("text", x = -168.5, y = 57.4, label = "Pribilof", size = 2) +
		annotate("text", x = -168.5, y = 57, label = "Islands", size = 2) +
		annotate("text", x = -176, y = 58, label = "Zhemchug", size = 2) +
		annotate("text", x = -176, y = 57.5, label = "Canyon", size = 2) +
		annotate("text", x = -163, y = 53, label = "Unimak", size = 2) +
		annotate("text", x = -163, y = 52.5, label = "Island", size = 2) +
  	annotate("text", x = -160, y = 61.5, label = "Alaska", size = 2) +
	geom_segment(aes(x = -164, y = 54.5, xend = -163, yend = 53.5),
  	size = 0.25,
    color = "gray20")

	
	 ggsave("./output/plots/bering_sea_map.tiff",
			 bering_sea_map_form, dpi = 500,
			 width = 4, height = 4, units = "in")
 
	  ggsave("./scripts/manuscript figure scripts/used in ms/pngs of figs/Figure1.tiff",
			 Figure1, dpi = 500,
			 width = 4, height = 4, units = "in")
	  
	  
