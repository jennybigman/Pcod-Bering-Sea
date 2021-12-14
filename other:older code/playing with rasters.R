
	
	my_rast <- rast(xmin = (min(ROMS_hindcast_dat$longitude)), xmax = (max(ROMS_hindcast_dat$longitude)),
									ymin = (min(ROMS_hindcast_dat$latitude)), ymax = (max(ROMS_hindcast_dat$latitude)),
									vals = ROMS_hindcast_dat$temp)

plot(my_rast)
