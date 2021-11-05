# trying to make a grid based on center of grid cells and area

cellsize = 0.5

ROMS_dat_hind_trim_sum <- ROMS_dat_hind_trim %>%
	group_by(latitude, long_not_360) %>%
	summarize(mean_temp = mean(temp),
						mean_area = mean(area_km2),
						mean_hatch_suc = mean(hatch_success_cauchy),
						mean_sp_hab = mean(sp_hab_suit))

ROMS_dat_hind_trim_sum_sf <- ROMS_dat_hind_trim_sum %>%
	  	st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326)


ROMS_geom <- ROMS_dat_hind_trim_sf$geometry

test_grid <- st_make_grid(
	st_as_sfc(
		st_bbox(ROMS_geom) + 
			c(-cellsize/2, -cellsize/2,
				cellsize/2, cellsize/2)),
	 what = "polygons", cellsize = cellsize)

ggplot() +
	geom_sf(data = ROMS_geom, color = "red") +
	geom_sf(data = test_grid, fill = "transparent")

plot(test_grid)
plot(ROMS_geom, add = TRUE)


test_grid_cut <- st_intersection(test_grid, full_poly)

ggplot() +
	#geom_sf(data = ROMS_geom, color = "red") +
	geom_sf(data = test_grid_cut, fill = "transparent")

### example



points <- data.frame(long = c(76.75,77.25,76.75,77.25,77.75),
                     lat = c(11.25,10.75,10.25,10.25,10.25)) %>% 
	st_as_sf(coords=c('long','lat'), crs=4326)

cellsize = 0.5
g2 = st_make_grid(
         st_as_sfc(
           st_bbox(points) + 
              c(-cellsize/2, -cellsize/2,
                 cellsize/2, cellsize/2)),
        what="polygons", cellsize=cellsize)

###
area_df <- area_df %>%
		mutate(long_not_360 = case_when(
				longitude >= 180 ~ longitude - 360,
				longitude < 180 ~ longitude))
	
area_df_sf <- area_df %>%
	  	st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326)

	
ggplot() +
	geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
	geom_sf(data = area_df_sf, aes(color = area_km2)) 

ggplot()+
	geom_sf(data = area_df_sf, aes(color = area_km2)) +
  geom_sf(data=newland, color="black", size=1) +
  geom_sf(data=russia_32635, color="black", size=1) +
  coord_sf(crs=3338)+
  xlim(c(xmin,xmax))+ylim(c(ymin,ymax))+
  theme_bw()

buffer_try <- st_buffer(ROMS_dat_hind_trim_sum_sf, ROMS_dat_hind_trim_sum_sf$mean_area)


ggplot()+
	geom_sf(data = buffer_try, aes(color = mean_area)) +
  geom_sf(data=newland, color="black", size=1) +
  geom_sf(data=russia_32635, color="black", size=1) +
  coord_sf(crs=3338)+
  xlim(c(xmin,xmax))+ylim(c(ymin,ymax))+
  theme_bw()