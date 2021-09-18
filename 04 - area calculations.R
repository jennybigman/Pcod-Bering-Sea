 # 04 - calculating area

	# start with one year

	dat1970_sf <- sm_temp_hind_df_sf %>% filter(year == 1970)

	# Convert points to sp (assumes that the sf object is called example_points)
	example_points <- as(example_points, "Spatial")
	
	dat1970_sf_sp <- as(dat1970_sf, "Spatial")

	# Generate empty raster layer and rasterize points
	 example_raster <- raster(crs = crs(example_points), vals = 0, resolution = c(0.5, 0.5), ext = extent(c(-180, 180, -90, 90))) %>%
   rasterize(example_points, .)
	 
	 dat1970_raster <- raster(dat1970_sf)
	 
	 dat1970_df <- as.data.frame(dat1970_raster, xy = TRUE)

	 ggplot() +
	 	geom_raster(data = dat1970_df, aes(x = x, y = y))	 
	 
	 dat_raster <- rasterize(dat1970_sf)
	 
	 
	 library(dplyr)
df <- read_sf(file) %>%
    mutate(name = as.factor(NAME)) %>%
    dplyr::select(SID74, SID79, name) %>%
    st_rasterize()

(file = system.file("gpkg/nc.gpkg", package="sf"))

df <- read_sf(file) %>% 
    st_geometry() %>%
    st_as_stars() %>%
    plot()

df <- dat1970_sf %>%
	dplyr::select(hatch_success_gaus, sp_hab_suit) %>% st_rasterize()


library(dplyr)
dfraster <- read_sf(file) %>%
    mutate(name = as.factor(NAME)) %>%
    dplyr::select(SID74, SID79, name) %>%
    st_rasterize()

rasterdf <- st_rasterize(dat1970_sf)

st_area(dat1970_sf)


