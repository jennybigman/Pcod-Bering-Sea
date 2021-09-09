bsregions.df <- bsregions.df %>% 
	mutate(longitude = long,
				 latitude = lat)

bsregions.df_sf <- createSF_points(bsregions.df)
bsregions.df_sf <- cbind(bsregions.df_sf, st_coordinates(st_centroid(bsregions.df_sf)))

# make plot
NBSRegion_Map_New <- ggplot() +
  # add coastline
  geom_polygon(data = reg, aes(x = long, y = lat, group = group), 
               fill = "darkgrey", color = NA) + 
  # add polygon
  geom_polygon(data = bsregions.df, aes(long, lat, group=group), 
  						 linetype = "solid", colour = "black", alpha = 0) +
  # configure projection and plot domain
  coord_map(xlim = lons, ylim = lats) +
	geom_text(data = bsregions.df_sf, aes(x = longitude, y = latitude, label = DOMAIN)) +
  # formatting
  theme_bw()+
  xlab("Longitude")+
  ylab("Latitude")+
  theme(axis.text.x=element_text(size=10, color = "black"), 
  			axis.text.y = element_text(size=10, color = "black"))
  
ggsave(file = "./Pcod-Bering-Sea/output/plots/Dave_BS_Regions.png", NBSRegion_Map_New,
       height = 10, width = 10, units = "in")


