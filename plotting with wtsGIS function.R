	df <- data_long_list[[1]]

	df_sf <- createSF_points(df)

test <- 	ggplot() +
 	geom_sf(data = df_sf, aes(color = val)) + 
  	#coord_sf(crs = crs_bering) +
  scale_x_continuous(
 		breaks = c(160, 170, 180, 190, 200, 210),
  	) +
 	scale_color_viridis_c() +
  	labs(colour = "Bottom temperature (˚C)") +
  	theme_bw() 
  												
ggsave(path = "./Pcod-Bering-Sea/output/plots/", filename = "test2.png", 
       plot = test,
       height = 5, width = 5, units = "in")