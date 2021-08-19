	df <- data_long_list[[1]]

	df_sf <- createSF_points(df)

test <- 	ggplot() +
 	geom_sf(data = df_sf, aes(color = val)) + 
	# facet_wrap(~ month_name, ncol = 4, nrow = 3) +
  	coord_sf(crs = crs_bering) +
  scale_x_continuous(
 		breaks = c(160, 170, 180, 190, 200, 210),
  	) +
 	scale_color_viridis_c() +
  	labs(colour = "Bottom temperature (˚C)") +
  	theme_bw() 
  												
ggsave(file = "./Pcod-Bering-Sea/output/plots/test2.png", 
       plot = test,
       height = 5, width = 5, units = "in")

beepr::beep("fanfare")


## above works but object is class of waiver? 

df2 <- df
df2$latitude <- round(df$latitude, 2)
df2$longitude <- round(df$longitude, 2)

pdf("testplot.pdf")
levelplot(val~longitude*latitude, data = df2) +
	latticeExtra::layer(sp.polygons(bPols,fill="gray30",lwd=0.5))
dev.off

