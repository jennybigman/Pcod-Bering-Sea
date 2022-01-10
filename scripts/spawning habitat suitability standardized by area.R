	# standardized by area ####
	
	reg_df_sum <- reg_df %>% 
		st_drop_geometry() %>%
		mutate(latitude = lats) %>%
		group_by(region, latitude, longitude) %>%
		summarise(mean_area = mean(area_km2))
	
	area_sum <- reg_df_sum %>%
		group_by(region) %>%
		summarize(total_area = sum(mean_area))
	
	inner_area <- as.numeric(area_sum[1,2])
	mid_area <- as.numeric(area_sum[2,2])
	out_area <- as.numeric(area_sum[3,2])
	
	yr_hab_dat_std <- yr_hab_dat %>%
		mutate(std_sphabsuit = case_when(
			region == "inner" ~ annual_spawning_hab_suit/inner_area,
			region == "middle" ~ annual_spawning_hab_suit/mid_area,
			region == "outer" ~ annual_spawning_hab_suit/out_area
		))
					 
	# plot
	
		yrly_std_sphab_region_plot <-    
   	ggplot(data = yr_hab_dat_std) +
	 	geom_line(aes(year, std_sphabsuit, group = region, colour = region),
            data = . %>% filter(region == "inner"), color = "red") +
	  geom_line(aes(year, std_sphabsuit, group = region, colour = region),
            data = . %>% filter(region == "middle"), color = "blue") +
	 	geom_line(aes(year, std_sphabsuit, group = region, colour = region),
            data = . %>% filter(region == "outer"), color = "black") +
   	xlab("Year") + 
	  scale_y_continuous(
	  	name = "Spawning habitat suitability/area",
	  	breaks = c(0, 0.0000025, 0.000005, 0.0000075, 0.00001),
	  	labels = c("0", "0.0000025", "0.000005", "0.0000075", "0.00001")
	  ) +
   	xlim(1970, 2035) +
   	theme_bw() +
  	theme(legend.position = "none") +
  	theme(
  	  axis.text.x =element_text(size=18, colour = "grey50"),
  	  axis.text.y =element_text(size=10, colour = "grey50"),
  	  axis.ticks = element_line(colour = "grey50"),
  	  axis.line = element_line(colour = "grey50"),
  	  axis.title= element_text(size=20, color = "grey30"),
  	  panel.grid.major = element_blank(),
  	  panel.grid.minor = element_blank(),
  	  panel.border = element_rect(fill = NA, color = "grey50"))
   
   yrly_std_sphab_region_plot_text <- yrly_std_sphab_region_plot +
		annotate(geom = "text", x = 2028, y = 0.0000075,
           label = "outer shelf",
           color = "#000000", size = 6) +
		annotate(geom = "text", x = 2028, y = 0.0000012,
           label = "middle shelf",
           color = "blue", size = 6) +
		annotate(geom = "text", x = 2027.5, y = 0.0000007,
           label = "inner shelf",
           color = "red", size = 6)
 
	ggsave("./output/plots/yrly_std_sphab_region_plot.png",
			 yrly_std_sphab_region_plot_text,
			 width = 10, height = 7, units = "in")

	
		proj_yr_hab_dat_scens <- proj_yr_hab_dat %>%
		filter(projection != "historical")
	
	proj_yr_hab_dat_scens <- tidyr::unite(proj_yr_hab_dat_scens,"sim_proj",
																			 simulation, projection, remove = F)

	colors <- c("#efd966", "#b79a00", 
						  "#7fb27f", "#004700", 
						  "#6666b2", "#000059")

	sim_proj <- unique(proj_yr_hab_dat_scens$sim_proj)
	
	names(colors) <- unique(proj_yr_hab_dat_scens$sim_proj)
	
	
	proj_yr_hab_dat_scens <-    
   	ggplot(data = proj_yr_hab_dat_scens) +
	 	geom_line(aes(year, annual_spawning_hab_suit, group = sim_proj, colour = sim_proj))  +
		facet_grid(simulation ~ region) +
   	xlab("Year") + 
		scale_color_manual(name = "sim_proj", values = colors) +
	  scale_y_continuous(
	  	name = "Spawning habitat suitability",
	  	breaks = c(0, 0.20, 0.40, 0.60, 0.8),
	  ) +
   	theme_bw() +
  	theme(legend.position = "none") +
  	theme(
  		strip.background = element_blank(),
  		strip.text = element_text(size = 16, face = "bold"),
  	  axis.text=element_text(size=16, colour = "grey50"),
  	  axis.ticks = element_line(colour = "grey50"),
  	  axis.line = element_line(colour = "grey50"),
  	  axis.text.x = element_text(size = 16),
  	  axis.title= element_text(size=16, color = "grey30"),
  	  panel.grid.major = element_blank(),
  	  panel.grid.minor = element_blank(),
  	  panel.border = element_rect(fill = NA, color = "grey50"))
   
	ggsave("./output/plots/proj_yr_hab_dat_scens.png",
			 proj_yr_hab_dat_scens,
			 width = 10, height = 7, units = "in")