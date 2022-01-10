# calculating the mean latitude of spawning habitat suitability

	#### year ####
	
	#### hindcasts ####
	
	hind_mean_lat_yr <- function(x){
		
		new_dat <- ROMS_hindcast_dat %>%
			filter(., sp_hab_suit >= x)
		
		new_dat_sum <- new_dat %>%
			group_by(year) %>%
			summarise(hist_mean_lat = mean(latitude)) 

		new_dat_sum		
	}
	
	sp_hab_thresholds <- c(0.5, 0.9)
	
	hind_mean_lats_yr <- lapply(sp_hab_thresholds, hind_mean_lat_yr)
	
	hind_mean_lats_yr_0.5 <- hind_mean_lats_yr[[1]] %>%
		mutate(sp_hab_threshold = 0.5)

	hind_mean_lats_yr_0.9 <- hind_mean_lats_yr[[2]]	%>%
		mutate(sp_hab_threshold = 0.9)
	
	hind_mean_lats_yr_df <- bind_rows(hind_mean_lats_yr_0.5, hind_mean_lats_yr_0.9) 
	
	
	#### projections ####
	
	# bias-corrected temp with variance ratio
		
	proj_mean_lat_yr_var <- function(x){
		
		new_dat <- ROMS_projected_dat %>%
			filter(., sp_hab_suit_var >= x)
		
		new_dat_sum <- new_dat %>%
			group_by(simulation, projection, year) %>%
			summarise(proj_mean_lat = mean(Lat)) 

		new_dat_sum		
	}
	
	sp_hab_thresholds <- c(0.5, 0.9)
	
	proj_mean_lat_yr_var <- lapply(sp_hab_thresholds, proj_mean_lat_yr_var)
	
	proj_mean_lats_var_yr_0.5 <- proj_mean_lat_yr_var[[1]] %>%
		mutate(sp_hab_threshold = 0.5)

	proj_mean_lats_var_yr_0.9 <- proj_mean_lat_yr_var[[2]]	%>%
		mutate(sp_hab_threshold = 0.9)
	
	proj_mean_lats_yr_var_df <- bind_rows(proj_mean_lats_var_yr_0.5, proj_mean_lats_var_yr_0.9) 
	
	proj_mean_lats_yr_var_df <- tidyr::unite(proj_mean_lats_yr_var_df,"sim_proj",
																			 simulation, projection, remove = F)

	proj_mean_lats_yr_var_df_plot <- proj_mean_lats_yr_var_df %>%
		filter(!str_detect(sim_proj, "_historical"))
	
	hist_data <- proj_mean_lats_yr_var_df %>%
		filter(str_detect(sim_proj, "_historical"))

	
	colors <- c("#efd966", "#b79a00", 
						  "#7fb27f", "#004700", 
						  "#6666b2", "#000059")

	sim_proj <- unique(proj_mean_lats_yr_var_df_plot$sim_proj)
	
	names(colors) <- unique(proj_mean_lats_yr_var_df_plot$sim_proj)
	
	
	mean_latitude_plot_var <-    
   	ggplot(data = hind_mean_lats_yr_df) +
	 	geom_line(aes(year, hist_mean_lat, alpha = 0.5),
            data = . %>% filter(sp_hab_threshold == 0.5), color = "black") +
		geom_line(data = proj_mean_lats_yr_var_df_plot, 
							aes(year, proj_mean_lat, color = sim_proj, group = sim_proj, alpha = 0.5)) +
		facet_grid(sp_hab_threshold ~ simulation) +
	  geom_line(aes(year, hist_mean_lat, colour = sp_hab_threshold, alpha = 0.5),
            data = . %>% filter(sp_hab_threshold == 0.9), color = "black") +
		geom_line(data = hist_data, 
							aes(year, proj_mean_lat, color = "lightgrey", alpha = 0.5)) +
		xlab("Year") +
		scale_color_manual(name = "sim_proj", values = colors) +
	  scale_y_continuous(
	  	name = "Meanlatitude\nvar",
	  	breaks = c(56, 58, 60, 62),
	  	labels = c("56˚N", "58˚N", "60˚N", "62˚N")
	  ) +
   	xlim(1970, 2100) +
    theme_bw() +
  	theme(legend.position = "none") +
  	theme(
			strip.background = element_blank(),
  		strip.text = element_text(size = 18, face = "bold"),
			axis.text = element_text(size = 16, colour = "grey50"),
  	  axis.ticks = element_line(colour = "grey50"),
  	  axis.line = element_line(colour = "grey50"),
  	  axis.title = element_text(size=18, color = "grey30"),
  	  panel.grid.major = element_blank(),
  	  panel.grid.minor = element_blank(),
  	  panel.border = element_rect(fill = NA, color = "grey50"))
	
	
		ggsave("./output/plots/mean_latitude_var_plot.png",
			 mean_latitude_plot_var,
			 width = 10, height = 5, units = "in")
	
	
	#### month ####

	# hindcasts 
		
	mean_lat_mo_hind <- function(x){
		
		new_dat <- ROMS_hindcast_dat %>%
			filter(., sp_hab_suit >= x)
		
		new_dat_sum <- new_dat %>%
			group_by(year, month_name, month) %>%
			summarise(hist_mean_lat = mean(latitude)) 

		new_dat_sum		
	}
	
	sp_hab_thresholds <- c(0.5, 0.9)
	
	mean_lats_mo_hind <- lapply(sp_hab_thresholds, mean_lat_mo_hind)
	
	mean_lats_mo_hind_0.5 <- mean_lats_mo_hind[[1]] %>%
		mutate(sp_hab_threshold = 0.5)
	
	mean_lats_mo_hind_0.9 <- mean_lats_mo_hind[[2]]	%>%
		mutate(sp_hab_threshold = 0.9)
	
	mean_lats_mo_hind_df <- bind_rows(mean_lats_mo_hind_0.5, mean_lats_mo_hind_0.9)
	
	# reorder for plotting
	
	mean_lats_mo_hind_df$month_name <- factor(mean_lats_mo_hind_df$month_name)
  mean_lats_mo_hind_df$month_name <- fct_reorder(mean_lats_mo_hind_df$month_name, 
  																					mean_lats_mo_hind_df$month)
  
  # projections
  
	# bias-corrected temp with variance ratio
  		
  	mean_lat_mo_proj_var <- function(x){
		
		new_dat <- ROMS_projected_dat %>%
			filter(., sp_hab_suit_var >= x)
		
		new_dat_sum <- new_dat %>%
			group_by(simulation, projection, year, month_name, month) %>%
			summarise(proj_mean_lat = mean(latitude)) 

		new_dat_sum		
	}
	
	sp_hab_thresholds <- c(0.5, 0.9)
	
	mean_lat_mo_proj_var <- lapply(sp_hab_thresholds, mean_lat_mo_proj_var)
	
	mean_lat_mo_proj_var_0.5 <- mean_lat_mo_proj_var[[1]] %>%
		mutate(sp_hab_threshold = 0.5)
	
	mean_lat_mo_proj_var_0.9 <- mean_lat_mo_proj_var[[2]]	%>%
		mutate(sp_hab_threshold = 0.9)
	
	mean_lat_mo_proj_var_df <- bind_rows(mean_lat_mo_proj_var_0.5, mean_lat_mo_proj_var_0.9) 
	
	# reorder for plotting
	
	mean_lat_mo_proj_var_df$month_name <- factor(mean_lat_mo_proj_var_df$month_name)
  mean_lat_mo_proj_var_df$month_name <- fct_reorder(mean_lat_mo_proj_var_df$month_name, 
  																					mean_lat_mo_proj_var_df$month)
 
	# plot
  

	mean_lat_mo_proj_var_df <- tidyr::unite(mean_lat_mo_proj_var_df,"sim_proj",
																			 simulation, projection, remove = F)

	mean_lat_mo_proj_var_df_plot <- mean_lat_mo_proj_var_df %>%
		filter(!str_detect(sim_proj, "_historical"))
	
	hist_data <- mean_lat_mo_proj_var_df %>%
		filter(str_detect(sim_proj, "_historical"))
	
	colors <- c("#efd966", "#b79a00", 
						  "#7fb27f", "#004700", 
						  "#6666b2", "#000059")

	sim_proj <- unique(mean_lat_mo_proj_var_df$sim_proj)
	
	names(colors) <- unique(mean_lat_mo_proj_var_df_plot$sim_proj)
	
	mean_lat_mo_plot_var <- 
		ggplot(mean_lats_mo_hind_df) +
	 	geom_line(aes(year, hist_mean_lat, alpha = 0.5),
		  data = . %>% filter(sp_hab_threshold == 0.5), color = "black") +
		geom_line(data = mean_lat_mo_proj_var_df, 
							aes(year, proj_mean_lat, color = sim_proj, group = sim_proj, alpha = 0.5)) +
		facet_grid(month_name ~ simulation + sp_hab_threshold) +
		geom_line(aes(year, hist_mean_lat, alpha = 0.5),
		  data = . %>% filter(sp_hab_threshold == 0.9), color = "black") +
		geom_line(data = hist_data, 
							aes(year, proj_mean_lat, color = "lightgrey", alpha = 0.5)) +
		xlab("Year") +
		scale_color_manual(name = "sim_proj", values = colors) +
	  scale_y_continuous(
	  	name = "Mean latitude\nvar",
	  	breaks = c(56, 58, 60, 62),
	  	labels = c("56˚N", "58˚N", "60˚N", "62˚N"),
	  	limits = c(55, 63)
	  ) +
   	xlim(1970, 2100) +
		theme_bw() +
  	theme(legend.position = "none") +
  	theme(
			strip.background = element_blank(),
  		strip.text = element_text(size = 18, face = "bold"),
			axis.text = element_text(size = 16, colour = "grey50"),
  	  axis.ticks = element_line(colour = "grey50"),
  	  axis.line = element_line(colour = "grey50"),
  	  axis.title = element_text(size=18, color = "grey30"),
  	  panel.grid.major = element_blank(),
  	  panel.grid.minor = element_blank(),
  	  panel.border = element_rect(fill = NA, color = "grey50"))
	
  		ggsave("./output/plots/mean_lat_mo_plot_var.png",
			 mean_lat_mo_plot_var,
			 width = 15, height = 7, units = "in")
  		
  		
  		
  	# try fixing labels
  		
  facet_labeller_top <- function(variable, value) {
  c(
    "", 
    "",
    "",
    "",
    "",
    ""
  )
}

facet_labeller_bottom <- function(variable, value) {
  c(
    "0.5", 
    "0.9",
    "0.5",
    "0.9",
    "0.5", 
    "0.9"
  )
}
  		
  mean_lat_mo_plot <- 
		ggplot(mean_lats_mo_hind_df) +
	 	geom_line(aes(year, hist_mean_lat, alpha = 0.5),
		  data = . %>% filter(sp_hab_threshold == 0.5), color = "black") +
		geom_line(data = mean_lats_mo_proj_df_plot, 
							aes(year, proj_mean_lat, color = sim_proj, group = sim_proj, alpha = 0.5)) +
		facet_grid(month_name ~ simulation + sp_hab_threshold,
							 labeller = labeller(
							 	simulation=as_labeller(facet_labeller_top),
                sp_hab_threshold = as_labeller(facet_labeller_bottom))) +
		geom_line(aes(year, hist_mean_lat, alpha = 0.5),
		  data = . %>% filter(sp_hab_threshold == 0.9), color = "black") +
		geom_line(data = hist_data, 
							aes(year, proj_mean_lat, color = "lightgrey", alpha = 0.5)) +
		xlab("Year") +
		scale_color_manual(name = "sim_proj", values = colors) +
	  scale_y_continuous(
	  	name = "Mean\nlatitude",
	  	breaks = c(56, 58, 60, 62),
	  	labels = c("56˚N", "58˚N", "60˚N", "62˚N")
	  ) +
   	xlim(1970, 2100) +
		theme_bw() +
  	theme(legend.position = "none") +
  	theme(
  		plot.title = element_text(size = 18, face = "bold"),
			strip.background = element_blank(),
  		strip.text = element_text(size = 18, face = "bold"),
			axis.text = element_text(size = 16, colour = "grey50"),
  	  axis.ticks = element_line(colour = "grey50"),
  	  axis.line = element_line(colour = "grey50"),
  	  axis.title = element_text(size=18, color = "grey30"),
  	  panel.grid.major = element_blank(),
  	  panel.grid.minor = element_blank(),
  	  panel.border = element_rect(fill = NA, color = "grey50"))
	
  
 mean_lat_mo_plot2 <- 
 	mean_lats_mo_proj_df %>% 
  ggplot(aes(year, proj_mean_lat)) + 
  geom_blank() + 
  facet_grid(~ simulation, scales = "free_x") +
  theme(panel.spacing.x = unit(0,"cm"),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.margin = margin(b = -2),
				strip.background = element_blank(),
  			strip.text = element_text(size = 18, face = "bold"),
				panel.background = element_rect(colour = "white", fill = "white"), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
 
 
 
 plot1 <- mean_lat_mo_plot2 + theme(plot.margin = unit(c(-10, -10, -10, -10), "in"))
 plot2 <- mean_lat_mo_plot + theme(plot.margin = unit(c(0, 0.2, 0.2, 0.2), "in"))
 
	mean_lat_mo_plot_form <- plot1/plot2 + plot_layout(heights = c(0.1,100) ) 
 
 
 
  		ggsave("./output/plots/mean_lat_mo_plot_form.png",
			 mean_lat_mo_plot_form,
			 width = 15, height = 7, units = "in")
  		
  		
  		
  		

	