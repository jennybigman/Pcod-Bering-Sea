## bias corrected temps at the weekly level

	# read in bias corrected temps
  cesm_dfs_trim_wkgc <- fread("./data/cesm_dfs_trim_wkgc.csv")
  gfdl_dfs_trim_wkgc <- fread("./data/gfdl_dfs_trim_wkgc.csv")
  miroc_dfs_trim_wkgc <- fread("./data/miroc_dfs_trim_wkgc.csv")
  
  proj_df_list <- list(cesm_dfs_trim_wkgc, 
  										 gfdl_dfs_trim_wkgc,
  										 miroc_dfs_trim_wkgc)
 
	# summarize by grid cell
   sum_func <- function(df){
   	
   	df <- df %>%
   	group_by(latitude, longitude) %>%
   	summarize(bc_temp_sum = mean(bc_temp),
   						scal_factor_sum = mean(scaling_factor))
   	
   }
   
   proj_sum_dfs <- lapply(
   	list(cesm_dfs_trim_wkgc, gfdl_dfs_trim_wkgc, miroc_dfs_trim_wkgc),
   	sum_func)
  
  # convert to sf files
   
  sf_func <- function(df){
   
		df %>%
  		 mutate(long_not_360 = case_when(
						 longitude >= 180 ~ longitude - 360,
						 longitude < 180 ~ longitude)) %>% 
  	st_as_sf(coords = c("long_not_360", "latitude"), crs = 4326, remove = FALSE)
 
  }
  
  proj_sum_df_sf_list <- lapply(proj_sum_dfs, sf_func)
  
  # plot scaling factors
   
   plot_func <- function(df) {
   	
  	plot <- ggplot() +
			geom_sf(data = df, aes(color = scal_factor_sum))  +
			geom_sf(data = world_map_data, fill = "grey", lwd = 0) +
			coord_sf(crs = 3338) +
			scale_color_viridis_c() +
 			scale_x_continuous(
 				breaks = breaks_x,
 				labels = breaks_x,
 				name = "Longitude",
 				limits = limits_x
 			) +
 			scale_y_continuous(
 				breaks = breaks_y,
 				limits = limits_y,
 				name = "Latitude",
 			) +
  	 labs(colour = "scaling factor") +
			theme_bw() +
 			theme(
 				strip.text = element_text(size = 14, face = "bold"),
 				strip.background = element_blank(),
 				axis.text = element_text(size = 12),	
  			axis.title = element_text(size = 14),
  			legend.title.align = 0.5)
   
  	plot
   
   }
   
  
  scal_fac_plot_list  <- lapply(proj_sum_df_sf_list, plot_func)
  
	name_func <- function(x){
  	paste0(x, "_scaling_factor")
  }
   
	sims <- c("cesm", "gfdl", "miroc")
	
  names_sims <- sapply(sims, name_func)
  
	file_name_func <- function(x){
  	paste0("/Users/jenniferbigman/Dropbox/NOAA AFSC Postdoc/Pcod Project/Pcod Bering Sea Habitat Suitability/Pcod-Bering-Sea/output/plots/", x)
  }
   
  file_names <- sapply(names_sims, file_name_func)
	
	plot_list <- mapply(ggsave_func, x = scal_fac_plot_list, 
											y = file_names, w = 4, h = 4)


   
   
 