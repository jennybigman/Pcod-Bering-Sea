# 02 - plotting weekly-averaged bottom temps and hatch success

	#### load and transform data ####

	
	  setwd("~/Google Drive/NOAA AFSC Postdoc/Pcod Bering Sea Habitat Suitability/ACLIM2-main")
    main   <- getwd()  #"~/GitHub_new/ACLIM2
    source("R/make.R")
    source("R/sub_scripts/load_maps.R") 

    sim        <- "B10K-K20_CORECFS" 

		ID_func <- function(x){
    	IDin <- paste0(x, "_subgrid")
    	paste0("_", IDin)
    }
    
    years <- seq(1970, 1970, by = 1) 

    IDs <- sapply(years, ID_func)
    
    # for file nmae
    var_use    <- "_bottom5m_temp"

    # file paths for ROMS data
		fl_list <- function(x){
    	file.path(main,Rdata_path,sim,"Level2", paste0(sim,var_use,x,".Rdata"))
    }
    
    fl_paths <- lapply(IDs, fl_list)
    
	# function to read in file path and transform data

    data_transform <- function(x){ #### this function takes forever if running it over all years at once ####
    
  			load(x)
    	
  		# format data into a tidy dataframe
    		i <-1
    		data_long <- data.frame(latitude = as.vector(temp$lat),
    		                   longitude = as.vector(temp$lon),
    		                   val = as.vector(temp$val[,,i]),
    		                   time = temp$time[i],
    		                   year = substr( temp$time[i],1,4),stringsAsFactors = F
    		                   )
    		
    		for(i in 2:dim(temp$val)[3])
    		  data_long <- rbind(data_long,
    		                      data.frame(latitude = as.vector(temp$lat),
    		                       longitude = as.vector(temp$lon),
    		                       val = as.vector(temp$val[,,i]),
    		                       time = temp$time[i],
    		                       year = substr( temp$time[i],1,4),stringsAsFactors = F)
    		  )
    
    		 
  			 # calculate hatch success for both cauchy and gaussian distributions
  			 hatch_success_cauchy_func <- function(x, k = 0.453, mu = 4.192, sigma = 2.125 ){
         
  			 		(k / (1 + (((x - mu)/sigma))^2)) 
         }
    
    
         hatch_success_gaus_func <- function(x, k = 0.395, mu = 4.50, sigma = 2.58){
    	  
        	k * exp(-1/2 * (x - mu)^2/sigma^2)
         } 
    
         data_long <- data_long %>%
    								  mutate(hatch_success_cauchy = sapply(val, hatch_success_cauchy_func)) %>%
    								  mutate(hatch_success_gaus = sapply(val, hatch_success_gaus_func))
    	  
         # omit NAs for plotting
         data_long <- na.omit(data_long)
         
         data_long <- data_long %>% distinct(across(everything()))
         
         data_long
         
    }
    
        data_long_list <- lapply(fl_paths, data_transform)

    
	 	     # convert to shapefile
        sf_func <- function(x){ 
        
        
           df_sf <- convert2shp(x)


    		}

    
    # create list of shapefiles for each year
    sf_file_list <- lapply(data_long_list, sf_func)
    
    #### function to plot -- bottom temp ####
    
    # facet by month 
    
    bottom_temp_monthly_plot_func <- function(x){
    	
    	  plot <- ggplot() +
  						 geom_sf(data = df1, aes(color = val)) +
  						 geom_sf(data=st_transform(bering_sf,crs=crs_bering),fill="lightgrey", 
  										 color="black",lwd=0.25) +
  						 facet_wrap(~ month_name, ncol = 4, nrow = 3) +
               xlab("Longitude") + ylab("Latitude") +
  						 coord_sf(crs = crs_bering) +
  					   scale_x_continuous(
   						 breaks = c(170, 180, -170)) +
  						 scale_color_viridis_c() +
   						 labs(colour = "˚C") +
   						 theme_bw() +
  						 theme(legend.title = element_text(size = 10))
    	 
    	 plot
            
    }
    
        bottom_temp_monthly_plot_list <- sapply(sf_file_list, bottom_temp_monthly_plot_func)

        # save plots
        setwd("~/Google Drive/NOAA AFSC Postdoc/Pcod Bering Sea Habitat Suitability/Pcod-Bering-Sea/output/plots/monthly plots")
       
        years <- c(1970:1970)
        
        mo_name_func <- function(x){
        	year_month <- paste0(x, "_bottom_temp_monthly")
        }
        
        month_names <- sapply(years, mo_name_func)

				for(i in 1:length(bottom_temp_monthly_plot_list)) {
  				for (j in month_names) {
  						ggsave(plot = bottom_temp_monthly_plot_list[[i]],
        			file = paste(j,".png",sep=""),
        			width = 10, height = 10, units = "in")
        }}

     
        # facet by week -- bottom temp
        
        bottom_temp_weekly_plot_func <- function(x){
         
        	 plot <- ggplot() +
  						 geom_sf(data = x, aes(color = val)) +
  						 geom_sf(data=st_transform(bering_sf,crs=crs_bering),fill="lightgrey", 
  										 color="black",lwd=0.25) +
  						 facet_wrap(~ week, ncol = 8, nrow = 8) +
               xlab("Longitude") + ylab("Latitude") +
  						 coord_sf(crs = crs_bering) +
  					   scale_x_continuous(
   						 breaks = c(170, 180, -170)) +
  						 scale_color_viridis_c() +
   						 labs(colour = "˚C") +
   						 theme_bw() +
  						 theme(legend.title = element_text(size = 10))
	
          plot
         
        }
         
        
        bottom_temp_weekly_plot_list <- lapply(sf_file_list, bottom_temp_weekly_plot_func)
        
        # save plots
        setwd("~/Google Drive/NOAA AFSC Postdoc/Pcod Bering Sea Habitat Suitability/Pcod-Bering-Sea/output/plots/weekly plots")
       
        years <- c(1970:2020)
        
        week_name_func <- function(x){
        	year_month <- paste0(x, "_bottom_temp_weekly")
        }
        
        week_names <- sapply(years, week_name_func)

				for(i in 1:length(bottom_temp_weekly_plot_list)) {
  				for (j in week_names) {
  					ggsave(plot = bottom_temp_weekly_plot_list[[i]],
        		file = paste(j,".png",sep=""),
        		width = 15, height = 15, units = "in")
  			}}
        
        
        #### function to plot -- hatch success ####
    
    		# facet by month 
    
    		hatch_success_monthly_plot_func <- function(x){
    	
    	  plot <- ggplot() +
  						 geom_sf(data = x, aes(color = hatch_success_cauchy)) +
  						 geom_sf(data=st_transform(bering_sf,crs=crs_bering),fill="lightgrey", 
  										 color="black",lwd=0.25) +
  						 facet_wrap(~ month_name, ncol = 4, nrow = 3) +
               xlab("Longitude") + ylab("Latitude") +
  						 coord_sf(crs = crs_bering) +
  					   scale_x_continuous(
   						 breaks = c(170, 180, -170)) +
  						 scale_color_viridis_c() +
   						 labs(colour = "Proportion hatch success") +
   						 theme_bw() +
  						 theme(legend.title = element_text(size = 10))
    	 
    	 plot
            
    }
    
        hatch_success_monthly_plot_list <- lapply(sf_file_list, hatch_success_monthly_plot_func)

        
        setwd("~/Google Drive/NOAA AFSC Postdoc/Pcod Bering Sea Habitat Suitability/Pcod-Bering-Sea/output/plots/monthly plots")
       
        years <- c(1970:2020)
        
        mo_name_func <- function(x){
        	year_month <- paste0(x, "_hatch_success_monthly")
        }
        
        month_names <- sapply(years, mo_name_func)

				for(i in 1:length(hatch_success_monthly_plot_list)) {
  			for (j in month_names) {
  			ggsave(plot = hatch_success_monthly_plot_list[[i]],
         file = paste(j,".png",sep=""),
         width = 10, height = 10, units = "in"
         )
  }}

     
        # facet by week -- bottom temp
        
        hatch_success_weekly_plot_func <- function(x){
         
        	
        	 plot <- ggplot() +
  						 geom_sf(data = x, aes(color = hatch_success_cauchy)) +
  						 geom_sf(data=st_transform(bering_sf,crs=crs_bering),fill="lightgrey", 
  										 color="black",lwd=0.25) +
  						 facet_wrap(~ week, ncol = 8, nrow = 8) +
               xlab("Longitude") + ylab("Latitude") +
  						 coord_sf(crs = crs_bering) +
  					   scale_x_continuous(
   						 breaks = c(170, 180, -170)) +
  						 scale_color_viridis_c() +
   						 labs(colour = "Proportion hatch success") +
   						 theme_bw() +
  						 theme(legend.title = element_text(size = 10))
	
          plot
         
        }
         
        
        hatch_success_weekly_plot_list <- lapply(sf_file_list, hatch_success_weekly_plot_func)

        setwd("~/Google Drive/NOAA AFSC Postdoc/Pcod Bering Sea Habitat Suitability/Pcod-Bering-Sea/output/plots/weekly plots")
       
        years <- c(1970:2020) 
        
        week_name_func <- function(x){
        	year_month <- paste0(x, "_hatch_success_weekly")
        }
        
        week_names <- sapply(years, week_name_func)

				for(i in 1:length(hatch_success_weekly_plot_list)) {
  			for (j in week_names) {
  			ggsave(plot = hatch_success_weekly_plot_list[[i]],
         file = paste(j,".png",sep=""),
         width = 15, height = 15, units = "in"
         )
  }}

