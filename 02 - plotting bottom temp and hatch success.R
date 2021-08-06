# different approach - function to plot each year separately

fl_list <- function(x){
    	file.path(main,Rdata_path,sim,"Level2", paste0(sim,var_use,x,".Rdata"))
    }
    
    fl_paths <- lapply(IDs, fl_list)
    
    fl_paths_sub <- fl_paths[c(1:10)]
    
    # function to read in file path and transform data

    data_transform <- function(x){
    
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
    
    		 data_long <- na.omit(data_long)
    		 
    		 # add date, month, day, week
  			 data_long$date <- as.Date(data_long$time) # date in Date format
				 data_long$month <- month(data_long$date) # month of year
				 data_long$day <- day(data_long$date) # day of year
				 data_long$week <- week(data_long$date) # week of year
	      
				 data_long$month_name <- NA
         
         data_long$month_name[data_long$month == 1] <- "January"
         data_long$month_name[data_long$month == 2] <- "February"
		     data_long$month_name[data_long$month == 3] <- "March"
		     data_long$month_name[data_long$month == 4] <- "April"
		     data_long$month_name[data_long$month == 5] <- "May"
		     data_long$month_name[data_long$month == 6] <- "June"
		     data_long$month_name[data_long$month == 7] <- "July"
		     data_long$month_name[data_long$month == 8] <- "August"
		     data_long$month_name[data_long$month == 9] <- "September"
		     data_long$month_name[data_long$month == 10] <- "October"
		     data_long$month_name[data_long$month == 11] <- "November"
		     data_long$month_name[data_long$month == 12] <- "December"
		
	       data_long <- na.omit(data_long)

				 data_long$month_name <- factor(data_long$month_name)
  			 data_long$month_name <- fct_reorder(data_long$month_name, data_long$month)
	 	
         df_sf <- convert2shp(data_long)
         
         
    		}

    
    sf_file_list <- lapply(fl_paths_sub, data_transform)
    
    
    # function to plot
    
    bottom_temp_monthly_plot_func <- function(x){
    	
    	  

    	  plot <- ggplot() +
  						 geom_sf(data = x, aes(color = val)) +
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
    
        monthly_plot_list <- lapply(sf_file_list, bottom_temp_monthly_plot_func)

        
        setwd("~/Google Drive/NOAA AFSC Postdoc/Pcod Bering Sea Habitat Suitability/Pcod-Bering-Sea/output")
       
        years <- c(1970:2020)
        
        mo_name_func <- 

				for(i in 1:length(monthly_plot_list)) {
  			for (j in names) {
  			ggsave(plot = monthly_plot_list[[i]],
         file = paste(j,".png",sep=""),
         width = 10, height = 10, units = "in"
         )
  }}

     
        # facet by week
        
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
         
        
        weekly_plot_list <- lapply(sf_file_list, bottom_temp_weekly_plot_func)

        setwd("~/Google Drive/NOAA AFSC Postdoc/Pcod Bering Sea Habitat Suitability/Pcod-Bering-Sea/output")
       
        names <- "1970_weekly"

				for(i in 1:length(weekly_plot_list)) {
  			for (j in names) {
  			ggsave(plot = weekly_plot_list[[i]],
         file = paste(j,".png",sep=""),
         width = 15, height = 15, units = "in"
         )
  }}
