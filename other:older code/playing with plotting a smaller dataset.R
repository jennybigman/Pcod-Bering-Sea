# try converting to shapefile with fewer data points and plotting

#### Setup workspace ---------------------------------------------------------------
		setwd("~/ACLIM2") 
		
		library(devtools)
		library(ncdf4)
		library(thredds)
		library(lubridate)
		library(stringr)

    # rm(list=ls())
    tmstp  <- format(Sys.time(), "%Y_%m_%d")
    main   <- getwd()  #"~/GitHub_new/ACLIM2
    source("R/make.R")

    
    #### Create dataset options object -------------------------------------------------
    
    # preview the datasets on the server:
    url_list <- tds_list_datasets(thredds_url = ACLIM_data_url)
    
    #display the full set of datasets:
    cat(paste(url_list$dataset,"\n"))
    
    # define the simulation to download:
    mod  <- "B10K-H16"  # ROMSNPZ model 
    	#### B10K-K20 doesn't work here, why? ####
    hind <- "CORECFS"   # Hindcast
    
    # define the projection simulation:
    hind  <- paste0(mod,"_",hind)
    
    # get the url for the projection and hindcast datasets:
    hind_url       <- url_list[url_list$dataset == paste0(hind,"/"),]$path
    
    # preview the projection and hindcast data and data catalogs (Level 1, 2, and 3):
    hind_datasets  <- tds_list_datasets(thredds_url = hind_url)
    
    #### Download data ---------------------------------------------------------------

     # assign the simulation to download
    sim        <- "B10K-K20_CORECFS" 

    # create a list of dates for which to extract temp for
    weekly_dates <- seq.Date(as.Date("1970-01-01"), by = "week", length.out = 52)

		# remove the year because to get temp data, years and day/month are separate
  	weekly_dates <- str_remove(weekly_dates, "1970")
  	
  	# add a timestamp
  	time_zone_func <- function(x){
  		paste(x, "12:00:00 GMT")
  	}
  	
  	dates_times <- sapply(weekly_dates, time_zone_func)
  	
  	
  	# function to create vector of IDs for labeling file names
    ID_func <- function(x){
    	IDin <- paste0(x, "_subgrid")
    	paste0("_", IDin)
    }
    
    years <- seq(1970, 1975, by = 1)

    IDs <- sapply(years, ID_func)
    
    # for file nmae
    var_use    <- "_bottom5m_temp"
    
    # function to download weekly-averaged bottom temp data from level 2 nc files for all years 
    
     bt_func <- function(x, y){
    
    	# creating a file path to open data later
    		fl <- file.path(main,Rdata_path,sim,"Level2", paste0(sim,var_use,x,".Rdata"))
    
    	# code to download files from ACLIM tutorial
    		if(!file.exists(file.path(Rdata_path,fl))){
    		  get_l2(
    		    ID          = x,
    		    overwrite   = T,
    		    xi_rangeIN  = seq(1,182,10),
    		    eta_rangeIN = seq(1,258,10),
    		    ds_list     = "Bottom 5m", # changed from tutorial code b/c we only want bottom temps
    		    trIN        = dates_times,
    		    yearsIN     = y,
    		    sub_varlist = list('Bottom 5m' = "temp" ), # changed from tutorial code b/c we only want bottom temps
    		    sim_list    = sim  )
    			
    		}}
    
    bottom_temp_lists <- mapply(bt_func, x = IDs, y = years)

    # create a list of file paths to subsequently load

     fl_list <- function(x){
    	file.path(main,Rdata_path,sim,"Level2", paste0(sim,var_use,x,".Rdata"))
    }
    
    fl_paths <- lapply(IDs, fl_list)
    
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
    
    		data_long
    
    		}

	
  	data_list <- lapply(fl_paths, data_transform)

    temp_dat_lower_res_6yrs <- bind_rows(data_list)

    #### plot ####
    
    setwd("/Users/jenniferbigman/ACLIM2") 
		
		library(devtools)
		library(ncdf4)
		library(thredds)
		library(lubridate)
		library(stringr)
		
    main   <- getwd()  #"~/GitHub_new/ACLIM2

    source("R/make.R")
    source("R/sub_scripts/load_maps.R") 

    
    	# add columns
		temp_dat_lower_res_6yrs$date <- as.Date(temp_dat_lower_res_6yrs$time) # date in Date format
		
		temp_dat_lower_res_6yrs$month <- month(temp_dat_lower_res_6yrs$date) # month of year
		
		temp_dat_lower_res_6yrs$day <- day(temp_dat_lower_res_6yrs$date) # day of year

	  temp_dat_lower_res_6yrs$week <- week(temp_dat_lower_res_6yrs$date) # week of year
	  
	  #temp_dat_lower_res_6yrs_1970 <- temp_dat_lower_res_6yrs %>% filter(., year == 1970)
	  
		# convert to shape file
	  temp_dat_lower_res_6yrs_sf <- convert2shp(temp_dat_lower_res_6yrs)
	  
	  temp_dat_lower_res_6yrs_sf <- na.omit(temp_dat_lower_res_6yrs_sf)
	  
	  # plot for one year
	  
	  temp_dat_lower_res_6yrs_sf_1970 <- temp_dat_lower_res_6yrs_sf %>% filter(., year == 1970)
	  temp_dat_lower_res_6yrs_sf_1970_NA <- na.omit(temp_dat_lower_res_6yrs_sf_1970)
	  
	  bottomT_ggplot_plot <- ggplot() +
  												 geom_sf(data = temp_dat_lower_res_6yrs_sf_1970_NA, 
  												 				aes(color = val)) +
  												 geom_sf(data=st_transform(bering_sf,crs=crs_bering),fill="lightgrey", # this is the part I need to figure out
  												 				color="black",lwd=0.25) +
                        	 xlab("Longitude") + ylab("Latitude") +
  										  	 coord_sf(crs = crs_bering) +
  												 scale_x_continuous(
   													breaks = c(170, 180, -170)
   												  ) +
  												scale_color_viridis_c() +
   												labs(colour = "Bottom temperature (˚C)") +
   												theme_bw() 
	
	  
	  # facet by month
	  add_col_text <- function(df, x, y, z){
	  	df$x[df$x == y] <- z
	  }
	  
	  months_names_list <- c("January", "February", "March", "April", "May", "June", "July", 
	  								 "August", "September", "October", "November", "December")
	  
	  month_num_list <- c(1:12)
	  
	  df <- mapply(add_col_text, df = temp_dat_lower_res_6yrs_sf_1970_NA,
	  						 x = temp_dat_lower_res_6yrs_sf_1970_NA$month,
	  						 y = month_num_list,
	  						 z = months_names_list)

		temp_dat_lower_res_6yrs_sf_1970$month_name[temp_dat_lower_res_6yrs_sf_1970$month == 1] <- "January"
		temp_dat_lower_res_6yrs_sf_1970$month_name[temp_dat_lower_res_6yrs_sf_1970$month == 2] <- "February"
		temp_dat_lower_res_6yrs_sf_1970$month_name[temp_dat_lower_res_6yrs_sf_1970$month == 3] <- "March"
		temp_dat_lower_res_6yrs_sf_1970$month_name[temp_dat_lower_res_6yrs_sf_1970$month == 4] <- "April"
		temp_dat_lower_res_6yrs_sf_1970$month_name[temp_dat_lower_res_6yrs_sf_1970$month == 5] <- "May"
		temp_dat_lower_res_6yrs_sf_1970$month_name[temp_dat_lower_res_6yrs_sf_1970$month == 6] <- "June"
		temp_dat_lower_res_6yrs_sf_1970$month_name[temp_dat_lower_res_6yrs_sf_1970$month == 7] <- "July"
		temp_dat_lower_res_6yrs_sf_1970$month_name[temp_dat_lower_res_6yrs_sf_1970$month == 8] <- "August"
		temp_dat_lower_res_6yrs_sf_1970$month_name[temp_dat_lower_res_6yrs_sf_1970$month == 9] <- "September"
		temp_dat_lower_res_6yrs_sf_1970$month_name[temp_dat_lower_res_6yrs_sf_1970$month == 10] <- "October"
		temp_dat_lower_res_6yrs_sf_1970$month_name[temp_dat_lower_res_6yrs_sf_1970$month == 11] <- "November"
		temp_dat_lower_res_6yrs_sf_1970$month_name[temp_dat_lower_res_6yrs_sf_1970$month == 12] <- "December"

		temp_dat_lower_res_6yrs_sf_1970_NA <- na.omit(temp_dat_lower_res_6yrs_sf_1970)
		
		temp_dat_lower_res_6yrs_sf_1970_NA$month_name <- factor(temp_dat_lower_res_6yrs_sf_1970_NA$month_name)
    temp_dat_lower_res_6yrs_sf_1970_NA$month_name <- fct_reorder(temp_dat_lower_res_6yrs_sf_1970_NA$month_name,
    																								 temp_dat_lower_res_6yrs_sf_1970_NA$month)
	 	
   test_botT_month_plot <- ggplot() +
  												 geom_sf(data = temp_dat_lower_res_6yrs_sf_1970_NA, 
  												 				aes(color = val)) +
  												 geom_sf(data=st_transform(bering_sf,crs=crs_bering),fill="lightgrey", # this is the part I need to figure out
  												 				color="black",lwd=0.25) +
  												 facet_wrap(~ month_name, ncol = 4, nrow = 3) +
                        	 xlab("Longitude") + ylab("Latitude") +
  										  	 coord_sf(crs = crs_bering) +
  												 scale_x_continuous(
   													breaks = c(170, 180, -170)
   												  ) +
  												scale_color_viridis_c() +
   												labs(colour = "˚C") +
   												theme_bw() +
  												theme(
  													legend.title = element_text(size = 10)
  													)
   
   ggsave(file = ("./output/plots/test_botT_month_plot.png"),
   			  plot = test_botT_month_plot,
   			  height = 7.5, width = 12, units = "in")

   
   # facet by week
   
   test_botT_week_plot <- ggplot() +
  												 geom_sf(data = temp_dat_lower_res_6yrs_sf_1970_NA, 
  												 				aes(color = val)) +
  												 geom_sf(data=st_transform(bering_sf,crs=crs_bering),fill="lightgrey", # this is the part I need to figure out
  												 				color="black",lwd=0.25) +
  												 facet_wrap(~ week, ncol = 8, nrow = 8) +
                        	 xlab("Longitude") + ylab("Latitude") +
  										  	 coord_sf(crs = crs_bering) +
  												 scale_x_continuous(
   													breaks = c(170, 180, -170)
   												  ) +
  												scale_color_viridis_c() +
   												labs(colour = "˚C") +
   												theme_bw() +
  												theme(
  													legend.title = element_text(size = 10)
  													)
	
	   ggsave(file = ("./output/plots/test_botT_week_plot.png"),
   			  plot = test_botT_week_plot,
   			  height = 7.5, width = 12, units = "in")
