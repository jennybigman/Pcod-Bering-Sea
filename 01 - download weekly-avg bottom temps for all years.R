# 01 - download weekly-averaged bottom temp data for all years from Bering10K ROMS model

		#### load data resulting from below code
    library(data.table)
    library(tidyverse)
    setwd("~/Google Drive/NOAA AFSC Postdoc/Pcod Bering Sea Habitat Suitability")
		#all_temp_dat <- fread( "./data/all_temp_dat.csv")
    
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
  	
    # the full grid is large and takes a longtime to plot, so let's subsample the grid every 4 cells
  	  ### what is this? ###
   
    # function to create vector of IDs for labeling file names
    ID_func <- function(x){
    	IDin <- paste0(x, "_subgrid")
    	paste0("_", IDin)
    }
    
    years <- seq(1970, 2020, by = 1)

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
    		    #xi_rangeIN  = seq(1,182,10),
    		    #eta_rangeIN = seq(1,258,10),
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

  	# combine all temps from all years
    all_temp_dat <- bind_rows(data_list)
    
    
    # check to make sure all data is there
    unique(all_temp_dat$year)
		unique(all_temp_dat$time)  
	
		# write to csv
		setwd("~/Google Drive/NOAA AFSC Postdoc/Pcod Bering Sea Habitat Suitability")
		fwrite(all_temp_dat, "./data/all_temp_dat.csv")
		