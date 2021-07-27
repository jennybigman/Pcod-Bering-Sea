# 01 -- playing with ROMS output

# goal is to map bottom temperatures for weekly slices of one year 

		setwd("~/ACLIM2") 
		
		library(devtools)
		library(ncdf4)
		library(thredds)
		library(lubridate)
		library(stringr)

 
    # SETUP WORKSPACE
    # rm(list=ls())
    tmstp  <- format(Sys.time(), "%Y_%m_%d")
    main   <- getwd()  #"~/GitHub_new/ACLIM2
    source("R/make.R")

    
		#### Step 1: get data URLs ####
    
    # preview the datasets on the server:
    url_list <- tds_list_datasets(thredds_url = ACLIM_data_url)
    
    #display the full set of datasets:
    cat(paste(url_list$dataset,"\n"))
    
    # define the simulation to download:
    mod  <- "B10K-H16"  # ROMSNPZ model #### B10K-K20 doesn't work here, why? ####
    hind <- "CORECFS"   # Hindcast
    
    # define the projection simulation:
    hind  <- paste0(mod,"_",hind)
    
    # get the url for the projection and hindcast datasets:
    hind_url       <- url_list[url_list$dataset == paste0(hind,"/"),]$path
    
    # preview the projection and hindcast data and data catalogs (Level 1, 2, and 3):
    hind_datasets  <- tds_list_datasets(thredds_url = hind_url)
    
    # get url for the projection and hindcast Level 2 catalog
    hind_l2_cat    <- hind_datasets[hind_datasets$dataset == "Level 2/",]$path
    hind_l2_cat

    # preview hindcast Level 2 datasets:
    hind_l2_datasets  <- tds_list_datasets(hind_l2_cat)
    hind_l2_datasets$dataset
    
    # get url for bottom temperature:
    hind_l2_BT_url    <- hind_l2_datasets[hind_l2_datasets$dataset == "Bottom 5m",]$path
    hind_l2_BT_url
    
    
		#### Download bottom temps for eavery year for week of Aug 1 ####
   
    # run this line if load_gis is set to F in R/setup.R:
    source("R/sub_scripts/load_maps.R")  #### I get an error here but checked source code and only one function so may not be an issue ####
    
    # assign the simulation to download
    sim        <- "B10K-K20_CORECFS" 

    
    # code below will extract weekly averaged temperature data for all years (1970-2020) for the week of Aug1
    
    # sample the model years as close to Aug 1 as the model timesteps run:
    tr          <- c("-08-1 12:00:00 GMT") 
    
        # the full grid is large and takes a longtime to plot, so let's subsample the grid every 4 cells
   
    IDin       <- "_Aug1_subgrid"
    var_use    <- "_bottom5m_temp"
    
    # open a "region" or strata specific nc file -- (creating a file path to open data later)
    fl         <- file.path(main,Rdata_path,sim,"Level2",
                            paste0(sim,var_use,IDin,".Rdata"))
   
    # load bottom temp data from level 2 nc files for all years for august  (yearsIN = NULL by default)
    startTime2 = Sys.time()
    if(!file.exists(file.path(Rdata_path,fl))){
      get_l2(
        ID          = IDin,
        overwrite   = T,
        xi_rangeIN  = seq(1,182,10),
        eta_rangeIN = seq(1,258,10),
        ds_list     = "Bottom 5m", # changed from tutorial code b/c we only wnat bottom temmps
        trIN        = tr,
        sub_varlist = list('Bottom 5m' = "temp" ), # changed from tutorial code b/c we only wnat bottom temmps
        sim_list    = sim  )
    }
    endTime2  = Sys.time()
    endTime2  - startTime2
    
    # load R data file
    load(fl)   # temp
    
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
    
    unique(data_long$year) # 1970 - 2020
    unique(data_long$time) # close to Aug 1 of each year
    
    
    #### Download bottom temps for every week of one year ####
    
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
   
    IDin       <- "_1970_subgrid"

    # creating a file path to open data later
    fl         <- file.path(main,Rdata_path,sim,"Level2",
                            paste0(sim,var_use,IDin,".Rdata"))
   
    # load bottom temp data from level 2 nc files for all years for august  (yearsIN = NULL by default)

    bt_func <- function(x){
    startTime2 = Sys.time()
    if(!file.exists(file.path(Rdata_path,fl))){
      get_l2(
        ID          = IDin,
        overwrite   = T,
        xi_rangeIN  = seq(1,182,10),
        eta_rangeIN = seq(1,258,10),
        ds_list     = "Bottom 5m", # changed from tutorial code b/c we only wnat bottom temmps
        trIN        = dates_times,
        yearsIN     = 1970,
        sub_varlist = list('Bottom 5m' = "temp" ), # changed from tutorial code b/c we only wnat bottom temmps
        sim_list    = sim  )
    }
    endTime2  = Sys.time()
    endTime2  - startTime2
    }
	  
    bottom_temp_lists <- sapply(1970, bt_func)
    
    # load R data file
    load(fl)   # temp
    
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
    
    unique(data_long$year) # 1970
    unique(data_long$time) # one weekly-averaged bottom temp data point for each week
    
    ########### above works to get weekly-averaged temp data for one year ##### (only 50 weeks available for 1970)
    
    #### Download weekly-averaged bottom temp from all years ####
    
    # assign the simulation to download
    sim        <- "B10K-K20_CORECFS" 

    # create lists of dates for which to extract bottom temp data -- we want one data point per week for
    # years 1970 - 2020 
    
    year_start_func <- function(x){
    	paste0(x, "-01-01")
    }
   
    years <- seq(1970, 2020, by = 1)
    
    start_dates <- sapply(years, year_start_func)
    
  	date_func <- function(x){
  	seq.Date(as.Date(x), by = "week", length.out = 52)
  	}
  
  	start_dates <- as.Date(start_dates)
  	
  	dates_list <- lapply(start_dates,function(x) { 
    			date_func(x)
		})

		# extract one list to work from 
  	
  	dates_list_1970 <- dates_list[[1]]
  	
  	dates_list_1970_1 <- str_remove(dates_list_1970, "1970")
  	
  	time_zone_func <- function(x){
  		paste(x, "12:00:00 GMT")
  	}
  	
  	dates_times <- sapply(dates_list_1970_1, time_zone_func)
  	
    # the full grid is large and takes a longtime to plot, so let's subsample the grid every 4 cells
  	### what is this? ###
   
    ID_func <- function(x){
    	IDin <- paste0(x, "_subgrid")
    	paste0("_", IDin)
    }
    
    years <- seq(1970, 2020, by = 1)

    IDs <- sapply(years, ID_func)
    
    
   
    # load bottom temp data from level 2 nc files for all years for august  (yearsIN = NULL by default)

    bt_func2 <- function(x, y){
    	
    	# creating a file path to open data later
    fl         <- file.path(main,Rdata_path,sim,"Level2",
                            paste0(sim,var_use,x,".Rdata"))
    
    startTime2 = Sys.time()
    if(!file.exists(file.path(Rdata_path,fl))){
      get_l2(
        ID          = x,
        overwrite   = T,
        xi_rangeIN  = seq(1,182,10),
        eta_rangeIN = seq(1,258,10),
        ds_list     = "Bottom 5m", # changed from tutorial code b/c we only wnat bottom temmps
        trIN        = dates_times,
        yearsIN     = y,
        sub_varlist = list('Bottom 5m' = "temp" ), # changed from tutorial code b/c we only wnat bottom temmps
        sim_list    = sim  )
    }
    endTime2  = Sys.time()
    endTime2  - startTime2
    }
	  
    bottom_temp_lists <- mapply(bt_func2, x = IDs, y = years)
    
    # load R data file
    load(fl)   # temp
    
    fl_list <- function(x){
    	file.path(main,Rdata_path,sim,"Level2", paste0(sim,var_use,x,".Rdata"))
    }
    
    fl_paths <- lapply(IDs, fl_list)
    

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
    
    unique(data_long$year) # 1970
    unique(data_long$time) # one weekly-averaged bottom temp data point for each week
    
    ########### above works to get weekly-averaged temp data for one year ##### (only 50 weeks available for 1970)
    
    #### Download weekly-averaged bottom temp from all years ####
    
    
    
    
    
    
    
    
    
    
    
    
    #################################################
    
  
    # assign the simulation to download
    sim        <- "B10K-K20_CORECFS" 

   
   # create a list of dates for which to extract bottom temp data (weekly averages)
    
    month_function <- function(x){
    	paste0(x, "-1 12:00:00 GMT")
    }
    
    numList <- seq(1, 12, by = 1)
    
    dates_times <- sapply(numList, month_function)
    
    # the full grid is large and takes a longtime to plot, so let's subsample the grid every 4 cells
   
    IDin       <- "_Aug1_subgrid"
    var_use    <- "_bottom5m_temp"
    
    # open a "region" or strata specific nc file -- (creating a file path to open data later)
    fl         <- file.path(main,Rdata_path,sim,"Level2",
                            paste0(sim,var_use,IDin,".Rdata"))
   
    # load bottom temp data from level 2 nc files for all years for august  (yearsIN = NULL by default)
    startTime2 = Sys.time()
    if(!file.exists(file.path(Rdata_path,fl))){
      get_l2(
        ID          = IDin,
        overwrite   = T,
        xi_rangeIN  = seq(1,182,10),
        eta_rangeIN = seq(1,258,10),
        ds_list     = "Bottom 5m", # changed from tutorial code b/c we only wnat bottom temmps
        trIN        = tr,
        sub_varlist = list('Bottom 5m' = "temp" ), # changed from tutorial code b/c we only wnat bottom temmps
        sim_list    = sim  )
    }
    endTime2  = Sys.time()
    endTime2  - startTime2
    
    # load R data file
    load(fl)   # temp
    
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
    
    unique(data_long$year) # 1970 - 2020
    unique(data_long$time) # close to Aug 1 of each year
    