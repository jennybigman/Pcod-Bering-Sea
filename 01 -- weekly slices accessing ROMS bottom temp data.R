# 01 -- playing with ROMS output

# goal is to map bottom temperatures for weekly slices of one year 

		setwd("~/ACLIM2") 
		
		library(devtools)
		library(ncdf4)
		library(thredds)

 
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
    
    
#### Step 2: Download a subset of the Level2 data (full 10KM Lat Lon re-gridded data) ####
   
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
    
    # open a "region" or strata specific nc file
    fl         <- file.path(main,Rdata_path,sim,"Level2",
                            paste0(sim,var_use,IDin,".Rdata"))
   
    # load data from level 2 nc files for all years and vars (yearsIN = NULL by default)
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
    
    