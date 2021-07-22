# 01 -- playing with ROMS output

# goal is to map bottom temperatures for weekly slices of one year 

		setwd("~/ACLIM2") 
		
		library(devtools)
		library(ncdf4)
		library(thredds)

    # --------------------------------------
    # SETUP WORKSPACE
    # rm(list=ls())
    tmstp  <- format(Sys.time(), "%Y_%m_%d")
    main   <- getwd()  #"~/GitHub_new/ACLIM2
    source("R/make.R")
    # --------------------------------------
    
    #### Step 1: get data URLs ####
    
    # preview the datasets on the server:
    url_list <- tds_list_datasets(thredds_url = ACLIM_data_url)
    
    #display the full set of datasets:
    cat(paste(url_list$dataset,"\n"))
    # define the simulation to download:

    mod  <- "B10K-H16"  # ROMSNPZ model
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
    
    #### Download a subset of the Level2 data (full 10KM Lat Lon re-gridded data) ####
    