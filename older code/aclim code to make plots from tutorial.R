# accessing ROMS data from ACLIM github: https://github.com/kholsman/ACLIM2

# first set working directory to ACLIM (current directory is my project on my machine)
setwd("~/ACLIM2") # alternatively, could go right to ACLIM project

# The below code will extract variables from the Level 2 and Level 3 netcdf files (.nc) and save them as 
#compressed .Rdata files on your local Data/in/Newest/Rdata folder.

#### Setup up the R workspace ####
# First let’s get the workspace set up, we will then step through an example downloading the hindcast and a 
# single projection (CMIP5 MIROC rcp8.5) but you can loop the code below to download the full set of 
# CMIP5 projections.

#### 3.3.1 Set up R workspace ####

# SETUP WORKSPACE
    # rm(list=ls())
    tmstp  <- format(Sys.time(), "%Y_%m_%d")
    main   <- getwd()  #"~/GitHub_new/ACLIM2
    source("R/make.R")

# Let’s take a look at the available online datasets:   
    
    # preview the datasets on the server:
    url_list <- tds_list_datasets(thredds_url = ACLIM_data_url)
    
    #display the full set of datasets:
    cat(paste(url_list$dataset,"\n"))
    
####  3.3.2. Download Level 2 data ####
    
# First we will explore the Level 2 bottom temperature data on the ACLIM Thredds server using the H16 
# hindcast and the H16 (CMIP5) projection for MIROC under rcp8.5. The first step is to get the data urls:
    
    # define the simulation to download:
    cmip <- "CMIP5"     # Coupled Model Intercomparison Phase
    GCM  <- "MIROC"     # Global Circulation Model
    rcp  <- "rcp85"     # future carbon scenario
    mod  <- "B10K-H16"  # ROMSNPZ model
    hind <- "CORECFS"   # Hindcast
    
    # define the projection simulation:
    proj  <- paste0(mod,"_",cmip,"_",GCM,"_",rcp)
    hind  <- paste0(mod,"_",hind)
    
    # get the url for the projection and hindcast datasets:
    proj_url       <- url_list[url_list$dataset == paste0(proj,"/"),]$path
    hind_url       <- url_list[url_list$dataset == paste0(hind,"/"),]$path
    
    # preview the projection and hindcast data and data catalogs (Level 1, 2, and 3):
    proj_datasets  <- tds_list_datasets(thredds_url = proj_url)
    hind_datasets  <- tds_list_datasets(thredds_url = hind_url)
    
    # get url for the projection and hindcast Level 2 and Level 3 catalogs
    proj_l2_cat    <- proj_datasets[proj_datasets$dataset == "Level 2/",]$path
    proj_l3_cat    <- proj_datasets[proj_datasets$dataset == "Level 3/",]$path
    hind_l2_cat    <- hind_datasets[hind_datasets$dataset == "Level 2/",]$path
    hind_l3_cat    <- hind_datasets[hind_datasets$dataset == "Level 3/",]$path
    hind_l2_cat
    
     # preview the projection and hindcast Level 2 datasets:
    proj_l2_datasets  <- tds_list_datasets(proj_l2_cat)
    hind_l2_datasets  <- tds_list_datasets(hind_l2_cat)
    proj_l2_datasets$dataset

# run this line if load_gis is set to F in R/setup.R:
    source("R/sub_scripts/load_maps.R")  

    # now create plots of average BT during four time periods
    time_seg   <- list( '1970-1980' = c(1970:1980),
                        '1980-1990' = c(1980:1990),
                        '1990-2000' = c(1990:2000),
                        '2000-2010' = c(2000:2010),
                        '2010-2020' = c(2010:2020))
    
     # preview the datasets on the server:
     tds_list_datasets(thredds_url = ACLIM_data_url)
  
    # assign the simulation to download
    # --> Tinker: try selecting a different set of models to compare
    sim        <- "B10K-K20_CORECFS" 
    #ms <- c("B10K-H16_CORECFS","B10K-K20_CORECFS" )
    
    # Currently available Level 2 variables
    dl     <- proj_l2_datasets$dataset  # datasets
    
    svl    <- list(
                'Bottom 5m' = "temp",
                'Surface 5m' = "temp",
                'Integrated' = c("EupS","Cop","NCaS") )
    
    # Let's sample the model years as close to Aug 1 as the model timesteps run:
    tr          <- c("-08-1 12:00:00 GMT") 
    
    # the full grid is large and takes a longtime to plot, so let's subsample the grid every 4 cells
   
    IDin       <- "_Aug1_subgrid"
    var_use    <- "_bottom5m_temp"
    
    # open a "region" or strata specific nc file
    fl         <- file.path(main,Rdata_path,sim,"Level2",
                            paste0(sim,var_use,IDin,".Rdata"))
    
   # load data from level 2 nc files (approx <10sec)
    startTime = Sys.time()
    if(!file.exists(file.path(Rdata_path,fl))){
      get_l2(
        ID          = "_1990_subgrid",
        overwrite   = T,
        xi_rangeIN  = seq(1,182,10),
        eta_rangeIN = seq(1,258,10),
        ds_list     = dl[1],  # must be same length as sub_varlist
        trIN        = tr,
        yearsIN     = 1990,
        sub_varlist = list('Bottom 5m' = "temp" ),  
        sim_list    = sim  )
    }
    endTime  = Sys.time()
    endTime  - startTime
    
    # load data from level 2 nc files for all years and vars (yearsIN = NULL by default)
    #       NOTE: THIS IS SLOOOOOW..~ 2 min
    startTime2 = Sys.time()
    if(!file.exists(file.path(Rdata_path,fl))){
      get_l2(
        ID          = IDin,
        overwrite   = T,
        xi_rangeIN  = seq(1,182,10),
        eta_rangeIN = seq(1,258,10),
        ds_list     = dl,
        trIN        = tr,
        sub_varlist = svl,  
        sim_list    = sim  )
    }
    endTime2  = Sys.time()
    endTime2  - startTime2
    
    # load R data file
    load(fl)   # temp
    
    # there are smarter ways to do this;looping because 
    # we don't want to mess it up but this is slow...
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
    
    
    # get the mean values for the time blocks from the rdata versions
    # may throw "implicit NA" errors that can be ignored
    tmp_var <-data_long # get mean var val for each time segment
    j<-0
    for(i in 1:length(time_seg)){
      if(length( which(as.numeric(tmp_var$year)%in%time_seg[[i]] ))>0){
        j <- j +1
        mn_tmp_var <- tmp_var%>%
          filter(year%in%time_seg[[i]],!is.na(val))%>%
          group_by(latitude, longitude)%>%
          summarise(mnval = mean(val,rm.na=T))
        
        mn_tmp_var$time_period <- factor(names(time_seg)[i],levels=names(time_seg))
        
      if(j == 1) mn_var <- mn_tmp_var
      if(j >  1) mn_var <- rbind(mn_var,mn_tmp_var)
       rm(mn_tmp_var)
      }
    }
    
    # convert results to a shapefile
    L2_sf  <- convert2shp(mn_var%>%filter(!is.na(mnval)))
    
    p9_hind     <- plot_stations_basemap(sfIN = L2_sf,
                                fillIN = "mnval",
                                colorIN = "mnval",
                                sizeIN=.6) +
      #facet_wrap(.~time_period,nrow=2,ncol=3)+
      facet_grid(.~time_period)+
      scale_color_viridis_c()+
      scale_fill_viridis_c()+
      guides(
        color =  guide_legend(title="Bottom T (degC)"),
        fill  =  guide_legend(title="Bottom T (degC)")) +
      ggtitle(paste(sim,var_use,IDin))
   
    
    ##### playing with geom_sf() ####
    
nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
    
ggplot(nc) +
  geom_sf(aes(fill = AREA))

nc_3857 <- sf::st_transform(nc, 3857)

ggplot() +
  geom_sf(data = nc) +
  geom_sf(data = nc_3857, colour = "red", fill = NA)

nc_3857$mid <- sf::st_centroid(nc_3857$geometry)

ggplot(nc_3857) +
  geom_sf(colour = "white") +
  geom_sf(aes(geometry = mid, size = AREA), show.legend = "point")


ggplot(nc_3857) +
  geom_sf() +
  annotate("point", x = -80, y = 35, colour = "red", size = 4) +
  coord_sf(default_crs = sf::st_crs(4326))