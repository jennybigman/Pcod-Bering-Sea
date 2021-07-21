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

# Now that we have the URLs let’s take a look at the available Level2 datasets:
    
    # Bottom 5m : bottom water temperature at 5 meters
    # Surface 5m : surface water temperature in the first 5 meters
    # Integrated : Integrated water column averages for various NPZ variables
    
    # preview the projection and hindcast Level 2 datasets:
    proj_l2_datasets  <- tds_list_datasets(proj_l2_cat)
    hind_l2_datasets  <- tds_list_datasets(hind_l2_cat)
    proj_l2_datasets$dataset
    
    # get url for bottom temperature:
    proj_l2_BT_url    <- proj_l2_datasets[proj_l2_datasets$dataset == "Bottom 5m",]$path
    hind_l2_BT_url    <- hind_l2_datasets[hind_l2_datasets$dataset == "Bottom 5m",]$path
    proj_l2_BT_url
    
# Now we can download a subset of the Level2 data (full 10KM Lat Lon re-gridded data), here with an example
# of sampling on Aug 1 of each year:    
    
    # Currently available Level 2 variables
    dl     <- proj_l2_datasets$dataset  # datasets

    # variable list
    svl <- list(
           'Bottom 5m' = "temp",
           'Surface 5m' = "temp",
           'Integrated' = c("EupS","Cop","NCaS") ) 
       
    
    # preview the variables, timesteps, and lat lon in each dataset:
    l2_info <- scan_l2(ds_list = dl,sim_list = "B10K-H16_CORECFS" )
    
    names(l2_info)
    l2_info[["Bottom 5m"]]$vars 
    l2_info[["Surface 5m"]]$vars
    l2_info[["Integrated"]]$vars
    max(l2_info[["Integrated"]]$time_steps)
    l2_info[["Integrated"]]$years
    
    
    # Simulation list:
    # --> --> Tinker:add additional projection scenarios here
    sl <- c(hind, proj)

    # Currently available Level 2 variables
    dl     <- proj_l2_datasets$dataset  # datasets
    
    # variables to pull from each data set
    # --> --> Tinker: try subbing in other Integrated variables 
    # (l2_info[["Integrated"]]$vars) into the third list vector 
    svl <- list(
      'Bottom 5m' = "temp",
      'Surface 5m' = "temp",
      'Integrated' = c("EupS","Cop","NCaS") ) 
   
    
    # Let's sample the model years as close to Aug 1 as the model timesteps run: 
    # --> --> Tinker - try a different date
    tr          <- c("-08-1 12:00:00 GMT") 
    
    # grab nc files from the aclim server and convert to rdatafiles with the ID Aug1
    get_l2(
      ID          = "_Aug1",
      overwrite   =  T,
      ds_list     = dl,
      trIN        = tr,
      sub_varlist = svl,  
      sim_list    = sl  ) 
    
    
    #### how do I work with these data I just downloaded #### ? 
    
#### 3.3.3 Download Level 3 data ####
    
# Now let’s grab some of the Level 3 data and store it in the Data/in/Newest/Rdata folder. 
# This is comparatively faster because Level 3 files are already post-processed to be in the ACLIM indices 
# format and are relatively small:
    
    # Simulation list:
    # --> --> Tinker:add additional projection scenarios here
    sl <- c(hind, proj)
    
    # variable list
    # --> --> Tinker:add additional variables to varlist
    vl <- c(
              "temp_bottom5m",    # bottom temperature,
              "NCaS_integrated",  # Large Cop
              "Cop_integrated",   # Small Cop
              "EupS_integrated")  # Shelf  euphausiids
    
    # convert  nc files into a long data.frame for each variable
    # three options are:

    # opt 1: access nc files remotely (fast, less local storage needed)
    get_l3(web_nc = TRUE, download_nc = F,
          varlist = vl,sim_list = sl)
    
    # opt 2:  download nc files then access locallly:
    get_l3(web_nc = TRUE, download_nc = T,
          local_path = file.path(local_fl,"aclim_thredds"),
          varlist = vl,sim_list = sl)
    
     # opt 3:  access existing nc files locally:
    get_l3(web_nc = F, download_nc = F,
          local_path = file.path(local_fl,"aclim_thredds"),
          varlist = vl,sim_list = sl)
    
#### 3.3.4 Download Level 3 CMIP6 (ACLIM only for now) ####
    
    # download from google drive
    
#### 4 Explore indices and plot data ###
    
#### 4.1 Data exploration with the minimal installation ####    
    
library(ncdf4)
library(thredds)     
    
   # Open connection to Level 2 bottom 5 meter layer
   url_base <- "https://data.pmel.noaa.gov/aclim/thredds/"
   opendap  <- "dodsC/Level2/B10K-K20_CORECFS_bottom5m.nc"
   nc       <- nc_open(paste(url_base,opendap,sep=""))

   # Examination of the nc object shows variables such as temperature (temp)
   #        float temp[xi_rho,eta_rho,ocean_time]   
   #            long_name: time-averaged potential temperature, bottom 5m mean
   #            units: Celsius
   #            time: ocean_time
   #            coordinates: lon_rho lat_rho ocean_time
   #            field: temperature, scalar, series
   #            _FillValue: 9.99999993381581e+36
   #            cell_methods: s_rho: mean

   # temp has three dimensions - xi_rho, eta_rho, and ocean_time
   # Now we make vectors of each axis.
   xi_axis  <- seq(1,182) # Hardcoded axis length #### what is this ####
   eta_axis <- seq(1,258) # Hardcoded axis length
   
   # time units in GMT: seconds since 1900-01-01 00:00:00
   t_axis   <- ncvar_get(nc,"ocean_time")
   time_axis <- as.POSIXct(t_axis, origin = "1900-01-01", tz = "GMT") #### what is this ####

   # Make two dates to find in the data
   date1 <- ISOdate(year=2010, month=7, day=1, hour = 12, tz = "GMT")
   date2 <- ISOdate(year=2019, month=7, day=1, hour = 12, tz = "GMT")

   # Which time index is closest to those dates?
   timerec1 <- which.min(abs(time_axis - date1))
   timerec2 <- which.min(abs(time_axis - date2))

   # Center time of the closest weekly average
   time_axis[timerec1]
   time_axis[timerec2]

   # Get full xi, eta grid (count=-1) for two time slices
   # Get one record starting at desired timerec.  
   # Careful (easy to grab too much data, if count and start are missing
   # it will grab all the data).
   temp1 <- ncvar_get(nc, "temp", start=c(1,1,timerec1), count=c(-1,-1,1))
   temp2 <- ncvar_get(nc, "temp", start=c(1,1,timerec2), count=c(-1,-1,1))

   # Plot comparison (not checking scale here)
   par(mfrow=c(1,2))
   image(temp1)
   image(temp2)
   
   # Get lat/lon for better mapping - getting whole variable #### is this lat/long ####
   lats <- ncvar_get(nc,"lat_rho")
   lons <- ncvar_get(nc,"lon_rho")

   # Visualizing the coordinate transformation 
   plot(lons,lats)

   # Let's flag water <2 degrees C
   par(mfrow=c(1,2))
   plot(lons,lats,col=ifelse(temp1<2,"blue","green"),main="2010")
   plot(lons,lats,col=ifelse(temp2<2,"blue","green"),main="2019")

   # Close the connection
   nc_close(nc)
   
   # for figure: Bottom temperature <2 degrees C (blue) and >=2 degrees C (green)
   
# 4.2 Level 3 indices ####
   
# Level 3 indices can be used to generate seasonal, monthly, and annual indices (like those reported in
# Reum et al. (2020), Holsman et al. (2020). 
   
# In the section below we explore these indices in more detail using R, including using (2) above to 
# generate weekly, monthly, and seasonal indices (e.g. Fall Zooplankton) for use in biological models.

# In section 3 below we explore these indices in more detail using R, including using (2) above to generate 
# weekly, monthly, and seasonal indices (e.g. Fall Zooplankton) for use in biological models. 
   
# The following examples show how to analyze and plot the ACLIM indices from the .Rdata files created in the
# previous step 3. 

# Please be sure to coordinate with ROMSNPZ modeling team members to ensure data is applied appropriately.   

# 4.2.1 Explore Level 3 data catalog ####
   
# Once the base files and setup are loaded you can explore the index types. Recall that in each scenario 
# folder there are two indices saved within the Level3 subfolders:

# ACLIMsurveyrep_B10K-x.nc contains summer groundfish trawl “survey replicated” indices (using mean date and 
# lat lon) (Note that the resampling stations need to be removed before creating bottom temperature maps)

# ACLIMregion_B10K-x.nc: contains weekly “strata” values (Note that area weighting should be used to combine
# values across multiple strata)
   
# First run the below set of code to set up the workspace:
   
    # SETUP WORKSPACE
    tmstp  <- format(Sys.time(), "%Y_%m_%d")
    main   <- getwd()  #"~/GitHub_new/ACLIM2
    source("R/make.R")

    # list of the scenario x GCM downscaled ACLIM indices
    for(k in aclim)
     cat(paste(k,"\n"))
    
    embargoed # not yet public or published
    public    # published runs (CMIP5)
    
    # get some info about a scenario:
    all_info1
    all_info2
   
    # variables in each of the two files:
    srvy_vars
    weekly_vars
  
    #summary tables for variables
    srvy_var_def
    weekly_var_def
    
    # explore stations in the survey replicated data:
    head(station_info)

# 4.2.2 Level 3 Spatial indices (survey replicated) ####
    
# Let’s start by exploring the survey replicated values for each variable. Previous steps generated the 
# Rdata files that are stored in the ACLIMsurveyrep_B10K-[version_CMIPx_GCM_RCP].Rdata in each corresponding
# simulation folder
    
# plot     
    
    
  # if load_gis is set to FALSE in R/setup.R (default) 
   # we will need to load the gis layers and packages
   # if this is the first time through this would be a good time
   # to grab a coffee...
   
   source("R/sub_scripts/load_maps.R")
  
   # first convert the station_info object into a shapefile for mapping:
   station_sf         <- convert2shp(station_info)
   station_sf$stratum <- factor(station_sf$stratum)
   
   # plot the stations:
   p <- plot_stations_basemap(sfIN = station_sf,
                              fillIN = "subregion",
                              colorIN = "subregion") + 
     scale_color_viridis_d(begin = .2,end=.6) +
     scale_fill_viridis_d(begin  = .2,end=.6)
  
   if(update.figs){
     p
     ggsave(file=file.path(main,"Figs/stations_NS.jpg"),width=5,height=5)
    }

   p2 <- plot_stations_basemap(sfIN = station_sf,fillIN = "stratum",colorIN = "stratum") + 
     scale_color_viridis_d() +
     scale_fill_viridis_d()
   
   if(update.figs){
     p2
   ggsave(file=file.path(main,"Figs/stations.jpg"),width=5,height=5)}
    
#### Section 5 -- Hindcasts ####
  
# 5.1 Level 3 hindcasts ####
   
# Level 3 hindcast products inculde survey replicated station data and strata averaged weekly values. 
# The code below will explore these in more detail
    
# 5.1.1 Level 3 hindcasts: spatial patterns #### 
   # this code shows how to get data from the .Rdata files ####
   
# Now let’s explore the survey replicated data in more detail and use to plot bottom temperature.

# run this line if load_gis is set to F in R/setup.R:
    source("R/sub_scripts/load_maps.R")  

    # preview the l3 data for the hindcast:
    tt <- all_info1%>%filter(name =="B10K-K20_CORECFS")
    tt <- seq(as.numeric(substring(tt$Start,1,4)),
              as.numeric(substring(tt$End,1,4)),10)
    
    # now create plots of average BT during four time periods
    time_seg   <- list( '1970-1980' = c(1970:1980),
                        '1980-1990' = c(1980:1990),
                        '1990-2000' = c(1990:2000),
                        '2000-2010' = c(2000:2010),
                        '2010-2020' = c(2010:2020))
  
    # lists the possible variables
    srvy_vars  # lists the possible variables
    
    # specify the variables to plot
    vl        <- c(
                  "temp_bottom5m",
                  "NCaS_integrated", # Large Cop
                  "Cop_integrated",  # Small Cop
                  "EupS_integrated") # Euphausiids
    
    # assign the simulation to download
    # --> Tinker: try selecting a different set of models to compare
    sim        <-"B10K-K20_CORECFS" 
    
    # open a "region" or strata specific nc file
    fl         <- file.path(sim,paste0(srvy_txt,sim,".Rdata"))
     
    # create local rdata files (opt 1)
    if(!file.exists(file.path(Rdata_path,fl)))
      get_l3(web_nc = TRUE, download_nc = F,
          varlist = vl,sim_list =sim )
    
    # load object 'ACLIMsurveyrep'
    load(file.path(main,Rdata_path,fl))   
    
       
    
    # Collate mean values across timeperiods and simulations

        ms <- c("B10K-H16_CORECFS","B10K-K20_CORECFS" )
   
    # Loop over model set
    for(sim in ms){
     fl         <- file.path(sim,paste0(srvy_txt,sim,".Rdata"))
     
    if(!file.exists( file.path(Rdata_path,fl)) )
      get_l3(web_nc = TRUE, download_nc = F,
          varlist = vl,sim_list =sim )
    }
      
    # get the mean values for the time blocks from the rdata versions
    # will throw "implicit NA" errors that can be ignored
    mn_var_all <- get_mn_rd(modset = ms,
                            names  = c("H16","K20") ,
                            varUSE = "temp_bottom5m")
    # --> Tinker:           varUSE = "EupS_integrated") 
    
    # convert results to a shapefile
    mn_var_sf  <- convert2shp(mn_var_all%>%filter(!is.na(mnval)))
    lab_t      <- "Bering10K CORECFS hindcast"
    
    p_hind_3         <- plot_stations_basemap(sfIN = mn_var_sf,
                                fillIN = "mnval",
                                colorIN = "mnval",
                                sizeIN=.3) +
      facet_grid(simulation~time_period)+
      scale_color_viridis_c()+
      scale_fill_viridis_c()+
      guides(
        color =  guide_legend(title="Bottom T (degC)"),
        fill  =  guide_legend(title="Bottom T (degC)")) +
      ggtitle(lab_t)
   
    # This is slow but it works (repeat dev.new() twice if in Rstudio)...
    dev.new()
    p_hind_3
    
    if(update.figs)  
      ggsave(file=file.path(main,"Figs/mn_hindcast_BT.jpg"),width=8,height=6)
    
 # now let's look at the marine heatwave cobditions in 2018 and compare that to the average conditions prior
 # to 2010
    
    # now create plots of average BT during four time periods
    time_seg   <- list( '1970-2010' = c(1970:2010),
                        '2018-2018' = c(2018:2018))
  
    # assign the simulation to download
    sim        <- "B10K-K20_CORECFS" 
    
    # open a "region" or strata specific nc file
    fl         <- file.path(sim,paste0(srvy_txt,sim,".Rdata"))
     
    # load object 'ACLIMsurveyrep'
    load(file.path(main,Rdata_path,fl))   
      
    # get the mean values for the time blocks from the rdata versions
    mn_var_all <- get_mn_rd(modset = "B10K-K20_CORECFS",
                            varUSE = "temp_bottom5m")
    
    # convert results to a shapefile
    mn_var_sf  <- convert2shp(mn_var_all%>%filter(!is.na(mnval)))
    lab_t      <- "Bering10K CORECFS hindcast"
    
    p_mhw      <- plot_stations_basemap(sfIN = mn_var_sf,
                                fillIN = "mnval",
                                colorIN = "mnval",
                                sizeIN=.3) +
      facet_grid(simulation~time_period)+
      scale_color_viridis_c()+
      scale_fill_viridis_c()+
      guides(
        color =  guide_legend(title="Bottom T (degC)"),
        fill  =  guide_legend(title="Bottom T (degC)")) +
      ggtitle(lab_t)
   
    # This is slow but it works (repeat dev.new() twice if in Rstudio)...
    dev.new(width=4,height=3)
    p_mhw
    
    if(update.figs)  
      ggsave(file=file.path(main,"Figs/mn_hindcast_mhw.jpg"),width=4,height=3)
    
    
    # these are decadal averages of bottom temperature from the two hindcast models
    
# 5.1.2 Level 3 hindcasts: weekly strata averages ####
    
# The next set of indices to  explore are the weekly strata-specific values for each variable.
# These are stored in the ACLIMregion_B10K-[version_CMIPx_GCM_RCP].nc in each scenario folder.

 # View an individual variable (e.g., Bottom Temp)

        weekly_vars

    # assign the simulation to download
    sim        <- "B10K-K20_CORECFS" 
    
    # define a "region" or strata specific nc file
    fl         <- file.path(sim,paste0(reg_txt,sim,".Rdata"))
    

    vl        <- c(
                  "temp_bottom5m",
                  "NCaS_integrated", # Large Cop
                  "Cop_integrated",  # Small Cop
                  "EupS_integrated") # Euphausiids
    
    # create local rdata files (opt 1)
    if(!file.exists(file.path(Rdata_path,fl)))
      get_l3(web_nc = TRUE, download_nc = F,
          varlist = vl,sim_list = sim)
    
 
    # load object 'ACLIMregion' for bottom temperature
    load(file.path(main,Rdata_path,fl))  
    tmp_var    <- ACLIMregion%>%filter(var == "temp_bottom5m")
    
   # now plot the data:
   p4_hind <- ggplot(data = tmp_var) + 
     geom_line(aes(x=time,y=val,color= strata),alpha=.8)+
     facet_grid(basin~.)+
     ylab(tmp_var$units[1])+
     ggtitle( paste(sim,tmp_var$var[1]))+
     theme_minimal()
   p4_hind
   
    if(update.figs)  
      ggsave(file=file.path(main,"Figs/hind_weekly_bystrata.jpg"),width=8,height=5)

   
   # To get the average value for a set of strata, weight the val by the area:
   mn_NEBS <- getAVGnSUM(strataIN = NEBS_strata, dataIN = tmp_var)
   mn_NEBS$basin = "NEBS"
   mn_SEBS <-getAVGnSUM(strataIN = SEBS_strata, dataIN = tmp_var)
   mn_SEBS$basin = "SEBS"
   
   p5_hind <- ggplot(data = rbind(mn_NEBS,mn_SEBS)) + 
      geom_line(aes(x=time,y=mn_val,color=basin),alpha=.8)+
      geom_smooth(aes(x=time,y=mn_val,color=basin),
                  formula = y ~ x, se = T)+
      facet_grid(basin~.)+
      scale_color_viridis_d(begin=.4,end=.8)+
      ylab(tmp_var$units[1])+
      ggtitle( paste(sim,mn_NEBS$var[1]))+
      theme_minimal()
   
  p5_hind
  if(update.figs)  
    ggsave(file=file.path(main,"Figs/hind_weekly_byreg.jpg"),width=8,height=5)
  
# 5.1.3 Level 3 hindcasts: seasonal averages ####
  
# now using a similar approach get the seasonal mean values for a variable
  
 # assign the simulation to download
      sim        <- "B10K-K20_CORECFS" 
   

    # Set up seasons (this follows Holsman et al. 2020)
      seasons <- data.frame(mo = 1:12, 
                   season =factor("",
                     levels=c("Winter","Spring","Summer","Fall")))
      seasons$season[1:3]   <- "Winter"
      seasons$season[4:6]   <- "Spring"
      seasons$season[7:9]   <- "Summer"
      seasons$season[10:12] <- "Fall"
    
       
    vl <- c(
                  "temp_bottom5m",
                  "NCaS_integrated", # Large Cop
                  "Cop_integrated",  # Small Cop
                  "EupS_integrated") # Euphausiids
    
    # create local rdata files (opt 1)
    if(!file.exists(file.path(Rdata_path,fl)))
      get_l3(web_nc = TRUE, download_nc = F,
          varlist = vl,sim_list = sim)
    
    # open a "region" or strata specific  file
    fl      <- file.path(sim,paste0(reg_txt,sim,".Rdata"))
    load(file.path(main,Rdata_path,fl))
    
    # get large zooplankton as the sum of euph and NCaS
    tmp_var    <- ACLIMregion%>%
      filter(var%in%vl[c(2,3)])%>%
      group_by(time,strata,strata_area_km2,basin)%>%
      group_by(time,
             strata,
             strata_area_km2,
             basin,
             units)%>%
      summarise(val =sum(val))%>%
      mutate(var       = "Zoop_integrated",
             long_name ="Total On-shelf 
             large zooplankton concentration, 
             integrated over depth (NCa, Eup)")
    
    rm(ACLIMregion)
    head(tmp_var)
    
    # define some columns for year mo and julian day
    tmp_var$yr     <- strptime(as.Date(tmp_var$time),
                               format="%Y-%m-%d")$year + 1900
    tmp_var$mo     <- strptime(as.Date(tmp_var$time),
                               format="%Y-%m-%d")$mon  + 1
    tmp_var$jday   <- strptime(as.Date(tmp_var$time),
                               format="%Y-%m-%d")$yday + 1
    tmp_var$season <- seasons[tmp_var$mo,2]
    
    # To get the average value for a set of strata, weight the val by the area: (slow...)
    mn_NEBS_season <- getAVGnSUM(
      strataIN = NEBS_strata,
      dataIN = tmp_var,
      tblock=c("yr","season"))
    mn_NEBS_season$basin = "NEBS"
    
    mn_SEBS_season <- getAVGnSUM(
      strataIN = SEBS_strata, 
      dataIN = tmp_var,
      tblock=c("yr","season"))
    mn_SEBS_season$basin = "SEBS"
    
   plot_data      <- rbind(mn_NEBS_season,mn_SEBS_season)
    
   # plot Fall values:
   p6_hind <- ggplot(data = plot_data%>%filter(season=="Fall") ) + 
      geom_line(   aes(x = yr,y = mn_val,color=basin),alpha=.8)+
      geom_smooth( aes(x = yr,y = mn_val,color=basin),
                  formula = y ~ x, se = T)+
      facet_grid(basin~.)+
      scale_color_viridis_d(begin=.4,end=.8)+
      ylab(tmp_var$units[1])+
      ggtitle( paste(sim,"Fall",mn_NEBS_season$var[1]))+
      theme_minimal()
  p6_hind
  
  
  if(update.figs)  
    ggsave(file=file.path(main,"Figs/Hind_Fall_large_Zoop.jpg"),width=8,height=5)

# 5.1.4 Level 3 hindcasts: monthly averages ####
  
  # To get the average value for a set of strata, weight the val by the area: (slow...)
    mn_NEBS_season <- getAVGnSUM(
      strataIN = NEBS_strata,
      dataIN   = tmp_var,
      tblock   = c("yr","mo"))
    mn_NEBS_season$basin = "NEBS"
    
    mn_SEBS_season <- getAVGnSUM(
      strataIN = SEBS_strata, 
      dataIN = tmp_var,
      tblock=c("yr","mo"))
    mn_SEBS_season$basin = "SEBS"
    
    plot_data      <- rbind(mn_NEBS_season,mn_SEBS_season)
    
   # plot Fall values:
   p7_hind <- ggplot(data = plot_data%>%filter(mo==9) ) + 
      geom_line(   aes(x = yr,y = mn_val,color=basin),alpha=.8)+
      geom_smooth( aes(x = yr,y = mn_val,color=basin),
                  formula = y ~ x, se = T)+
      facet_grid(basin~.)+
      scale_color_viridis_d(begin=.4,end=.8)+
      ylab(tmp_var$units[1])+
      ggtitle( paste(aclim[2],"Sept.",mn_NEBS_season$var[1]))+
      theme_minimal()
   dev.new()
  p7_hind
  
  if(update.figs)  
    ggsave(file=file.path(main,"Figs/Hind_Sept_large_Zoop.jpg"),width=8,height=5)
  
  
#### 5.2 Level 2 hindcasts #### 
   
# Level 2 data can be explored in the same way as the above indices but we will focus in the section below 
# on a simple spatial plot and temporal index. The advantage of Level2 indices is in the spatial resolution
# and values outside of the survey area.
   
#  5.2.1 Level 2 hindcasts: custom spatial indices ####
   
# create spatial plots of hindcast time periods for Aug 1 of each year
   
   # run this line if load_gis is set to F in R/setup.R:
    source("R/sub_scripts/load_maps.R")  

    # now create plots of average BT during four time periods ### five time periods ? ####
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
    
    ## mnval is mean temp at the lat/long (for the time period) #### ?
    
    ## so this code already rotated the xi and eta axis to geographic axes? ####
    
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
   
    # This is slow but it works (repeat dev.new() twice if in Rstudio)...
    dev.new()
    p9_hind
    
    if(update.figs)  
      ggsave(file=file.path(main,"Figs/Hind_sub_grid_mn_BT_Aug1.jpg"),width=8,height=4)
  
    # graphics.off()
  
    
# 5.2.2 Level 2 hindcasts: M2 mooring comparison ####
    
    # as a final hindcast comparison, let's look at a surface temperature from observations vs the
    # H16 and K20 versions of the hindcast
    
    # M2_lat <- (56.87°N, -164.06°W)
    # 56.877    -164.06 xi = 99    eta= 62
    IDin       <- "_2013_M2"
    var_use    <- "_surface5m_temp"
    
    # get data from M2 data page:
    pmelM2_url <-"https://www.ncei.noaa.gov/data/oceans/ncei/ocads/data/0157599/"
    yr_dat     <- "M2_164W_57N_Apr2019_May2019.csv"
    yr_dat     <- "M2_164W_57N_May2013_Sep2013.csv"
        
    # preview the datasets on the server:
    temp <- tempfile()
    download.file(paste0(pmelM2_url,yr_dat),temp)
    #M2data <- read.csv(temp,skip=4,stringsAsFactors = F)
    M2data <- read.csv(temp,skip=0,stringsAsFactors = F)
    
    unlink(temp)
      
    # convert date and time to t 
    M2data$t <-as.POSIXct(paste0(M2data$Date," ",M2data$Time,":00"),"%m/%d/%Y %H:%M:%S",
                             origin =   "1900-01-01 00:00:00",
                             tz = "GMT")
    
    # open a "region" or strata specific nc file
    fl         <- file.path(main,Rdata_path,sim,"Level2",
                            paste0(sim,var_use,IDin,".Rdata"))

    # assign the simulation to download
    sim        <- "B10K-K20_CORECFS" 
    
    # Let's sample the model years as close to Aug 1 as the model timesteps run:
    #tr          <- c("-08-1 12:00:00 GMT") 
    tr          <- substring(M2data$t,5,20)
    # the full grid is large and takes a longtime to plot, so let's subsample the grid every 4 cells
   
    # load data from level 2 nc files (grab a coffee, takes a few mins)
    if(!file.exists(file.path(Rdata_path,fl))){
      get_l2(
        ID          = IDin,
        overwrite   = T,
        xi_rangeIN  = 99,
        eta_rangeIN = 62,
        ds_list     = dl[2],  # must be same length as sub_varlist
        trIN        = tr,
        yearsIN     = 2013,
        sub_varlist = list('Surface 5m' = "temp" ),  
        sim_list    = c("B10K-H16_CORECFS","B10K-K20_CORECFS" )  )
    }
    
    # load R data file
    # open a "region" or strata specific nc file
    sim <- "B10K-H16_CORECFS"
    fl         <- file.path(main,Rdata_path,sim,"Level2",
                            paste0(sim,var_use,IDin,".Rdata"))
    load(fl)   # temp
    
    # there are smarter ways to do this;looping because 
    # we don't want to mess it up but this is slow...
    i <-1
    data_long <- data.frame(latitude = as.vector(temp$lat),
                       longitude = as.vector(temp$lon),
                       val = as.vector(temp$val[,,i]),
                       sim  = sim,
                       time = temp$time[i],
                       year = substr( temp$time[i],1,4),stringsAsFactors = F
                       )
    
    for(i in 2:dim(temp$val)[3])
      data_long <- rbind(data_long,
                          data.frame(latitude = as.vector(temp$lat),
                           longitude = as.vector(temp$lon),
                           val = as.vector(temp$val[,,i]),
                           sim  = sim,
                           time = temp$time[i],
                           year = substr( temp$time[i],1,4),stringsAsFactors = F)
                       )
    
    # open a "region" or strata specific nc file
    sim <- "B10K-K20_CORECFS"
    fl2         <- file.path(main,Rdata_path,sim,"Level2",
                            paste0(sim,var_use,IDin,".Rdata"))
    load(fl2)   # temp
    for(i in 1:dim(temp$val)[3])
    data_long <- rbind(data_long,
                          data.frame(latitude = as.vector(temp$lat),
                           longitude = as.vector(temp$lon),
                           val = as.vector(temp$val[,,i]),
                           sim  = sim,
                           time = temp$time[i],
                           year = substr( temp$time[i],1,4),stringsAsFactors = F)
                       )
    
    plotM2_dat        <- M2data%>%dplyr::select(SST = SST..C.,Date = t)
    plotM2_dat$sim    <- factor("Obs",levels=c("Obs","B10K-H16_CORECFS","B10K-K20_CORECFS"))
    plotM2_dat        <- plotM2_dat%>%filter(SST>-99)
    plotroms_dat      <- data_long%>%dplyr::select(SST = val,Date = time,sim)
    plotroms_dat$sim  <- factor(plotroms_dat$sim,levels=c("Obs","B10K-H16_CORECFS","B10K-K20_CORECFS"))
    plotdat           <- rbind(plotM2_dat,plotroms_dat)
   
    p10_hind     <- ggplot(plotdat) +
    geom_line(   aes(x=Date,y=SST,color=sim),alpha=.8)+
    # geom_smooth( aes(x = Date,y = SST,color=sim),
    #             formula = y ~ x, se = T)+
    scale_color_viridis_d(begin=.9,end=.2)+
    ylab(tmp_var$units[1])+
    ggtitle( "Bering M2 Mooring: 2013 SST")+
    theme_minimal()
   
    # This is slow but it works (repeat dev.new() twice if in Rstudio)...
    dev.new()
    p10_hind
    
    if(update.figs)  
    ggsave(file=file.path(main,"Figs/Hind_M2_SST.jpg"),width=8,height=4)
  
    # graphics.off()
    
#### 6 Projections #### 
    
    