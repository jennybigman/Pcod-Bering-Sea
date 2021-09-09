#LR code to access NetCDF files without ACLIM functions, create dataframe.

library(lattice)
library(devtools)
library(ncdf4)
library(thredds)
library(lubridate)
library(stringr)
library(dplyr)
library(data.table)

url_base <- "https://data.pmel.noaa.gov/aclim/thredds/"
opendap  <- "dodsC/Level2/B10K-K20_CORECFS_bottom5m.nc"

nc <- nc_open(paste(url_base,opendap,sep=""))

# Make vectors of each axis.
xi_axis  <- seq(1,182) # Hardcoded axis length, ROMS coordinates
eta_axis <- seq(1,258) # Hardcoded axis length, ROMS coordinates

lats <- ncvar_get(nc,"lat_rho")
lons <- ncvar_get(nc,"lon_rho")

t_axis   <- ncvar_get(nc,"ocean_time")
time_axis <- as.POSIXct(t_axis, origin = "1900-01-01", tz = "GMT") #all of the time slices in the weekly level 2 files

#bottom 5m temperature data is in "temp", essentially xi x eta x time

#Can extract time slices or time-series at a given grid point, or all data:

#I don't know why but trying to extract all data using the following throws an error.
# testtemp <- ncvar_get(nc,"temp")

#But it works if we just leave out the last two time steps count=c(182,258,2662) instead of count=c(182,258,2664)
temp_array<- ncvar_get(nc,"temp",start=c(1,1,1),count=c(182,258,2662)) 


dim(temp_array) #x , y , z (time), associated with xi, eta, time-axis values

image(temp_array[,,1]) #quickly look at first time-slice
#or
levelplot(temp_array[,,1])

#plot timeslice 3 (at time_axis[3]) on lat/lon grid instead of xi, eta:
plot(lons,lats,col=ifelse(temp_array[,,3]<2,"blue","green"),pch=".")

plot(temp_array[100,120,],type="l") #time-series at one lon/lat point
#point would be at lons[100,120] and lats[100,120]


dimnames(temp_array)<-list("Xi"=xi_axis,"Eta"=eta_axis,"Time"=t_axis[1:2662])

# Transform into a dataframe from 3D array
library(reshape2)
temp_df <-
  melt(temp_array, varnames=c("Xi", "Eta", "Time"), value.name="temp")

#now need to translate to lat/lon and time:

temp_df$DateTime<- as.POSIXct(temp_df$Time, origin = "1900-01-01", tz = "GMT")
temp_df$Lon <- lons[cbind(temp_df$Xi,temp_df$Eta)]
temp_df$Lat <- lats[cbind(temp_df$Xi,temp_df$Eta)]


#could save this dataframe for later use...


pdf("testplot.pdf") #I think the figure has too many data points,issues with resolution/grid to plot properly. 
print(levelplot(temp~ Lon * Lat,data=temp_df[temp_df$DateTime==time_axis[1],]))
dev.off()

plot(Lat~Lon,data=temp_df[temp_df$DateTime==time_axis[1],])

nc_close(nc) #close the connection with the remote netCDF file.

temp_df <- na.omit(temp_df)

temp_df <- temp_df %>% distinct(across(everything()))

setwd("~/Google Drive/NOAA AFSC Postdoc/Pcod Bering Sea Habitat Suitability")
fwrite(temp_df, "./data/ROMS_all_temp.csv")

# compare to my df

all_temp_dat <- fread( "./data/all_temp_dat.csv")



jb_nrow <- nrow(all_temp_dat)
lr_nrow <- nrow(temp_df)

jb_nrow - lr_nrow
