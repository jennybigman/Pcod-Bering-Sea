# accessing the grid file to get areas

	# libraries
  library(ncdf4)
  library(thredds)   
  library(reshape2)

  # PMEL thredds server (for all available data)
  url_base <- "https://data.pmel.noaa.gov/aclim/thredds/"

  # List available runs for whole server
  tds_list_datasets(url_base)

  # Dataset address for hindcast Level2 data
  dataset  <- "catalog/extended_grid/catalog.html"
 
  # List available data for Level2 hindcast
  tds_list_datasets(paste(url_base,dataset,sep=""))
  
  # Opendap address for bottom 5 meter layer
  opendap  <- "dodsC/extended_grid/Bering10K_extended_grid.nc"

  # Open ncdf4 connection with dataset
  nc_handle <- nc_open(paste(url_base,opendap,sep="")) 

  # Show metadata from this dataset
  nc_handle
  
  
  ######
  
url_base <- "https://data.pmel.noaa.gov/aclim/thredds/"
opendap  <- "dodsC/extended_grid/Bering10K_extended_grid.nc"

nc <- nc_open(paste(url_base,opendap,sep=""))

# Make vectors of each axis.
xi_axis  <- seq(1,182) # Hardcoded axis length, ROMS coordinates
eta_axis <- seq(1,258) # Hardcoded axis length, ROMS coordinates

lats <- ncvar_get(nc,"lat_rho")
lons <- ncvar_get(nc,"lon_rho")

area_array<- ncvar_get(nc,"area_feast")

dim(area_array)

dimnames(area_array)<-list("Xi"=xi_axis,"Eta"=eta_axis)

area_df <-
  melt(area_array, varnames=c("Xi", "Eta"), value.name="area_km2")

area_df$Lon <- lons[cbind(area_df$Xi,area_df$Eta)]
area_df$Lat <- lats[cbind(area_df$Xi,area_df$Eta)]


fwrite(temp_df, "./data/ROMS_area_grid_cells.csv")
