# 01 - download weekly-averaged bottom temp data for all years from Bering10K ROMS model

		#### load data resulting from below code
    library(data.table)
    library(tidyverse)   
    setwd("~/Google Drive/NOAA AFSC Postdoc/Pcod Bering Sea Habitat Suitability")
		#all_temp_dat <- fread( "./data/all_temp_dat.csv")
    
		#### Setup workspace ---------------------------------------------------------------
		setwd("~/Google Drive/NOAA AFSC Postdoc/Pcod Bering Sea Habitat Suitability/ACLIM2-main")
    
		library(devtools)
		library(ncdf4)
		library(thredds)
		library(lubridate)
		library(stringr)

    # rm(list=ls())
    tmstp  <- format(Sys.time(), "%Y_%m_%d")
    main   <- getwd()  #"~/GitHub_new/ACLIM2
    source("R/make.R")

    
	#### load and transform data ####
    library(lubridate)
	
	# load list of files from Bering 10K
	
	  setwd("~/Google Drive/NOAA AFSC Postdoc/Pcod Bering Sea Habitat Suitability/ACLIM2-main")
    main   <- getwd()  #"~/GitHub_new/ACLIM2
    source("R/make.R")
    source("R/sub_scripts/load_maps.R") 
    
    #################
    # Buck's CRS CODE ####
    #################
    
    #'
#' @title Get coordinate reference system representation as \code{sf::crs} object
#'
#' @description Function to get coordinate reference system representation as \code{sf::crs} object
#'
#' @param x - see details
#'
#' @return object of class \code{sf::crs} or NULL if \code{x} is NULL
#'
#' @details \code{x} can be NULL, NA, numeric, character,
#' inherit from sf classes crs, sf, sfc, sfg, or bbox, or
#' inherit from sp classes CRS or Spatial.
#'
#' If \code{x} is numeric, it should represent an EPSG code.
#'
#' If \code{x} is character, it should be a valid PROJ4 string or one of the following:
#' "longlat","latlong","WGS84","NAD83","NAD27", "AlaskaAlbers", "utmXX" or "utmXXs". In the
#' latter two cases, XX should be a valid utm zone.
#'
#' Based on deprecated function \code{tmaptools::get_proj4} by M. Tennekes
#' (see https://github.com/mtennekes/oldtmaptools/blob/master/R/get_proj4_code.R).
#'
#' @importFrom sf st_crs
#'
#' @export
#'
get_crs <- function(x) {
	if (is.null(x)) {
		return(NULL);
	} else if (inherits(x, "crs")) {
	    return(x);
	} else if (inherits(x,c("sf","sfc","sfg","bbox"))){
	    return(sf::st_crs(x));
	} else if (inherits(x, "CRS")) {
	    return(sf::st_crs(attr(x, "projargs")));
	} else if (inherits(x, "Spatial")) {
	    return(sf::st_crs(sp::proj4string(x)));
	} else if (suppressWarnings(is.na(x))) {
		return(sf::st_crs());
	} else if (!is.numeric(x) && !is.character(x)) {
		stop("x is not character, numeric, or a crs, sf, sfc, sfg, CRS, or Spatial object", call.=FALSE)
	} else {
		if (toupper(x) %in% names(.proj_epsg)) {
		    return(create_crs(unname(.proj_epsg[toupper(x)])));
		} else if (is_num_string(x)) {
		    return(sf::st_crs(x));
		} else if (substr(x, 1, 3)=="utm") {
		    if (!(nchar(x) %in% c(5,6))) stop("\"utm\" shortcut code should be utmXX or utmXXs where XX refers to the utm zone")
			y<-sf::st_crs(paste("+proj=utm +zone=", substr(x, 4, 5), ifelse(substr(x, 6, 6)=="s", " +south", ""), " +ellps=WGS84 +datum=WGS84 +units=m +no_defs +towgs84=0,0,0", sep=""));
			return(y);
		} else {
			return(sf::st_crs(x));
		}
	}
}

#'
#' @title Get coordinate reference system representation as a PROJ4 string
#'
#' @description Function to get coordinate reference system representation as a PROJ4 string
#'
#' @param x - see details
#'
#' @return a PROJ4 string or NULL if \code{x} is NULL
#'
#' @details \code{x} can be NULL, NA, numeric, character,
#' inherit from sf classes crs, sf, sfc, or sfg, or
#' inherit from sp classes CRS or Spatial.
#'
#' If \code{x} is numeric, it should represent an EPSG code.
#'
#' If \code{x} is character, it should be a valid PROJ4 string or one of the following:
#' "longlat","latlong","WGS84","NAD83","NAD27", "AlaskaAlbers", "utmXX" or "utmXXs". In the
#' latter two cases, XX should be a valid utm zone.
#'
#' Based on deprecated function \code{tmaptools::get_proj4} by M. Tennekes
#' (see https://github.com/mtennekes/oldtmaptools/blob/master/R/get_proj4_code.R).
#'
#' @export
#'
get_proj4string <- function(x) {
	y <- get_crs(x);
	if (is.null(y)) return(NULL);
	return(y$proj4string);
}

#'
#' @title Get coordinate reference system representation as an EPSG number
#'
#' @description Function to get coordinate reference system representation as a, EPSG number
#'
#' @param x - see details
#'
#' @return an EPSG number or NULL if \code{x} is NULL
#'
#' @details \code{x} can be NULL, NA, numeric, character,
#' inherit from sf classes crs, sf, sfc, or sfg, or
#' inherit from sp classes CRS or Spatial.
#'
#' If \code{x} is numeric, it should represent an EPSG code.
#'
#' If \code{x} is character, it should be a valid PROJ4 string or one of the following:
#' "longlat","latlong","WGS84","NAD83","NAD27", "AlaskaAlbers", "utmXX" or "utmXXs". In the
#' latter two cases, XX should be a valid utm zone.
#'
#' Based on deprecated function \code{tmaptools::get_proj4} by M. Tennekes
#' (see https://github.com/mtennekes/oldtmaptools/blob/master/R/get_proj4_code.R).
#'
#' @export
#'
get_epsg <- function(x) {
	y <- get_crs(x);
	if (is.null(y)) return(NULL);
	return(y$epsg);
}

#'
#' @title Get coordinate reference system representation as sp::CRS object
#'
#' @description Function to get coordinate reference system representation as a sp::CRS object
#'
#' @param x - see details
#'
#' @return a sp::CRS object or NULL if \code{x} is NULL
#'
#' @details \code{x} can be NULL, NA, numeric, character,
#' inherit from sf classes crs, sf, sfc, or sfg, or
#' inherit from sp classes CRS or Spatial.
#'
#' If \code{x} is numeric, it should represent an EPSG code.
#'
#' If \code{x} is character, it should be a valid PROJ4 string or one of the following:
#' "longlat","latlong","WGS84","NAD83","NAD27", "AlaskaAlbers", "utmXX" or "utmXXs". In the
#' latter two cases, XX should be a valid utm zone.
#'
#' Based on deprecated function \code{tmaptools::get_proj4} by M. Tennekes
#' (see https://github.com/mtennekes/oldtmaptools/blob/master/R/get_proj4_code.R).
#'
#' @export
#'
get_spCRS <- function(x) {
	y <- get_crs(x);
	if (is.null(y)) return(NULL);
	return(sp::CRS(ifelse(is.na(y$proj4string), "", y$proj4string)));
}

create_crs <- function(x) {
    if (is.numeric(x)) {
        sf::st_crs(x);
    } else {
        structure(list(epsg = as.integer(NA), proj4string = x), class = "crs");
    }
}

is_num_string <- function(x) {
    suppressWarnings((length(x)==1)&&(!is.na(as.numeric(x[1]))));
}

.proj_epsg <- c(LONGLAT = 4326,
                LATLONG = 4326,
                WGS84   = 4326,
                NAD83   = 4269,
                NAD27   = 4267,
                ALASKAALBERS = 3338);

 
    #################
    # Buck's SFC CODE ####
    #################

#'
#' @title Create an \pkg{sf} (simple features) column with point geometries from x,y coordinate vectors
#'
#' @description Function to create an \pkg{sf} (simple features) column with point geometries (i.e., an sfc_POINT object) from x,y coordinate vectors.
#'
#' @param x - vector of x coordinates
#' @param y - vector of y coordinates
#' @param crs - coordinate reference system (convertible to an sf crs by \code{\link{get_crs}})
#' @param wrapDateline - flag (T/F) to use 0 to 360 rather than -180 to 180 range for longitudes
#'
#' @return an object of class sfc_POINT
#'
#' @details Uses package \pkg{sf}.
#'
#' @importFrom sf st_point
#' @importFrom sf st_sfc
#'
#' @export
#'
createSFC_points<-function(x,y,crs=sf::NA_crs_,wrapDateline=FALSE){
  #--create point geometries
  nr<-length(x);
  if (wrapDateline) x = ifelse(x<0,360+x,x);
  geoms<-vector(length=nr,mode="list");
  for (rw in 1:nr){
    geoms[[rw]] <- sf::st_point(x=c(x[rw],y[rw]),
                                dim="XY");
  }
  if (!inherits(crs,"crs")) crs = get_crs(crs);#--convert to sf::crs, if possible
  sfc_geoms<-sf::st_sfc(geoms,crs=crs);
  return(sfc_geoms);
}

 #################
    # Buck's SF CODE ####
 #################

#'
#' @title Create an \pkg{sf} (simple features) dataset with point geometries
#'
#' @description Function to create an \pkg{sf} (simple features) dataset with point geometries.
#'
#' @param dfr - dataframe with columns for x,y coordinates for points
#' @param xCol - name of column with x coordinates (e.g., longitudes)
#' @param yCol - name of column with y coordinates (e.g., latitudes)
#' @param crs - a coordinate reference system object convertible to a \code{sf::crs} object using \code{\link{get_crs}}
#' @param wrapDateline - flag (T/F) to use 0 to 360 rather than -180 to 180 range for longitudes
#'
#' @return an \pkg{sf} dataframe: dfr with "geom" column appended
#'
#' @details Uses packages \code{tibble}, \code{sf}, \code{dplyr}.
#'
#' @importFrom dplyr bind_cols
#' @importFrom sf st_sf
#' @importFrom tibble as_tibble
#'
#' @export
#'
createSF_points<-function(dfr,
                          xCol="longitude",
                          yCol="latitude",
                          crs=get_crs(4326),
                          wrapDateline=FALSE){
  #--create point geometries
  #----don't define crs at this point
  geoms<-createSFC_points(dfr[[xCol]],dfr[[yCol]],wrapDateline=wrapDateline);

  #--add point geometries to dfr as column "geom"
  #----define crs now
  dfr <- dplyr::bind_cols(tibble::as_tibble(dfr),
                          sf::st_sf(geom=geoms,
                                    crs=crs));

  #--convert to sf object
  sf_dfr <- sf::st_sf(dfr);

  return(sf_dfr)
}


