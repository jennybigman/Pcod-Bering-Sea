library(tidyverse)
library(rnaturalearth)
library(sf)
library(raster)
library(dplyr)

sf_use_s2(FALSE)


####Applying dateline solution to Russia####
#Import map data
world <- ne_countries(scale = "medium",
                      returnclass = "sf") 

#I use the Alaska albers projection for the map,
#limit extent (https://spatialreference.org/ref/epsg/nad83-alaska-albers/)
xmin <- -2255938
xmax <- 1646517
ymin <- 449981
ymax <- 2676986

#pull out russia (from stack overflow)
world %>% 
  filter(
    str_detect(name_long, 'Russia')
  ) %>% 
  dplyr::select(name_long, geometry) %>% 
  {. ->> russia}
#save as a russia specific coordinate system
russia %>% 
  st_transform(32635) %>% 
  {. ->> russia_32635} %>% 
  st_coordinates %>% 
  as_tibble %>% 
  dplyr::select(X) %>% 
  mutate(
    crs = 'utm_32635'
  ) %>% 
  {. ->> russia_coords_32635}

#define Pacific centered new coordinate system
my_proj <- '+proj=moll +lon_0=177 +lat_0=65 +units=m'

#save russia as that new system
russia_32635<-russia_32635 %>% 
  st_buffer(0) %>% 
  st_transform(crs(my_proj)) %>%
  st_simplify 
#isolate US and Canada for plotting
newland<-world%>%
  filter(name %in% c("Canada",  "United States"))

#plot... look at that! Only 50 lines of code for this.
#Maybe one day there will be a geom_remove_annoying_features()
ggplot()+
  geom_sf(data=newland, color="black", size=1)+
  geom_sf(data=russia_32635, color="black", size=1)+
  coord_sf(crs=3338)+
  xlim(c(xmin,xmax))+ylim(c(ymin,ymax))+
  theme_bw()

####Now to our marine management areas####
ESR <- st_read("../from Jordan Watson and Matt Callahan/Data/Alaska_Marine_Management_Areas.gdb")%>% 
  filter(Area_Type=="Ecosystem Subarea")

ESR2<-ESR %>% 
  st_buffer(0) %>% 
  st_transform(crs=3338) %>%
  st_simplify

ggplot()+
  geom_sf(data=newland, color="black", size=1)+
  geom_sf(data=russia_32635, color="black", size=1)+
  geom_sf(data=ESR2, color="red", size=1)+
  coord_sf(crs=3338)+
  xlim(c(xmin,xmax))+ylim(c(ymin,ymax))+
  theme_bw()
