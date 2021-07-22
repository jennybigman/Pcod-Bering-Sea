# Create figure for pop-up proposal using bottom temperatures from ROMS from Al for Mar 
# and Apr.

# for Jenny: setwd("~/Google Drive/NOAA AFSC Postdoc/Pcod Bering Sea Habitat Suitability/")

library("ncdf4");library("tidyr");library("abind");library("dplyr");library("lattice");
library(mapproj); library(rgdal);library(maps);library(mapdata);library(RColorBrewer);
library(PBSmapping) # From which we'll use the clipPolys function
library(marmap);library(tidyverse);library(maptools)

boundaries <- maps::map('worldHires', fill=TRUE,
                        xlim=c(-230,-150), ylim=c(50,70),plot=T)#FALSE)#
boundaries2 <- maps::map('world2Hires',regions=c("USA","USSR"), fill=TRUE,
                         ylim=c(53,66),xlim=c(171,215),plot=T)#FALSE) 
IDs <- sapply(strsplit(boundaries2$names, ":"), function(x) x[1])
bPols <- map2SpatialPolygons(boundaries2, IDs=IDs,
                             proj4string=CRS("+proj=longlat +datum=WGS84"))


# eggs<-read.csv("~/Projects/Pcod/SpawningHabitat_EggSurvival/Pcod and WP hatch 
#probabilities.csv")
eggs<-read.csv("./data and code from Laurel & Rogers 2020 CJFAS/laurelandrogerscode/Pcod and WP hatch probabilities.csv")
eggs<-eggs[,1:8]
eggs$Ntot<-eggs$Hatched+eggs$Dead
eggs$Phatch<-eggs$Hatched/(eggs$Hatched+eggs$Dead)

eggsP<-eggs[eggs$Species=="WP",] # walleye pollock
eggs<-eggs[eggs$Species=="Pcod",] # pacific cod

eggsP$pMax<-eggsP$Phatch/max(eggsP$Phatch)
eggs$pMax<-eggs$Phatch/max(eggs$Phatch)

#### where do these equations come from ####

#Gaussian
#res<- nls( Phatch ~ k*exp(-1/2*(Temp_act-mu)^2/sigma^2), start=c(mu=5,sigma=2,k=1) , data = eggs,weights=Ntot)
#v <- summary(res)$parameters[,"Estimate"]

#Lorentzian/Couchy
res2 <- nls( Phatch ~ k/(1+((Temp_act-mu)/sigma)^2), start=c(mu=5,sigma=2,k=1) ,
             data = eggs, weights=Ntot)
v2 <- summary(res2)$parameters[,"Estimate"]

#preds<-predict(res2,newdata=data.frame("Temp_act"=interpYr$z)) #Cauchy

# mar <- nc_open("bot_temp_latlon_mar.nc") 
# apr <- nc_open("bot_temp_latlon_apr.nc")
mar <- nc_open("./Pcod-Bering-Sea/prelim plots from NPRB proposal -- Rogers/bot_temp_latlon_mar.nc") # for Jenny
apr <- nc_open("./Pcod-Bering-Sea/prelim plots from NPRB proposal -- Rogers/bot_temp_latlon_apr.nc") # for Jenny

# give the data a check and this will show you the structure and units
print(mar)

#### go over this part #### why not doing everything for both mar and apr ####

lons360<-ncvar_get(mar,"LONGITUDE")
lons<-lons360
lons[lons360>180]<-lons[lons360>180]-360 # anything > 180 subtract 360?
lats<-ncvar_get(mar,"LATITUDE") # extract lats into an array of 1d
depth<-ncvar_get(mar,"S_RHO1_1") # extract depth into array of 1d -- why is depth negative? 
tunits <- ncatt_get(mar, "TNOW", "units")  # ?
ttime <- ncvar_get(mar, "TNOW") # time -- hours since 1091-01-15
marT<-ncvar_get(mar,"SBTMO") # array of long, lat, and year for march
aprT<-ncvar_get(apr,"SBTMO")  # array of long, lat, and year for april

# so marT/aprT are temp data for the 251 degrees long, 161 degrees lat, for 49 years in the lons/lats/years?
# why are some NA?

dim(marT) #shows 49 in the time dimension, 1970 - 2018

nc_close(mar)
nc_close(apr)

#image(Ts[,,42])
Yrs<-1970:2018

#preds<-predict(res2,newdata=data.frame("Temp_act"=interpYr$z)) #Cauchy
MarHatch<-v2[3]/(1+((marT-v2[1])/v2[2])^2) # so now these are hatch probabilities at the lon/lat in a given year in march? 
AprHatch<-v2[3]/(1+((aprT-v2[1])/v2[2])^2)

# what are these 
MarHatchSim_p1<-v2[3]/(1+((marT+1-v2[1])/v2[2])^2) # why add 1 here and subtract1 on next line 
MarHatchSim_m1<-v2[3]/(1+((marT-1-v2[1])/v2[2])^2) 

dimnames(AprHatch)[[1]]<-lons
dimnames(AprHatch)[[2]]<-lats
dimnames(AprHatch)[[3]]<-dimnames(MarHatch)[[3]]<-Yrs

HatchArr<-abind(MarHatch,AprHatch,along=4)
dimnames(HatchArr)[[1]]<-lons
dimnames(HatchArr)[[2]]<-lats
dimnames(HatchArr)[[3]]<-Yrs
dimnames(HatchArr)[[4]]<-c("March","April")
TArr<-abind(marT,aprT,along=4) # so this is just temp, right?
dimnames(TArr)[[3]]<-Yrs
dimnames(TArr)[[4]]<-c("March","April")

# why doing this ?
HatchArrSim<-abind(MarHatchSim_m1,MarHatchSim_p1,along=4)
dimnames(HatchArrSim)[[1]]<-lons
dimnames(HatchArrSim)[[2]]<-lats
dimnames(HatchArrSim)[[3]]<-Yrs
dimnames(HatchArrSim)[[4]]<-c("minus_1C","plus_1C")



mypal<-colorRampPalette(brewer.pal(10,"Spectral")[10:1])

mybathy<-getNOAA.bathy(lon1 = min(lons360)-0.1, lon2 = max(lons360)-360+0.1, 
                       lat1 = min(lats)-0.1, lat2 = max(lats)+0.1,
                       resolution = 4,keep=T,antimeridian=T)

mydepth<-get.depth(mybathy,lons360[1],lats[1],locator=FALSE) 

print(levelplot(MarHatch[,,c(43,49)],xlab="Lon",ylab="Lat",col.regions=mypal(100),as.table=F)) # 43 and 49 are 2012 and 2018, respectively
print(levelplot(AprHatch[,,c(43,49)],xlab="Lon",ylab="Lat",col.regions=mypal(100),as.table=F))

LLs<- expand.grid(lons360, lats) %>%
  rename(lon = Var1, lat = Var2) %>%
  mutate(depth = get.depth(mybathy,lon,lat,locator=FALSE)[,"depth"]) 

# the above didn't work for me b/c lat/lon are 1d arrays but need to be vectors, so run
# code below

LLs<- expand.grid(lons360, lats) %>%
  rename(lon = Var1, lat = Var2) 

LLs$lon <- as.vector(LLs$lon)
LLs$lat <- as.vector(LLs$lat)

LLs <- LLs %>%
       mutate(depth = get.depth(mybathy,lon,lat,locator=FALSE)[,"depth"]) 

#########

HatchArr2<-HatchArr
HatchArr2[,,,][LLs$depth < -250]<- NA
HatchArrSim[,,,][LLs$depth < -250]<- NA

c(43,49)
png("ROMS_HatchSuccess.png",res=300,units="in",width=8,height=8)
print(levelplot(HatchArr2[,,c(43,49),],xlab="Longitude",ylab="Latitude",col.regions=mypal(100),as.table=T,
      scales=list(x=list(cex=1,at=seq(21,231,by=40),labels=lons360[seq(21,231,by=40)],alternating=F),
            y=list(cex=1,at=seq(1,161,by=40),labels=lats[seq(1,161,by=40)]),alternating=F),
      par.settings=list(strip.background=list(col=c("gray90","gray70"))))
)
dev.off()

png("ROMS_BottomTemps.png",res=300,units="in",width=8,height=8)
print(levelplot(TArr[,,c(43,49),],xlab="Lon",ylab="Lat",col.regions=mypal(100),as.table=T,
      scales=list(x=list(cex=1,at=seq(1,251,by=30),labels=lons[seq(1,251,by=30)]),
            y=list(cex=1,at=seq(1,161,by=20),labels=lats[seq(1,161,by=20)])))
)
dev.off()


png("ROMS_HatchSuccess_Moreyears.png",res=300,units="in",width=8,height=8)
print(levelplot(HatchArr2[,,41:49,1],xlab="Longitude",ylab="Latitude",col.regions=mypal(100),as.table=T,
                scales=list(x=list(cex=1,at=seq(21,231,by=40),labels=lons360[seq(21,231,by=40)],alternating=F),
                            y=list(cex=1,at=seq(1,161,by=40),labels=lats[seq(1,161,by=40)]),alternating=F),
                par.settings=list(strip.background=list(col=c("gray90","gray70"))))
)
dev.off()


png("ROMS_HatchSuccess_March.png",res=300,units="in",width=8,height=8)
print(levelplot(HatchArr2[,,c(43,49),1],xlab="Longitude",ylab="Latitude",col.regions=mypal(100),as.table=T,
                scales=list(x=list(cex=1,at=seq(21,231,by=40),labels=lons360[seq(21,231,by=40)],alternating=F),
                            y=list(cex=1,at=seq(1,161,by=40),labels=lats[seq(1,161,by=40)]),alternating=F),
                par.settings=list(strip.background=list(col=c("gray90","gray70"))))
)
dev.off()

# HatchArrSim[HatchArrSim<0.25]<- NA
# 
# 
# print(levelplot(HatchArrSim[81:229,38:161,49,],xlab="Longitude",ylab="Latitude",col.regions=mypal(100),as.table=T,
#                 scales=list(x=list(cex=1,at=seq(1+10,149+10,by=40),labels=lons360[seq(81+10,229+10,by=40)],alternating=F),
#                             y=list(cex=1,at=seq(1+3,134,by=40),labels=lats[seq(38+3,161,by=40)]),alternating=F),
#                 par.settings=list(strip.background=list(col=c("gray90","gray70")))) 
#  #                 +  latticeExtra::layer(sp.polygons(bPols,fill="gray30",lwd=0.5)))
# )
# 
# #print(levelplot(HatchArr2[81:229,38:161,43,],xlab="Longitude",ylab="Latitude",col.regions=mypal(100),
#              ,as.table=T,
#               scales=list(x=list(cex=1,at=seq(1+10,149+10,by=40),labels=lons360[seq(81+10,229+10,by=40)],alternating=F),
#                             y=list(cex=1,at=seq(1+3,134,by=40),labels=lats[seq(38+3,161,by=40)]),alternating=F),
#                 par.settings=list(strip.background=list(col=c("gray90","gray70")),panel.background=list(col="gray80")))
# #                  + latticeExtra::layer(sp.polygons(bPols,fill="gray30",lwd=0.5))) #problem is Hatch data not in lat/lon
# )

############## Turn into dataframe.

HatchDF<-as.data.frame.table(HatchArr2)
HatchDF$Var1<-as.numeric(levels(HatchDF$Var1))[HatchDF$Var1]
HatchDF$Var2<-as.numeric(levels(HatchDF$Var2))[HatchDF$Var2]

colnames(HatchDF)<-c("Lon","Lat","Year","Month","phatch")
HatchDF$Lon360<-HatchDF$Lon
HatchDF$Lon360[HatchDF$Lon360<0]<-HatchDF$Lon360[HatchDF$Lon360<0]+360


HatchSim<-as.data.frame.table(HatchArrSim)
HatchSim$Var1<-as.numeric(levels(HatchSim$Var1))[HatchSim$Var1]
HatchSim$Var2<-as.numeric(levels(HatchSim$Var2))[HatchSim$Var2]

colnames(HatchSim)<-c("Lon","Lat","Year","Simulation","phatch")
HatchSim$Lon360<-HatchSim$Lon
HatchSim$Lon360[HatchSim$Lon360<0]<-HatchSim$Lon360[HatchSim$Lon360<0]+360

#######THIS CODE WORKS#################

myinds<-which(HatchDF$Year %in% c(2012,2018) & HatchDF$Lon360>=179 
              & HatchDF$Lon360<=205 & HatchDF$Lat>=54 & HatchDF$Month =="March" )

png("ROMS_HatchSuccess_2012_2018.png",res=300,units="in",width=7,height=4)

print(levelplot(phatch~Lon360*Lat|Year*Month,data=HatchDF[myinds,],xlab="Longitude E",ylab="Latitude N",col.regions=mypal(100),
                at=seq(0,0.5,length.out=100),as.table=T, 
                scales=list(x=list(alternating=FALSE,at=seq(182,202,by=6)),
                            y=list(alternating=FALSE,at=seq(55,65,by=2))),
                par.settings=list(strip.background=list(col=c("gray90","gray70")),panel.background=list(col="white")))
                        + latticeExtra::layer(sp.polygons(bPols,fill="gray30",lwd=0.5))) #problem is Hatch data not in lat/lon
dev.off()

myinds2<-which(HatchSim$Year %in% c(2018) & HatchSim$Lon360>=179 
              & HatchSim$Lon360<=205 & HatchSim$Lat>=54  )

HatchSim$phatchcat<-HatchSim$phatch>0.25

png("ROMS_HatchSuccess_2018_ErrorSimulation_v2.png",res=300,units="in",width=7,height=4)

print(levelplot(phatch~Lon360*Lat|Simulation*Year,data=HatchSim[myinds2,],xlab="Longitude E",ylab="Latitude N",col.regions=mypal(100),
                at=seq(0,0.5,length.out=100),as.table=TRUE, 
                scales=list(x=list(alternating=FALSE,at=seq(182,202,by=6)),
                            y=list(alternating=FALSE,at=seq(55,65,by=2))),
                par.settings=list(strip.background=list(col=c("gray90","gray70")),panel.background=list(col="white")))
      + latticeExtra::layer(sp.polygons(bPols,fill="gray30",lwd=0.5))
      + contourplot(phatch~Lon360*Lat|Simulation,data=HatchSim[myinds2,],labels=FALSE,at=c(0.3,0.5),lwd=2))
dev.off()



print(levelplot(phatchcat~Lon360*Lat|Simulation*Year,data=HatchSim[myinds2,],xlab="Longitude E",ylab="Latitude N",
                col.regions=c(0,"red3"),alpha.regions=0.8,colorkey=FALSE,as.table=T, 
                scales=list(x=list(alternating=FALSE,at=seq(182,202,by=6)),
                            y=list(alternating=FALSE,at=seq(55,65,by=2))),
                par.settings=list(strip.background=list(col=c("gray90","gray70")),panel.background=list(col="white")))
      + latticeExtra::layer(sp.polygons(bPols,fill="gray30",lwd=0.5)))
       


##########
# OLD CODE...........
###########


#WOuld like dataframe with Lat,Lon, Temp, Hatch, Month, Year

#w2hr <- map_data("world2Hires")
#names(w2hr) <- c("X","Y","PID","POS","region","subregion")
#myworld <- clipPolys(w2hr, xlim=c(min(lons),max(lons)),ylim=c(min(lats),max(lats)), keepExtra=TRUE)

LL<- expand.grid(lons360, lats) %>%
      rename(lon = Var1, lat = Var2) %>%
  mutate(marT = as.vector(marT[,,43])) %>% 
  mutate(depth = get.depth(mybathy,lon,lat,locator=FALSE)[,"depth"]) %>%
  filter(depth > -250)

ggplot(LL,aes(x = lon, y = lat)) + 
  #  geom_point(aes(x = lon, y = lat), size = 0.8) + 
  geom_raster(aes(fill=marT)) +
  scale_fill_viridis_c(name = "Temperature (C)") + 
  theme_void() + 
  coord_quickmap() + 
  ggtitle("Mytitel", subtitle = "Mysubtitle") 

  
  
png("test.png")
expand.grid(lons, lats) %>%
  rename(lon = Var1, lat = Var2) %>%
  mutate(marT = as.vector(marT[,,43])) %>% 
  ggplot(aes(x = lon, y = lat)) + 
#  geom_point(aes(x = lon, y = lat), size = 0.8) + 
  geom_raster(aes(fill=marT)) +
#  geom_polygon(data=myworld,
#               aes(x=X,y=Y,group=factor(PID)),fill=NA,color="red") + 
#  geom_polygon(data = w2hr, aes(x=long, y = lat, group = group), fill = NA, color = "red") +
#  borders("world", colour="black", fill=NA) + 
  scale_fill_viridis_c(name = "Temperature (C)") + 
  theme_void() + 
  coord_quickmap() + 
  ggtitle("Mytitel",
          subtitle = "Mysubtitle") 
dev.off()

#Messing around with how to plot NAs at -0.1 and adjust colorkey - didn't work.
print(levelplot(HatchArr2[,,1,],xlab="Longitude",ylab="Latitude",at=seq(-0.1,0.5,0.05),col.regions=c("gray90",mypal(12)),as.table=T,
                scales=list(x=list(cex=1,at=seq(21,231,by=40),labels=lons360[seq(21,231,by=40)],alternating=F),
                            y=list(cex=1,at=seq(1,161,by=40),labels=lats[seq(1,161,by=40)]),alternating=F),
                par.settings=list(strip.background=list(col=c("gray90","gray70")),
                                  colorkey=list(col=mypal(12)[2:12],at=seq(-.025,0.55,by=0.05))))
)

abc<-as.data.frame.table(HatchArr)
