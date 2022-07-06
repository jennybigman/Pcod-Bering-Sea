# sediment grain size raster

pacman::p_load(terra, here)

sed <- rast("./EFH Sediments Share_Jodi/EBS_phi_1km.grd")

plot(sed)


# large phi indicates fine grain size

# sediment grain size of currently good spawning habitat

# try to figure out depth of this

# look at distribution of phi for current vs future suitable habitat and see if very different 

sed2 <- rast(here("./EFH Sediments Share_Jodi/EBS_phi_1km.gri"))
