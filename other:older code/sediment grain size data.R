# sediment grain size raster

pacman::p_load(terra, here)

sed <- rast("./data/EFH Sediments Share_Jodi/EBS_phi_1km.grd")

plot(sed)

