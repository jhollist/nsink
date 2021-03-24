library(nsink)
load("inst/testdata.rda")
huc <- nsink_get_huc_id("niantic")$huc_12
niantic_data <- nsink_prep_data(huc, aea, "bug_data")
niantic_removal <- nsink_calc_removal(niantic_data)
niantic_static <- nsink_generate_static_maps(niantic_data, niantic_removal,
                                             samp_dens = 3000)
niantic_static_avg <- mean(raster::values(niantic_static$transport_idx),
                           na.rm = TRUE)

huc <- "011000060405"
horseneck_data <- nsink_prep_data(huc, aea, "bug_data")
horseneck_removal <- nsink_calc_removal(horseneck_data)
horseneck_static <- nsink_generate_static_maps(horseneck_data,
                                               horseneck_removal,
                                               samp_dens = 3000)
horseneck_static_avg <- mean(raster::values(horseneck_static$transport_idx),
                             na.rm = TRUE)

huc <- "010900040908"
low_west_data <- nsink_prep_data(huc, aea, "bug_data")
low_west_removal <- nsink_calc_removal(low_west_data)
low_west_static <- nsink_generate_static_maps(low_west_data,
                                              low_west_removal,
                                               samp_dens = 3000)
low_west_static_avg <- mean(raster::values(low_west_static$transport_idx),
                             na.rm = TRUE)

huc <- "010900040906"
up_west_data <- nsink_prep_data(huc, aea, "bug_data")
up_west_removal <- nsink_calc_removal(up_west_data)
# THIS IS THROWING AN ERROR... START HERE JEFF
up_west_static <- nsink_generate_static_maps(up_west_data,
                                             up_west_removal,
                                               samp_dens = 2000)
up_west_static_avg <- mean(raster::values(up_west_static$transport_idx),
                            na.rm = TRUE)

rm(huc)
rm(niantic_static)
rm(horseneck_static)
rm(low_west_static)
rm(up_west_static)
save(list = ls(), file = "inst/testdata2.rda", compress = "xz")
