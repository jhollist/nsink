context("nsink_load")

test_that("load runs as expected", {
  skip_on_ci()
  library(nsink)
  library(sf)
  aea <- "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
  nsink_load("nsink_test_data", "tested_", aea)
  expect_setequal(names(tested_data), c("streams", "lakes", "fdr", "impervious",
                                        "nlcd", "ssurgo","q", "tot",
                                        "lakemorpho", "huc", "raster_template"))
  expect_setequal(names(tested_removal), c("raster_method",
                                           "land_off_network_removal",
                                           "land_off_network_removal_type",
                                           "network_removal"))
  expect_setequal(names(tested_static_maps), c("removal_effic", "loading_idx",
                                               "transport_idx", "delivery_idx"))


})

unlink("nsink_test_data", recursive = TRUE, force = TRUE)
