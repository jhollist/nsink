context("nsink_prep_data")
library(nsink)
library(sf)
load(system.file("testdata.rda", package="nsink"))
 aea <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0
 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
removal <- nsink_calc_removal(niantic_nsink_data)
pt <- c(1948121, 2295822)
start_loc <- st_sf(st_sfc(st_point(c(pt)), crs = aea))
fp <- nsink_generate_flowpath(start_loc, niantic_nsink_data)
static_maps <- nsink_generate_static_maps(niantic_nsink_data, removal,fact = 1500)

test_that("static maps are generated correctly", {
  expect_setequal(names(static_maps), c("removal_effic", "loading_idx",
                                         "transport_effic", "delivery_idx"))
})
