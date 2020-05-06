context("nsink_prep_data")
library(nsink)
library(sf)
load(system.file("testdata.rda", package="nsink"))
 aea <- "ESRI:102003"
removal <- nsink_calc_removal(niantic_nsink_data)
pt <- c(1948121, 2295822)
start_loc <- st_sf(st_sfc(st_point(c(pt)), crs = aea))
fp <- nsink_generate_flowpath(start_loc, niantic_nsink_data)
static_maps <- nsink_generate_static_maps(niantic_nsink_data, removal,fact = 1500)

test_that("static maps are generated correctly", {
  expect_setequal(names(static_maps), c("removal_effic", "loading_idx",
                                         "transport_effic", "delivery_idx"))
})
