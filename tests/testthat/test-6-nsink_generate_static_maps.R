
context("nsink_generate_static_maps")
library(nsink)
library(sf)
load(system.file("testdata.rda", package="nsink"))
aea <- "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

pt <- c(1948121, 2295822)
start_loc <- st_sf(st_sfc(st_point(c(pt)), crs = aea))
fp <- nsink_generate_flowpath(start_loc, niantic_nsink_data)
static_maps <- nsink_generate_static_maps(niantic_nsink_data, removal,samp_dens = 3000)

test_that("static maps are generated correctly", {
  expect_setequal(names(static_maps), c("removal_effic", "loading_idx",
                                         "transport_idx", "delivery_idx"))
})

test_that("static maps have positive values", {
  expect_gte(min(raster::values(static_maps$removal_effic), na.rm = TRUE), 0)
  expect_gte(min(raster::values(static_maps$loading_idx), na.rm = TRUE), 0)
  expect_gte(min(raster::values(static_maps$transport_idx), na.rm = TRUE), 0)
  expect_gte(min(raster::values(static_maps$delivery_idx), na.rm = TRUE), 0)
})
