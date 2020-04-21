context("nsink_prep_data")
library(nsink)
library(sf)
load(system.file("testdata.rda", package="nsink"))
aea <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0
 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
pt <- c(1948121, 2295822)
start_loc <- st_sf(st_sfc(st_point(c(pt)), crs = aea))
start_loc_ll <- st_transform(start_loc, crs = 4326)
fp <- nsink_generate_flowpath(start_loc, niantic_nsink_data)


test_that("flowpath is generated", {
  expect_setequal(names(fp), c("flowpath_ends", "flowpath_network"))
})

test_that("prj check works", {
  expect_error(nsink_generate_flowpath(start_loc_ll,niantic_nsink_data))
})
