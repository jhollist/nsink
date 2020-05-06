context("nsink_prep_data")
library(nsink)
library(sf)
load(system.file("testdata.rda", package="nsink"))
aea <- "ESRI:102003"
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
