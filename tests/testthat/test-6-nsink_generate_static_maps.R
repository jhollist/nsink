context("nsink_generate_static_maps")
library(nsink)
library(sf)
load(system.file("testdata.rda", package="nsink"))
skip_on_ci()

test_that("static maps are generated correctly", {
  niantic_static <- nsink_generate_static_maps(niantic_data, niantic_removal,
                                                    samp_dens = 3000)
  expect_setequal(names(niantic_static), c("removal_effic", "loading_idx",
                                         "transport_idx", "delivery_idx"))
  expect_equal(mean(raster::values(niantic_static$transport_idx), na.rm = TRUE),
               niantic_static_avg, tolerance = 0.001)
  low_west_static <- nsink_generate_static_maps(low_west_data, low_west_removal,
                                                    samp_dens = 3000)
  expect_setequal(names(low_west_static), c("removal_effic", "loading_idx",
                                                "transport_idx", "delivery_idx"))
  expect_equal(mean(raster::values(low_west_static$transport_idx), na.rm = TRUE),
               low_west_static_avg, tolerance = 0.001)
  up_west_static <- nsink_generate_static_maps(up_west_data, up_west_removal,
                                                    samp_dens = 2000)
  expect_setequal(names(up_west_static), c("removal_effic", "loading_idx",
                                                "transport_idx", "delivery_idx"))
  expect_equal(mean(raster::values(up_west_static$transport_idx), na.rm = TRUE),
               up_west_static_avg, tolerance = 0.001)
})

test_that("static maps have positive values", {
  expect_gte(min(raster::values(niantic_static_maps$removal_effic), na.rm = TRUE), 0)
  expect_gte(min(raster::values(niantic_static_maps$loading_idx), na.rm = TRUE), 0)
  expect_gte(min(raster::values(niantic_static_maps$transport_idx), na.rm = TRUE), 0)
  expect_gte(min(raster::values(niantic_static_maps$delivery_idx), na.rm = TRUE), 0)
})
