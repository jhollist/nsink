context("nsink_generate_static_maps")
library(nsink)
library(sf)
load(system.file("testdata.rda", package="nsink"))

test_that("static maps are generated correctly", {
  skip_on_ci()
  niantic_static_maps <- nsink_generate_static_maps(niantic_data, niantic_removal,
                                                    samp_dens = 3000)
  expect_setequal(names(niantic_static_maps), c("removal_effic", "loading_idx",
                                         "transport_idx", "delivery_idx"))
})

test_that("static maps have positive values", {
  skip_on_ci()
  expect_gte(min(raster::values(niantic_static_maps$removal_effic), na.rm = TRUE), 0)
  expect_gte(min(raster::values(niantic_static_maps$loading_idx), na.rm = TRUE), 0)
  expect_gte(min(raster::values(niantic_static_maps$transport_idx), na.rm = TRUE), 0)
  expect_gte(min(raster::values(niantic_static_maps$delivery_idx), na.rm = TRUE), 0)
})
