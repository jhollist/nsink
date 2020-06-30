
context("nsink_calc_removal")
library(nsink)
load(system.file("testdata.rda", package="nsink"))
niantic_removal <- nsink_calc_removal(niantic_data)


test_that("calc removal returns as expected", {
  expect_setequal(names(niantic_removal), c("raster_method", "land_off_network_removal",
                                    "land_off_network_removal_type",
                                    "network_removal"))
})

test_that("removal object has correct classes", {
  expect_s4_class(niantic_removal$raster_method, "RasterStack")
  expect_s3_class(niantic_removal$land_off_network_removal, "sf")
  expect_s3_class(niantic_removal$land_off_network_removal_type, "sf")
  expect_s3_class(niantic_removal$network_removal, "sf")
})

test_that("removal values are all non-negative", {
  expect_gte(min(raster::values(niantic_removal$raster_method), na.rm = TRUE), 0)
  expect_gte(min(niantic_removal$land_off_network_removal$layer, na.rm = TRUE), 0)
  expect_gte(min(niantic_removal$network_removal$n_removal, na.rm = TRUE), 0)
})

test_that("wrong data errors as expected", {
  expect_error(nsink_calc_removal(Loblolly))
})
