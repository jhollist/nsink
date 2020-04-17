context("nsink_prep_data")
library(nsink)
load(system.file("testdata.rda", package="nsink"))
removal <- nsink_calc_removal(niantic_nsink_data)


test_that("calc removal returns as expected", {
  expect_setequal(names(removal), c("raster_method", "land_removal",
                                    "network_removal"))
})

test_that("removal object has correct classes", {
  expect_s4_class(removal$raster_method, "RasterStack")
  expect_s3_class(removal$land_removal, "sf")
  expect_s3_class(removal$network_removal, "sf")
})

test_that("removal values are all non-negative", {
  expect_gte(min(raster::values(removal$raster_method), na.rm = TRUE), 0)
  expect_gte(min(removal$land_removal$layer, na.rm = TRUE), 0)
  expect_gte(min(removal$network_removal$n_removal, na.rm = TRUE), 0)
})

test_that("wrong data errors as expected", {
  expect_error(nsink_calc_removal(iris))
})
