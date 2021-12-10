context("nsink_summarize_flowpath")
library(nsink)
library(sf)
load(system.file("testdata.rda", package="nsink"))

test_that("flowpath summary works correctly", {
  #skip_on_ci()
  start_loc <- st_sf(st_sfc(st_point(c(pt)), crs = aea))
  niantic_fp <- nsink_generate_flowpath(start_loc, niantic_data)
  niantic_fp_removal <- nsink_summarize_flowpath(niantic_fp, niantic_removal)
  expect_setequal(names(niantic_fp_removal), c("segment_type", "length_meters",
                                         "percent_removal", "n_in", "n_out"))
})

test_that("percent_removal between 0 and 80", {
  expect_lte(max(niantic_fp_removal$percent_removal), 80)
  expect_gte(min(niantic_fp_removal$percent_removal), 0)
})

test_that("n_out between 0 and 100", {
  expect_lte(max(niantic_fp_removal$n_out), 100)
  expect_gte(min(niantic_fp_removal$n_out), 0)
})
