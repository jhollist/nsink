context("nsink_prep_data")
library(nsink)
library(sf)
load(system.file("testdata.rda", package="nsink"))
removal <- nsink_calc_removal(niantic_nsink_data)
pt <- c(1948121, 2295822)
start_loc <- st_sf(st_sfc(st_point(c(pt)), crs = aea))
fp <- nsink_generate_flowpath(start_loc, niantic_nsink_data)
flow_summary <- nsink_summarize_flowpath(fp, removal)

test_that("flowpath summary works correctly", {
  expect_setequal(names(flow_summary), c("segment_type", "length",
                                         "percent_removal", "n_in", "n_out"))
})

test_that("percent_removal between 0 and 80", {
  expect_lte(max(flow_summary$percent_removal), 80)
  expect_gte(min(flow_summary$percent_removal), 0)
})

test_that("n_out between 0 and 100", {
  expect_lte(max(flow_summary$n_out), 100)
  expect_gte(min(flow_summary$n_out), 0)
})
