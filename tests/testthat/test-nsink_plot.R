context("nsink_plot")
library(nsink)
library(sf)
load(system.file("testdata.rda", package="nsink"))

test_that("plots plot", {
  expect_null(nsink_plot_delivery(static_maps$delivery_idx))
  expect_null(nsink_plot_removal(static_maps$removal_effic))
  expect_null(nsink_plot_transport(static_maps$transport_idx))
})
