
context("nsink_plot")
library(nsink)
library(sf)
load(system.file("testdata.rda", package="nsink"))

test_that("plots plot", {
  expect_null(nsink_plot(niantic_static_maps, "removal"))
  expect_null(nsink_plot(niantic_static_maps, "transport"))
  expect_null(nsink_plot(niantic_static_maps, "delivery"))
  expect_null(nsink_plot_delivery(niantic_static_maps$delivery_idx))
  expect_null(nsink_plot_removal(niantic_static_maps$removal_effic))
  expect_null(nsink_plot_transport(niantic_static_maps$transport_idx))
})
