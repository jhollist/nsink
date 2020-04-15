context("nsink_get_huc_id")
library(nsink)
niantic_huc <- nsink_get_huc_id("Niantic River")$huc_12

test_that("downloading data works", {
  expect_is(nsink_get_data(huc = niantic_huc, data_dir = "nsink_test_data"),
            "list")
})
