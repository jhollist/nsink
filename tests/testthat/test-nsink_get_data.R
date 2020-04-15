context("nsink_get_data")
library(nsink)
niantic_huc <- nsink_get_huc_id("Niantic River")$huc_12

test_that("downloading data works", {
  skip_on_ci()
  expect_type(nsink_get_data(huc = niantic_huc, data_dir = "nsink_test_data",
                             force = TRUE), "list")
})

test_that("bad huc doesn't work", {
  expect_error(nsink_get_data(huc = "Bad HUC", data_dir = "nsink_test_data"))
})

test_that("dowloaded all data that is expected",{
  skip_on_ci()
  expect_setequal(list.files("nsink_test_data/"),
                  c("attr", "erom", "fdr","imperv", "nhd",
                    "NHDPlusV21_NE_01_01a_FdrFac_01.7z",
                    "NHDPlusV21_NE_01_EROMExtension_06.7z",
                    "NHDPlusV21_NE_01_NHDPlusAttributes_09.7z",
                    "NHDPlusV21_NE_01_NHDSnapshot_04.7z",
                    "NHDPlusV21_NE_01_WBDSnapshot_03.7z",
                    "nlcd", "ssurgo", "wbd"))
})
