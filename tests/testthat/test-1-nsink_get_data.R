
context("nsink_get_data")
library(nsink)
niantic_huc <- nsink_get_huc_id("Niantic River")$huc_12
if(dir.exists("nsink_test_data")){
  unlink("nsink_test_data", recursive = TRUE)
}

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
  rpu <- unique(nsink:::wbd_lookup[grepl(paste0("^", niantic_huc), nsink:::wbd_lookup$HUC_12),]$RPU)
  rpu <- rpu[!is.na(rpu)]
  folders <- list.dirs(paste0("nsink_test_data/", rpu), full.names = FALSE, recursive = FALSE)
  zips <- list.files(paste0("nsink_test_data/", rpu), pattern = "*.7z")
  files <- c(folders, zips)
  expect_setequal(files,
                  c("attr", "erom", "fdr","imperv", "nhd",
                    "NHDPlusV21_NE_01_01a_FdrFac_01.7z",
                    "NHDPlusV21_NE_01_EROMExtension_06.7z",
                    "NHDPlusV21_NE_01_NHDPlusAttributes_09.7z",
                    "NHDPlusV21_NE_01_NHDSnapshot_04.7z",
                    "NHDPlusV21_NE_01_WBDSnapshot_03.7z",
                    "nlcd", "ssurgo", "wbd"))
})
