context("nsink_prep_data")
library(nsink)
niantic_huc_id <- nsink_get_huc_id("Niantic River")$huc_12
aea <- 5072

test_that("data prep works", {
  #skip("skipping test_nsink_prep_data")
  skip_on_ci()
  niantic_nsink_data <- nsink_prep_data(huc = niantic_huc_id, projection = aea,
                                        data_dir = "nsink_test_data")
  expect_type(niantic_nsink_data, "list")
  expect_setequal(names(niantic_nsink_data), c("streams", "lakes", "fdr",
                                               "impervious", "nlcd", "ssurgo",
                                               "q", "tot", "lakemorpho", "huc",
                                               "raster_template"))
})

test_that("data prep fails as expected",{
  expect_error(nsink_prep_data(huc = niantic_huc_id, projection = aea,
                               data_dir = "."))
})
