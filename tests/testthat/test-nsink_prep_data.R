context("nsink_prep_data")
library(nsink)
niantic_huc <- nsink_get_huc_id("Niantic River")$huc_12
aea <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0
+ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

test_that("data prep works", {
  skip_on_ci()
  niantic_nsink_data <- nsink_prep_data(huc = niantic_huc, projection = aea,
                                        data_dir = "nsink_test_data")
  expect_type(niantic_nsink_data, "list")
  expect_setequal(names(niantic_nsink_data), c("streams", "lakes", "fdr",
                                               "impervious", "nlcd", "ssurgo",
                                               "q", "tot", "lakemorpho", "huc",
                                               "raster_template"))
})

test_that("data prep fails as expected",{
  expect_error(nsink_prep_data(huc = niantic_huc, projection = aea,
                               data_dir = "."))
})
