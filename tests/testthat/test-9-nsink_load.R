context("nsink_load")

test_that("load runs as expected", {
  skip_on_ci()
  library(nsink)
  library(sf)
  aea <- 5072
  nsink_load("test_output", "tested_", aea)
  expect_setequal(names(tested_data), c("streams", "lakes", "fdr", "impervious",
                                        "nlcd", "ssurgo","q", "tot",
                                        "lakemorpho", "huc", "raster_template"))
  expect_setequal(names(tested_removal), c("raster_method",
                                           "land_off_network_removal",
                                           "land_off_network_removal_type",
                                           "network_removal"))
  expect_setequal(names(tested_static_maps), c("removal_effic", "loading_idx",
                                               "transport_idx", "delivery_idx"))


})

# Clean up after tests
unlink("test_output", recursive = TRUE, force = TRUE)
if(dir.exists("nsink_test_data")){
  unlink("nsink_test_data", recursive = TRUE, force = TRUE)
}
