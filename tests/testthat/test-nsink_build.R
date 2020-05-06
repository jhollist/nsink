context("nsink_prep_data")
if(dir.exists("nsink_test_output")){
  unlink("nsink_test_output", recursive = TRUE)
}

# Add code to pull tif names and check that they are all there.
test_that("build runs as expected", {
  skip_on_ci()
  library(nsink)
  library(sf)
  load(system.file("testdata.rda", package="nsink"))
  aea <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0
  +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
  nsink_build(nsink_get_huc_id("Niantic River")$huc_12, aea,
              output_folder = "nsink_test_output", fact = 1500)
  nsink_output_tif <- list.files("nsink_test_output", ".tif")
  expect_setequal(nsink_output_tif, c("delivery_idx.tif", "fdr.tif",
                                      "impervious.tif", "loading_idx.tif",
                                      "nlcd.tif", "removal_effic.tif",
                                      "transport_effic.tif"))
})
