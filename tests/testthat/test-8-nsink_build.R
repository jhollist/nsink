context("nsink_build")

test_that("build runs as expected", {
  skip_on_ci()
  library(nsink)
  library(sf)
  load(system.file("testdata.rda", package="nsink"))
  aea <- 5072

  nsink_build(nsink_get_huc_id("Niantic River")$huc_12, aea,
              output_folder = "nsink_test_data", samp_dens = 3000)
  nsink_output_tif <- list.files("nsink_test_data", ".tif")
  nsink_output_shp <- list.files("nsink_test_data", ".shp")

  expect_setequal(nsink_output_tif, c("delivery_idx.tif", "fdr.tif",
                                      "impervious.tif", "loading_idx.tif",
                                      "nlcd.tif", "removal_effic.tif",
                                      "transport_idx.tif"))
  expect_setequal(nsink_output_shp, c("huc.shp","lakes.shp","ssurgo.shp",
                                       "streams.shp"))
})

