context("nsink_build")

test_that("build runs as expected", {
  #skip_on_ci()
  library(nsink)
  library(sf)
  load(system.file("testdata.rda", package="nsink"))
  aea <- 5072

  nsink_build(nsink_get_huc_id("Niantic River")$huc_12, aea,
              output_dir = "test_output", data_dir = "nsink_test_data",
              samp_dens = 3000)
  nsink_output_tif <- list.files("test_output", ".tif")
  nsink_output_shp <- list.files("test_output", ".shp")

  expect_setequal(nsink_output_tif, c("delivery_idx.tif", "fdr.tif",
                                      "impervious.tif", "loading_idx.tif",
                                      "nlcd.tif", "removal_effic.tif",
                                      "transport_idx.tif"))
  expect_setequal(nsink_output_shp, c("huc.shp","lakes.shp","ssurgo.shp",
                                       "streams.shp"))
})

