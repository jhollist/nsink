context("nsink_get_huc_id")
library(nsink)

test_that("nsink_get_huc_id gets expected values back", {
  skip_on_ci()
  expect_is(nsink_get_huc_id("Niantic")$huc_12, "character")
  expect_is(nsink_get_huc_id("Niantic"), "tbl_df")
  expect_equal(nsink_get_huc_id("Niantic")$huc_12, "011000030304")
  expect_equal(nsink_get_huc_id("Niantic River", exact = TRUE)$huc_12_name,
               "Niantic River")
})
