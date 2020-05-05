#' Build out required datasets for N-Sink
#'
#' This function is a wrapper around the other functions and runs all of those
#' required to build out the full dataset need for a huc plus develop the four
#' static N-Sink maps: the nitrogen loading index, nitrogen removal effeciency,
#' nitrogen transport effeciencey, and the nitrogen delivery index.  The primary
#' purpose of this is to use the nsink package to develop the required datasets
#' for an nsink application to be built in other software.  This will take some
#' time to complete as it is downloaing 500-600 Mb of data, processing that data
#' and then creating output files.
#'
#' @param huc A character with the 12 digit HUC ID.  Maybe searched with
#'            \code{\link{nsink_get_huc_id}}
#' @param projection Projection to use for all spatial data, passed as a PROJ
#'                   string
#' @param output_folder Folder to store downloaded data and process nsink files
#' @param force Logical value used to force a new download if data already
#'              exists on file system
#' @param fact The \code{fact} controls the density of points to use when
#'             creating the nitrogen removal heat map.  The area of the
#'             watershed is sampled with points that are separated by the
#'             \code{fact} value.  The larger the value, the fewer the points.
#' @param signal_finish Logical that triggers a sound when build is finished.
#' @export
#' @return a list providing details on the huc used and the output location of
#'         the dataset
#' @examples
#' \dontrun{
#' library(nsink)
#' aea <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0
#' +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
#' nsink_build(nsink_get_huc_id("Niantic River")$huc_12, aea,
#'             output_folder = "nsink_output", fact = 300)}
nsink_build <- function(huc, projection,
                        output_folder = normalizePath("nsink_output", winslash = "/"),
                        force = FALSE,
                        fact = 300, signal_finish = FALSE) {

  # Check for/create/clean data directory
  if (!dir.exists(output_folder)) {
    dir.create(output_folder)
  }
  output_folder <- nsink_fix_data_directory(output_folder)

  # Get raw data
  message("Getting data...")
  nsink_raw_data <- nsink_get_data(
    huc = huc, data_dir = output_folder,
    force = force
  )
  # Prep raw data
  message("Prepping data...")
  nsink_prepped_data <- nsink_prep_data(
    huc = huc, projection = projection,
    data_dir = output_folder
  )
  # Calculate nitrogen removal
  message("Calculating removal...")
  nsink_removal <- nsink_calc_removal(nsink_prepped_data)

  # Generate the static maps
  message("Creating static maps...")
  nsink_static_maps <- nsink_generate_static_maps(
    input_data = nsink_prepped_data, removal = nsink_removal, fact = fact
  )

  # Write everything out to a folder
  message("Writing files...")
  nsink_write_prepped_data(nsink_prepped_data, output_folder)
  nsink_write_static_maps(nsink_static_maps, output_folder)

  if(signal_finish){
    beepr::beep(8)
  }
}

#' Write prepped data to files
#'
#' Writes out data either as shapefiles, for vector data, or tiffs for raster
#' data.
#' @param prepped_data A list of prepped data, as output by
#'                     \code{\link{nsink_prep_data}}
#' @param output_folder Folder to store downloaded data and process nsink files
#' @keywords internal
nsink_write_prepped_data <- function(prepped_data, output_folder) {
  sf::st_write(prepped_data$streams, paste0(output_folder, "streams.shp"),
    delete_layer = TRUE
  )
  prepped_data$lakes <- sf::st_zm(prepped_data$lakes, drop = TRUE)
  sf::st_write(prepped_data$lakes, paste0(output_folder, "lakes.shp"),
    delete_layer = TRUE
  )
  sf::st_write(prepped_data$ssurgo, paste0(output_folder, "ssurgo.shp"),
    delete_layer = TRUE
  )
  sf::st_write(prepped_data$huc, paste0(output_folder, "huc.shp"),
    delete_layer = TRUE
  )
  raster::writeRaster(prepped_data$fdr, paste0(output_folder, "fdr.tif"),
    overwrite = TRUE
  )
  raster::writeRaster(prepped_data$impervious,
    paste0(output_folder, "impervious.tif"),
    overwrite = TRUE
  )
  raster::writeRaster(prepped_data$nlcd, paste0(output_folder, "nlcd.tif"),
    overwrite = TRUE
  )
  readr::write_csv(prepped_data$q, paste0(output_folder, "q.csv"))
  readr::write_csv(prepped_data$tot, paste0(output_folder, "tot.csv"))
  readr::write_csv(prepped_data$lakemorpho,
                   paste0(output_folder, "lakemorpho.csv")
  )
}

#' Write static maps to files
#'
#' Writes out static mapps as tiffs
#'
#' @param static_maps A list of static maps, as output by
#'                     \code{\link{nsink_generate_static_maps}}
#' @param output_folder Output folder in which to save static maps as .tif
#' @keywords internal
nsink_write_static_maps <- function(static_maps, output_folder) {
  raster::writeRaster(static_maps$removal_effic,
    paste0(output_folder, "removal_effic.tif"),
    overwrite = TRUE
  )
  raster::writeRaster(static_maps$loading_idx,
    paste0(output_folder, "loading_idx.tif"),
    overwrite = TRUE
  )
  raster::writeRaster(static_maps$transport_effic,
    paste0(output_folder, "transport_effic.tif"),
    overwrite = TRUE
  )
  raster::writeRaster(static_maps$delivery_idx,
    paste0(output_folder, "delivery_idx.tif"),
    overwrite = TRUE
  )
}
