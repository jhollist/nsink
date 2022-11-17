#' Build out required datasets for N-Sink
#'
#' This function is a wrapper around the other functions and runs all of those
#' required to build out the full dataset needed for a huc and develops the four
#' static N-Sink maps: the nitrogen loading index, nitrogen removal effeciency,
#' nitrogen transport index, and the nitrogen delivery index.  The primary
#' purpose of this is to use the nsink package to develop the required datasets
#' for an nsink application to be built outside of R (e.g. ArcGIS).  This will
#' take some time to complete as it is downloading 500-600 Mb of data,
#' processing that data and then creating output files.
#'
#' @param huc A character with the 12 digit HUC ID.  May be searched with
#'            \code{\link{nsink_get_huc_id}}
#' @param projection Projection to use for all spatial data, specified as either an
#'                   EPSG code (as numeric) or WKT (as string).
#' @param output_dir Folder to write processed nsink files to.
#'                      Currently, the processed files will be overwritten if
#'                      the same output folder is used.  To run different
#'                      HUC12's specify separate output folders.
#' @param data_dir Folder to hold downloaded data.  The same data
#'                 directory can be used to hold data for multiple HUCs.  Data
#'                 will not be downloaded again if it already exists in this
#'                 folder.
#' @param force Logical value used to force a new download if data already
#'              exists on file system.
#' @param samp_dens The \code{samp_dens} controls the density of points to use when
#'             creating the nitrogen removal heat map.  The area of the
#'             watershed is sampled with points that are separated by the
#'             \code{samp_dens} value, in the units of the input data.
#'             The larger the value, the fewer the points.
#' @param year Year argument to be passed to FedData's \code{\link{get_nlcd}}
#'             function. Defaults to 2016.
#' @param ... Passes to \code{\link{nsink_calc_removal}} for the off network
#'            arguments: \code{off_network_lakes}, \code{off_network_streams},
#'            and \code{off_network_canalsditches}.
#' @export
#' @return A list providing details on the huc used and the output location of
#'         the dataset.
#' @examples
#' \dontrun{
#' library(nsink)
#' aea <- 5072
#' nsink_build(nsink_get_huc_id("Niantic River")$huc_12, aea,
#'             output_dir = "nsink_output", data_dir = "nsink_data",
#'              samp_dens = 600)
#' }
nsink_build <- function(huc, projection,
                        output_dir = normalizePath("nsink_output",
                                                   winslash = "/",
                                                   mustWork = FALSE),
                        data_dir = normalizePath("nsink_data", winslash = "/",
                                                 mustWork = FALSE),
                        force = FALSE,
                        samp_dens = 300,
                        year = "2016", ...) {

  year <- as.character(year)
  # Check for/create/clean output directory
  output_dir <- nsink_fix_data_directory(output_dir)
  data_dir <- nsink_fix_data_directory(data_dir)

  # Get raw data
  message("Getting data...")
  nsink_raw_data <- nsink_get_data(
    huc = huc, data_dir = data_dir,
    force = force, year = year)

  # Prep raw data
  message("Prepping data...")
  nsink_prepped_data <- nsink_prep_data(
    huc = huc, projection = projection,
    data_dir = data_dir, year = year)

  # Calculate nitrogen removal
  message("Calculating removal...")
  nsink_removal <- nsink_calc_removal(nsink_prepped_data, ...)

  # Generate the static maps
  message("Creating static maps...")
  nsink_static_maps <- nsink_generate_static_maps(input_data = nsink_prepped_data,
                                                  removal = nsink_removal,
                                                  samp_dens = samp_dens)

  # Write everything out to a folder
  message("Writing files...")
  nsink_write_prepped_data(nsink_prepped_data, output_dir)
  save(nsink_removal, file=paste0(output_dir, "removal.rda"), compress = "xz")
  nsink_write_static_maps(nsink_static_maps, output_dir)
}

#' Write prepped data to files
#'
#' Writes out data either as shapefiles, for vector data, or tiffs for raster
#' data.
#' @param prepped_data A list of prepped data, as output by
#'                     \code{\link{nsink_prep_data}}
#' @param output_dir Output folder to save processed nsink files to
#' @keywords internal
nsink_write_prepped_data <- function(prepped_data, output_dir) {
  suppressWarnings(sf::st_write(prepped_data$streams, paste0(output_dir, "streams.shp"),
    delete_layer = TRUE, quiet = TRUE
  ))
  prepped_data$lakes <- sf::st_zm(prepped_data$lakes, drop = TRUE)
  sf::st_write(prepped_data$lakes, paste0(output_dir, "lakes.shp"),
    delete_layer = TRUE, quiet = TRUE
  )
  suppressWarnings(sf::st_write(prepped_data$ssurgo, paste0(output_dir, "ssurgo.shp"),
    delete_layer = TRUE, quiet = TRUE
  ))
  suppressWarnings(sf::st_write(prepped_data$huc, paste0(output_dir, "huc.shp"),
    delete_layer = TRUE, quiet = TRUE
  ))
  raster::writeRaster(prepped_data$fdr, paste0(output_dir, "fdr.tif"),
    overwrite = TRUE
  )
  raster::writeRaster(prepped_data$impervious,
    paste0(output_dir, "impervious.tif"),
    overwrite = TRUE
  )
  raster::writeRaster(prepped_data$nlcd, paste0(output_dir, "nlcd.tif"),
    overwrite = TRUE
  )
  readr::write_csv(prepped_data$q, paste0(output_dir, "q.csv"))
  readr::write_csv(prepped_data$tot, paste0(output_dir, "tot.csv"))
  readr::write_csv(prepped_data$lakemorpho,
                   paste0(output_dir, "lakemorpho.csv")
  )
}

#' Write static maps to files
#'
#' Writes out static maps as tiffs
#'
#' @param static_maps A list of static maps, as output by
#'                     \code{\link{nsink_generate_static_maps}}
#' @param output_dir Output folder to save .tif static maps to
#' @keywords internal
nsink_write_static_maps <- function(static_maps, output_dir) {
  raster::writeRaster(static_maps$removal_effic,
    paste0(output_dir, "removal_effic.tif"),
    overwrite = TRUE
  )
  raster::writeRaster(static_maps$loading_idx,
    paste0(output_dir, "loading_idx.tif"),
    overwrite = TRUE
  )
  raster::writeRaster(static_maps$transport_idx,
    paste0(output_dir, "transport_idx.tif"),
    overwrite = TRUE
  )
  raster::writeRaster(static_maps$delivery_idx,
    paste0(output_dir, "delivery_idx.tif"),
    overwrite = TRUE
  )
}
