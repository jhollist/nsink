#' Build out required datasets for N-Sink
#'
#' This function is a wrapper around the other functions and runs all of those
#' required to build out the full dataset need for a huc plus develop the four
#' static N-Sink maps: the nitrogen loading index, nitrogen removal effeciency,
#' nitrogen transport effeciencey, and the nitrogen delivery index.
#'
#' @param huc A character with the 12 digit HUC ID.  Maybe searched with \code{\link{nsink_get_huc_id}}
#' @param projection
#' @param output_folder
#' @param force
#' @export
#' @return a list providing details on the huc used and the output location of the dataset
#' @examples
#' library(nsink)
#' nsink_build(nsink_get_huc_id("Niantic River")$huc_12)
nsink_build <- function(huc, projection,
                        output_folder = normalizePath("nsink_output"),
                        force = FALSE,
                        ){
  # Check for/create/clean data directory
  output_folder <- nsink_fix_data_directory(output_folder)
  nsink_raw_data <- nsink_get_data(huc = huc, data_dir = output_folder, force = force)
  nsink_prepped_data <- nsink_prep_data(huc = huc, projection = projection, data_dir = output_folder)
  nsink_removal <- nsink_calc_removal(nsink_prepped_data)
  nsink_static_maps <- nsink_generate_static_maps(input_data = nsink_prepped_data, removal = nsink_removal, )


}
