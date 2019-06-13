#' Calculates N-Sink nitrogen removal percent
#'
#' Starting with base data layers of NHDPlus, SSURGO, impervious surface, flow
#' velocity, and time of travel, this function calculates percentage of Nitrogen
#' removal.  Details for nitrogen removal calculation are from
#' \href{https://doi.org/10.1016/j.ecoleng.2010.02.006}{Kellogg et al. (2010)}.
#' This function assumes data has been downloaded with
#' \code{\link{nsink_get_data}} and has been prepared with
#' \code{\link{nsink_prep_data}}.
#'
#' @param input_data A list of input datasets created with
#'                   \code{\link{nsink_prep_data}}.
#'
#' @return A raster stack with one layer with nitrogen removal and a second
#'         layer with the type of removal (e.g. hydric soils, lakes, streams)
#'
#' @references Kellogg, D. Q., Gold, A. J., Cox, S., Addy, K., & August, P. V.
#'             (2010). A geospatial approach for assessing denitrification sinks
#'             within lower-order catchments. Ecological Engineering, 36(11),
#'             1596-1606.
#'             \href{https://doi.org/10.1016/j.ecoleng.2010.02.006}{Link}
#' @examples
#' \dontrun{
#' library(nsink)
#' niantic_huc <- nsink_get_huc_id("Niantic River")$huc_12
#' niantic_nsink_data <- nsink_prep_data(niantic_huc)
#' niantic_calc_removal(niantic_nsink_data)
#' }
nsink_calc_removal <- function(input_data){
  if(all(names(input_data) == c("streams","lakes", "fdr", "impervious", "ssurgo",
                            "q", "tot", "huc"))){
    land_removal <- nsink_calc_land_removal()
    stream_removal <- nsink_calc_stream_removal()
    lake_removal <- nsink_calc_lake_removal()
    merged_removal <- nsink_merge_removal()
    merged_type <- nsink_calc_removal_type()
  } else {
    stop("The input data do contain the expected data.  Check the object and
         re-run with nsink_prep_data().")
  }
  raster::stack(merged_removal, merged_type)
}

#' Calulates land-based nitrogen removal
#'
#' @param input_data list of data ...
#' @return raster of land based nitrogen removal
#' @keywords internal
nsink_calc_land_removal <- function(input_data){

}

#' Calculates stream-based nitrogen removal
#'
#' @param input_data list of data ...
#' @return raster of stream based nitrogen removal
#' @keywords internal
nsink_calc_stream_removal <- function(input_data){

}

#' Calculates lake-based nitrogen removal
#'
#' @param input_data list of data ...
#' @return raster of lake based nitrogen removal
#' @keywords internal
nsink_calc_lake_removal <- function(input_data){

}

#' Merges removal rasters into single raster
#'
#' @param removal_rasters list of land, stream, and lake nitrogen removal
#'                        rasters.
#' @return raster of stream based nitrogen removal
#' @keywords internal
nsink_calc_stream_removal <- function(removal_rasters){

}
