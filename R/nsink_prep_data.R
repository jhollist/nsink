#' Prepares N-Sink data for a given HUC
#'
#' In addition to having local access to the required dataset, those datasets
#' need to have some preparation.  This function standardizes projections and
#' extents and clips all datasets to the boundary of the specified HUC.
#' Additionally, any tabular datasets (e.g. flow, time of travel etc.) are
#' included in the output as well.
#'
#' @param huc
#' @param data_dir
#' @param projection
#' @return returns a list of sf, raster, or tabular objects for each of the required datasets plus
#'         the huc.
#' @export
#' @examples
nsink_prep_data <- function(){
  list(streams = , lakes = , fdr = , impervious = , ssurgo = , huc = )
}

#' Maybe have separate functions for prep on each object.
