#' Prepares N-Sink data for a given HUC
#'
#' In addition to having local access to the required dataset, those datasets
#' need to have some preparation.  This function standardizes projections and
#' extents and clips all datasets to the boundary of the specified HUC.
#' Additionally, any tabular datasets (e.g. flow, time of travel etc.) are
#' included in the output as well.
#'
#' @param huc A character string of the HUC12 ID.  Use
#'            \code{\link{nsink_get_huc_id}} to look up ID by name.
#' @param data_dir Base directory that contains N-Sink data folders.  Data may be downloaded
#'                 with the \code{\link{nsink_get_data}} function.
#' @param projection EPSG code or proj4 string
#' @return returns a list of sf, raster, or tabular objects for each of the
#'         required datasets plus the huc.
#' @export
#' @examples
#' \dontrun{
#' library(nsink)
#' aea <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
#' niantic_huc <- nsink_get_huc_id("Niantic River")$huc_12
#' nsink_prep_data(huc = niantic_huc, projection = aea)
#' }
nsink_prep_data <- function(huc, projection,
                            data_dir = paste0(getwd(), "nsink_data")){
  huc_sf <- sf::st_read(paste0(data_dir, "/wbd/WBD_Subwatershed.shp"))
  huc_sf <- sf::st_transform(huc_sf[huc_sf$HUC_12 == huc,],
                             crs = projection)
  huc_raster <- fasterize::raster(huc_sf, resolution = 30)
  browser()
  list(streams = nsink_prep_streams(huc_sf, data_dir),
       lakes = nsink_prep_lakes(huc_sf, data_dir),
       fdr = nsink_prep_fdr(huc_sf, huc_raster, data_dir),
       impervious = nsink_prep_impervious(huc_sf,  huc_raster, data_dir),
       ssurgo = nsink_prep_ssurgo(huc_sf, data_dir),
       huc = huc_sf,
       huc_raster =  huc_raster)
}

#' Prepare streams data for N-Sink
#'
#' Standardizes streams data by transforming data, clipping to HUC, ...
#'
#' @param huc_sf An sf object of the Watershed Boundaries Dataset HUC12
#' @param data_dir Base directory that contains N-Sink data folders.  Data may
#'                 be downloaded with the \code{\link{nsink_get_data}} function.
#' @return returns an sf object of the NHDPlus streams for the huc_sf
#' @keywords  internal
nsink_prep_streams <- function(huc_sf, data_dir){
  streams <- sf::st_read(paste0(data_dir, "/nhd/NHDFlowline.shp"))
  streams <- sf::st_transform(streams, sf::st_crs(huc_sf))
  streams <- sf::st_zm(streams)
  streams <- dplyr::rename_all(streams, tolower)
  streams <- dplyr::rename(streams, stream_comid = comid,
                           lake_comid = wbareacomi)
  streams <- dplyr::slice(streams, sf::st_contains(huc_sf, streams)[[1]])
  streams <- sf::st_crop(streams, sf::st_bbox(huc_sf))
  streams
}

#' Prepare lakes data for N-Sink
#'
#' Standardizes lakes data by transforming data, clipping to HUC, ...
#'
#' @param huc_sf An sf object of the Watershed Boundaries Dataset HUC12
#' @param data_dir Base directory that contains N-Sink data folders.  Data may be
#'                 downloaded with the \code{\link{nsink_get_data}} function.
#' @return returns an sf object of the NHDPlus lakes for the huc_sf
#' @keywords  internal
nsink_prep_lakes <- function(huc_sf, data_dir){
  lakes <- sf::st_read(paste0(data_dir, "/nhd/NHDWaterbody.shp"))
  lakes <- sf::st_transform(lakes, sf::st_crs(huc_sf))
  lakes <- dplyr::rename_all(lakes, tolower)
  lakes <- dplyr::rename(lakes, lake_comid = comid)
  lakes <- dplyr::filter(ftype == "LakePond")
  lakes <- dplyr::slice(sf::st_contains(huc_sf, lakes)[[1]])
  lakes
}

#' Prepare flow direction data for N-Sink
#'
#' Standardizes flow direction data by transforming data, clipping to HUC, ...
#'
#' @param huc_sf An sf object of the Watershed Boundaries Dataset HUC12
#' @param huc_raster A raster object of the Watershed Boundaries Dataset HUC12
#' @param data_dir Base directory that contains N-Sink data folders.  Data may be
#'                 downloaded with the \code{\link{nsink_get_data}} function.
#' @return returns a raster object of the flow direction for the huc_sf
#' @keywords  internal
nsink_prep_fdr <- function(huc_sf, huc_raster, data_dir){
  fdr <- raster::raster(paste0(data_dir, "/fdr"))
  fdr <- crop(fdr, as(huc_sf, "Spatial"))
  fdr <- projectRaster(fdr, huc_raster, method = "ngb")
  fdr
}

#' Prepare impervious cover data for N-Sink
#'
#' Standardizes impervious data by transforming data, clipping to HUC, ...
#'
#' @param huc_sf An sf object of the Watershed Boundaries Dataset HUC12
#' @param huc_raster A raster object of the Watershed Boundaries Dataset HUC12
#' @param data_dir Base directory that contains N-Sink data folders.  Data may
#'                 be downloaded with the \code{\link{nsink_get_data}} function.
#' @return returns a raster object of the impervious cover for the huc_sf
#' @keywords  internal
nsink_prep_impervious <- function(huc_sf, huc_raster, data_dir){
  impervious <- raster::raster(paste0(data_dir, "/imperv"))
}

#' Prepare SSURGO data for N-Sink
#'
#' Standardizes impervious data by transforming data, clipping to HUC, ...
#'
#' @param huc_sf An sf object of the Watershed Boundaries Dataset HUC12
#' @param data_dir Base directory that contains N-Sink data folders.  Data may
#'                 be downloaded with the \code{\link{nsink_get_data}} function.
#' @return returns a list object of the SSURGO data in spatial and tabular forms
#'         for the huc_sf
#' @keywords  internal
nsink_prep_ssurgo <- function(huc_sf, data_dir){

}
