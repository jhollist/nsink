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
                            data_dir = paste0(getwd(), "/nsink_data")){
  huc_sf <- sf::st_read(paste0(data_dir, "/wbd/WBD_Subwatershed.shp"))
  huc_sf <- sf::st_transform(huc_sf[huc_sf$HUC_12 == huc,],
                             crs = projection)
  huc_raster <- fasterize::raster(huc_sf, resolution = 30)
  list(streams = nsink_prep_streams(huc_sf, data_dir),
       lakes = nsink_prep_lakes(huc_sf, data_dir),
       fdr = nsink_prep_fdr(huc_sf, huc_raster, data_dir),
       impervious = nsink_prep_impervious(huc_sf,  huc_raster, data_dir),
       ssurgo = nsink_prep_ssurgo(huc_sf, data_dir),
       q = nsink_prep_q(data_dir),
       tot = nsink_prep_tot(data_dir),
       huc = huc_sf)
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
  if(file.exists(paste0(data_dir, "/nhd/NHDFlowline.shp"))){
    streams <- sf::st_read(paste0(data_dir, "/nhd/NHDFlowline.shp"))
    streams <- sf::st_transform(streams, sf::st_crs(huc_sf))
    streams <- sf::st_zm(streams)
    streams <- dplyr::rename_all(streams, tolower)
    streams <- dplyr::rename(streams, stream_comid = comid,
                             lake_comid = wbareacomi)
    streams <- dplyr::slice(streams, sf::st_contains(huc_sf, streams)[[1]])
    streams <- sf::st_crop(streams, sf::st_bbox(huc_sf))
    streams <- dplyr::mutate_if(streams, is.factor, as.character())
  } else {
    stop("The required data file does not exist.  Run nsink_get_data().")
  }
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
  if(file.exists(paste0(data_dir, "/nhd/NHDWaterbody.shp"))){
    lakes <- sf::st_read(paste0(data_dir, "/nhd/NHDWaterbody.shp"))
    lakes <- sf::st_transform(lakes, sf::st_crs(huc_sf))
    lakes <- dplyr::rename_all(lakes, tolower)
    lakes <- dplyr::rename(lakes, lake_comid = comid)
    lakes <- dplyr::filter(lakes, ftype == "LakePond")
    lakes <- dplyr::slice(lakes, sf::st_contains(huc_sf, lakes)[[1]])
  } else {
    stop("The required data file does not exist.  Run nsink_get_data().")
  }
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
  if(dir.exists(paste0(data_dir, "/fdr"))){
    fdr <- raster::raster(paste0(data_dir, "/fdr"))
    fdr <- crop(fdr, as(huc_sf, "Spatial"))
    fdr <- projectRaster(fdr, huc_raster, method = "ngb")
  } else {
    stop("The required data file does not exist.  Run nsink_get_data().")
  }
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
  if(file.exists(paste0(data_dir, "/imperv/",
                        as.character(huc_sf$HUC_12),
                        "_NLCD_2011_impervious.tif"))){
  impervious <- raster::raster(paste0(data_dir, "/imperv/",
                                      as.character(huc_sf$HUC_12),
                                      "_NLCD_2011_impervious.tif"))
  impervious <- raster::projectRaster(impervious, huc_raster)
  } else {
    stop("The required data file does not exist.  Run nsink_get_data().")
  }
  impervious
}

#' Prepare SSURGO data for N-Sink
#'
#' Standardizes impervious data by transforming data, clipping to HUC, ...
#'
#' @param huc_sf An sf object of the Watershed Boundaries Dataset HUC12
#' @param data_dir Base directory that contains N-Sink data folders.  Data may
#'                 be downloaded with the \code{\link{nsink_get_data}} function.
#' @return returns a sf object of the SSURGO data with hydric data added.
#'         for the huc_sf
#' @keywords  internal
nsink_prep_ssurgo <- function(huc_sf, data_dir){
  if(file.exists(paste0(data_dir, "/ssurgo/", as.character(huc_sf$HUC_12),
                        "_SSURGO_Mapunits.shp"))){
    ssurgo <- sf::st_read(paste0(data_dir, "/ssurgo/",
                                 as.character(huc_sf$HUC_12),
                                 "_SSURGO_Mapunits.shp"))
    ssurgo <- sf::st_transform(ssurgo, st_crs(huc_sf))
    ssurgo <- dplyr::rename_all(ssurgo, tolower)
    ssurgo <- dplyr::mutate(ssurgo, mukey = as(mukey, "character"))
    ssurgo_tbl <- read.csv(paste0(data_dir, "/ssurgo/", as.character(huc_sf$HUC_12),
                                  "_SSURGO_component.csv"))
    ssurgo_tbl <- dplyr::mutate(ssurgo_tbl, mukey = as(mukey, "character"))
    ssurgo_tbl <- dplyr::select(ssurgo_tbl, mukey, cokey, hydricrating,
                                comppct.r)
    ssurgo_tbl <- dplyr::filter(ssurgo_tbl, hydricrating == "Yes")
    ssurgo_tbl <- dplyr::group_by(ssurgo_tbl, mukey, hydricrating)
    ssurgo_tbl <- dplyr::summarize(ssurgo_tbl, hydric_pct = sum(comppct.r))
    ssurgo <- full_join(ssurgo, ssurgo_tbl, by = "mukey")
  } else {
    stop("The required data file does not exist.  Run nsink_get_data().")
  }
  ssurgo
}

#' Prepare flow data for N-Sink
#'
#' Standardizes flow data from the EROM tables.
#'
#' @param data_dir Base directory that contains N-Sink data folders.  Data may
#'                 be downloaded with the \code{\link{nsink_get_data}} function.
#' @return returns a tibble of the flow data
#' @keywords  internal
nsink_prep_q <- function(data_dir){
  if(file.exists(paste0(data_dir, "/erom/EROM_MA0001.DBF"))){
    q <- foreign::read.dbf(paste0(data_dir, "/erom/EROM_MA0001.DBF"))
    q <- dplyr::select(q, stream_comid = ComID, q_cfs = Q0001E)
    q <- dplyr::mutate(q, q_cms = q_cfs * 0.028316846592,
                mean_reach_depth = 0.2612 * (q_cms ^ 0.3966))
    q <- dplyr::mutate_if(q, is.factor, as.character())
  } else {
    stop("The required data file does not exist.  Run nsink_get_data().")
  }
  q
}

#' Prepare time of travel data for N-Sink
#'
#' Standardizes time of travel from the VAA tables.
#'
#' @param data_dir Base directory that contains N-Sink data folders.  Data may
#'                 be downloaded with the \code{\link{nsink_get_data}} function.
#' @return returns a tibble of the time of travel data
#' @keywords  internal
nsink_prep_tot <- function(data_dir){
  if(file.exists(paste0(data_dir, "/attr/PlusFlowlineVAA.dbf"))){
    tot <- foreign::read.dbf(paste0(data_dir, "/attr/PlusFlowlineVAA.dbf"))
    tot <- dplyr::rename_all(tot, tolower)
    tot <- dplyr::select(tot, stream_comid = comid, totma = totma)
    tot <- dplyr::mutate_if(tot, is.factor, as.character())
  } else {
    stop("The required data file does not exist.  Run nsink_get_data().")
  }
  tot
}
