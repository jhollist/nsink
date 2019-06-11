#' Get's N-sink data for a given HUC
#'
#' The required datasets for the N-sink analysis are available from multiple,
#' online resources.  This function takes a HUC as input and downloads local
#' copies of those datasets.
#' @param location An sf polygon object of the HUC (or really any spatial object) for
#'            which you wish to build the N-Sink database.
#' @param data_dir A directory to store N-Sink data downloads.  Defaults to
#'                 current working directory.
#' @export
#' @examples
#' data(niantic_huc)
#' nsink_get_data(location = niantic_huc)
nsink_get_data <- function(location, data_dir = paste0(getwd(),"/nsink_data")){
  browser()
  # Enforce same projection
  vpu_sf <- sf::st_transform(nhdR::vpu_shp, crs = 4269)
  loc_sf <- sf::st_point_on_surface(sf::st_transform(location, crs = 4269))

  # Get vpu
  vpu_inter_loc <- sf::st_intersection(vpu_sf, loc_sf)
  vpu <- vpu_inter_loc[vpu_inter_loc$UnitType == "VPU"]$UnitID

  # urls
  attr_url <- nsink_get_plus_remotepath(vpu, "NHDPlusAttributes")
  erom_url <- nsink_get_plus_remotepath(vpu, "EROMExtension")
  nhd_url <- nsink_get_plus_remotepath(vpu, "NHDSnapshot")
  fdr_url <- nsink_get_plus_remotepath(vpu, "FdrFac")
  wbd_url <- nsink_get_plus_remotepath(vpu, "WBDSnapshot")


  # Get required NHDPlus components with nhdR
  # nhdR manages its own cache location
  # does not yet include FdrFac or WBDSnapshot

  nhdR::nhd_plus_get(vpu = vpu, component = "NHDPlusAttributes")
  nhdR::nhd_plus_get(vpu = vpu, component = "EROMExtension")
  nhdR::nhd_plus_get(vpu = vpu, component = "NHDSnapshot")

  # Get other NHDPlus components not exposed with nhdR
  # nsink manages local cache

  fdr_url <- gsub("NHDSnapshot_04", "FdrFac_01",nhdR:::get_plus_remotepath(vpu))
  wbd_url <- gsub("NHDSnapshot_04", "WBDSnapshot_03",nhdR:::get_plus_remotepath(vpu))

  httr::GET(fdr_url, httr::write_disk(data_dir))



  # Get impervious
  # First need to limit to
  #FedData::get_

  # Get SSURGO


}
