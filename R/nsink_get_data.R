#' Get's N-sink data for a given HUC
#'
#' The required datasets for the N-sink analysis are available from multiple,
#' online resources.  This function takes a HUC as input and downloads local
#' copies of those datasets.
#' @param huc A character string of a HUC12 identifier or a HUC12 name.
#' @param data_dir A directory to store N-Sink data downloads.  Defaults to
#'                 current working directory.
#' @param download_again Logical to determine if files should be downloaded
#'                       again if they already exist locally.
#' @export
#' @examples
#' \dontrun{
#' niantic_huc <- "011000030304"
#' niantic_name <- "Niantic River"
#' nsink_get_data(huc = niantic_huc)
#' nsink_get_data(huc = niantic_name)
#' }
nsink_get_data <- function(huc, data_dir = paste0(getwd(),"/nsink_data"),
                           download_again = FALSE){

  # Check for/create data directory
  if(!dir.exists(data_dir)){dir.create(data_dir)}

  browser()

  # Get vpu
  vpu <- wbd_lookup[wbd_lookup$HUC_12 == huc | wbd_lookup$HU_12_NAME == huc,]$VPUID
  vpu <- vpu[!is.na(vpu)]

  # urls
  attr_url <- nsink_get_plus_remotepath(vpu, "NHDPlusAttributes")
  erom_url <- nsink_get_plus_remotepath(vpu, "EROMExtension")
  nhd_url <- nsink_get_plus_remotepath(vpu, "NHDSnapshot")
  fdr_url <- nsink_get_plus_remotepath(vpu, "FdrFac")
  wbd_url <- nsink_get_plus_remotepath(vpu, "WBDSnapshot")

  # get nhdplus data
  attr <- get_nhd_plus(attr_url, data_dir, download_again)
  erom <- get_nhd_plus(erom_url, data_dir, download_again)
  nhd <- get_nhd_plus(nhd_url, data_dir, download_again)
  fdr <- get_nhd_plus(fdr_url, data_dir, download_again)
  wbd <- get_nhd_plus(wbd_url, data_dir, download_again)

  # unzip nhdplus data
  run_7z(paste0(data_dir, "/", basename(attr_url)), data_dir)
  run_7z(paste0(data_dir, "/", basename(erom_url)), data_dir)
  run_7z(paste0(data_dir, "/", basename(nhd_url)), data_dir)
  run_7z(paste0(data_dir, "/", basename(fdr_url)), data_dir)
  run_7z(paste0(data_dir, "/", basename(wbd_url)), data_dir)

  # Use actual huc to limit downloads on impervious and ssurgo
  huc_sf <- sf::st_read(paste0(data_dir, "/WBD_Subwatershed.shp"))
  huc_sf <- huc_sf[huc_sf$HUC_12 == huc | huc_sf$HU_12_NAME == huc, ]


  # Get impervious
  imp <- FedData::get_nlcd(as(huc_sf, "Spatial"), dataset = "impervious",
                    label = vpu, extraction.dir = data_dir, raw.dir = data_dir,
                    force.redo = download_again)

  # Get SSURGO
  ssurgo <- FedData::get_ssurgo(as(huc_sf, "Spatial"),label = vpu,
                                extraction.dir = data_dir, raw.dir = data_dir,
                                force.redo = download_again)

}
