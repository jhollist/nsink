#' Get's N-sink data for a given HUC
#'
#' The required datasets for the N-sink analysis are available from multiple,
#' online resources.  This function takes a HUC as input and downloads local
#' copies of those datasets.
#' @param huc A character string of a HUC12 identifier or a HUC12 name.  Currently can run only a single HUC at a time.
#' @param data_dir A directory to store N-Sink data downloads.  Defaults to
#'                 current working directory.
#' @param force Logical to determine if files should be downloaded
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
                           force = FALSE){

  # Check for/create data directory
  if(!dir.exists(data_dir)){dir.create(data_dir)}



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
  attr <- get_nhd_plus(attr_url, data_dir, force)
  erom <- get_nhd_plus(erom_url, data_dir, force)
  nhd <- get_nhd_plus(nhd_url, data_dir, force)
  fdr <- get_nhd_plus(fdr_url, data_dir, force)
  wbd <- get_nhd_plus(wbd_url, data_dir, force)

  # unzip nhdplus data

  run_7z(paste0(data_dir, "/", basename(attr_url)), paste0(data_dir, "/attr"), force)
  run_7z(paste0(data_dir, "/", basename(erom_url)), paste0(data_dir, "/erom"), force)
  run_7z(paste0(data_dir, "/", basename(nhd_url)), paste0(data_dir, "/nhd"), force)
  run_7z(paste0(data_dir, "/", basename(fdr_url)), paste0(data_dir, "/fdr"), force)
  run_7z(paste0(data_dir, "/", basename(wbd_url)), paste0(data_dir, "/wbd"), force)

  # Use actual huc to limit downloads on impervious and ssurgo
  huc_sf <- sf::st_read(paste0(data_dir, "/wbd/WBD_Subwatershed.shp"))
  huc_12 <- huc_sf[huc_sf$HUC_12 == huc | huc_sf$HU_12_NAME == huc, ]
  # This was a hack to get the ssurgo to download via spatial as raw HUC12 on
  # niantic huc was throwing an error with get_ssurgo
  huc_12_buff <- st_buffer(huc_12, 0.01)


  # Get impervious
  imp <- FedData::get_nlcd(as(huc_12_buff, "Spatial"), dataset = "impervious",
                    label = huc, extraction.dir = paste0(data_dir, "/imperv"),
                    raw.dir = paste0(data_dir, "/imperv"),
                    force.redo = force)

  # Get SSURGO area symbol
  # Using HUC as spatial was throwing an error on example niantic huc
  #ss_area <- huc_ssurgo_lookup[huc_ssurgo_lookup$HUC_12 == huc_12$HUC_12, ]$areasymbol

  ssurgo <- FedData::get_ssurgo(as(huc_12_buff, "Spatial"),label = huc,
                                extraction.dir = paste0(data_dir, "/ssurgo"),
                                raw.dir = paste0(data_dir, "/ssurgo"),
                                force.redo = force)

  # Save the merged output as an .rda
  save(ssurgo, file = paste0(getwd(),"/nsink_data/ssurgo/ssurgo.rda"))

}
