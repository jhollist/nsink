#' Get's N-Sink data for a given HUC
#'
#' The required datasets for the N-sink analysis are available from multiple,
#' online resources.  This function takes a HUC as input and downloads local
#' copies of those datasets.
#' @param huc A character string of a HUC12 identifier.  Currently can run only a single HUC at a time.
#' @param data_dir A directory to store N-Sink data downloads.  Defaults to
#'                 current working directory.
#' @param force Logical to determine if files should be downloaded
#'                       again if they already exist locally.
#' @export
#' @return Returns a list with the huc used and the directory where the data is
#'         stored.
#' @examples
#' \dontrun{
#' library(nsink)
#' niantic_huc <- nsink_get_huc_id("Niantic River")$huc_12
#' nsink_get_data(huc = niantic_huc)
#' }
nsink_get_data <- function(huc, data_dir = paste0(getwd(),"/nsink_data"),
                           force = FALSE){

  huc <- as.character(huc)
  if(nchar(gsub("[[:alpha:]]+","", huc)) != 12) {
    stop("The supplied huc does not appear to be a 12 digit value")
  }

  # Check for/create data directory
  if(!dir.exists(data_dir)){dir.create(data_dir)}

  # Get vpu
  vpu <- wbd_lookup[wbd_lookup$HUC_12 == huc,]$VPUID
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
  suppressMessages({
  nsink_run_7z(paste0(data_dir, "/", basename(attr_url)), paste0(data_dir, "/attr"), force)
  nsink_run_7z(paste0(data_dir, "/", basename(erom_url)), paste0(data_dir, "/erom"), force)
  nsink_run_7z(paste0(data_dir, "/", basename(nhd_url)), paste0(data_dir, "/nhd"), force)
  nsink_run_7z(paste0(data_dir, "/", basename(fdr_url)), paste0(data_dir, "/fdr"), force)
  nsink_run_7z(paste0(data_dir, "/", basename(wbd_url)), paste0(data_dir, "/wbd"), force)
  })
browser()
  # Use actual huc to limit downloads on impervious and ssurgo
  huc_sf <- sf::st_read(paste0(data_dir, "/wbd/WBD_Subwatershed.shp"))
  huc_12 <- huc_sf[huc_sf$HUC_12 == huc, ]

  # Get impervious
  imp <- FedData::get_nlcd(as(huc_12, "Spatial"), dataset = "impervious",
                    label = huc, extraction.dir = paste0(data_dir, "/imperv"),
                    raw.dir = paste0(data_dir, "/imperv"),
                    force.redo = force)

  # Get SSURGO
  # This would occasional have connection reset and FedData would throw
  # an error.  Connection would eventually work.  This code repeats it until it works
  # I call this my "definition of insanity" method
  repeat_it <- TRUE

  while(is.logical(repeat_it)){

    repeat_it <- tryCatch(ssurgo <- FedData::get_ssurgo(as(huc_12, "Spatial"),
                                                        label = huc,
                                                        extraction.dir =
                                                          normalizPath(paste0(data_dir, "/ssurgo")),
                                                        raw.dir = normalizPath(paste0(data_dir,
                                                                         "/ssurgo")),
                                                        force.redo = force),
             error = function(e) TRUE)
  }




  # Return a list with the huc and the data_dir
  list(huc = huc, data_dir = data_dir)
}

#' Look up HUC 12 ID from a HUC name
#'
#' This function takes a HUC Name and returns all possible matching HUC 12 IDs.
#'
#' @param huc_name character string of a HUC Name
#' @return A data frame with HUC_12 and HU_12_NAME that match the huc_name
#' @importFrom dplyr tibble
#' @export
#' @examples
#' nsink_get_huc_id(huc_name = "Niantic River")
nsink_get_huc_id <- function(huc_name){
  idx <- stringr::str_detect(wbd_lookup$HU_12_NAME,huc_name)
  idx[is.na(idx)] <- FALSE
  wbd_match <- wbd_lookup[idx,]
  tibble(huc_12 = wbd_match$HUC_12, huc_12_name = wbd_match$HU_12_NAME,
         state = wbd_match$STATES)
}
