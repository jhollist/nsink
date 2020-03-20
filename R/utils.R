#' function to download nhd files
#' @param download_url url to download
#' @param data_dir The data dir
#' @param force Force new download
#' @keywords internal
get_nhd_plus <- function(download_url,
                         data_dir = normalizePath("nsink_data/"),
                         download_again = FALSE) {
  if (!file.exists(paste0(data_dir, basename(download_url))) | download_again) {
    message(paste0("Downloading ", basename(download_url), " to ", data_dir))
    down <- httr::GET(download_url,
      httr::write_disk(paste0(data_dir, basename(download_url))),
      overwrite = TRUE, httr::progress()
    )
  } else {
    message("File, ", basename(download_url), " already downloaded. \nTo force another download of the file, set the force argument to TRUE. \n")
  }
}

#' Get remote path for NHDPlus components
#'
#' Code modified from https://github.com/jsta/nhdR/blob/master/R/utils.R.  Still
#' need to figure out best way to acknowledge jsta as author and include GPL
#'
#' @param rpu raster processing unit for NHDPlus. available in nsink:::wbd_lookup
#' @param component which component to download
#' @keywords internal

nsink_get_plus_remotepath <- function(rpu, component = c(
                                        "NHDSnapshot",
                                        "FdrFac",
                                        "EROMExtension",
                                        "WBDSnapshot",
                                        "NHDPlusAttributes"
                                      )) {
  component <- match.arg(component)
  if (component == "NHDSnapshot") {
    component <- "NHDSnapshot_04"
  }
  if (component == "FdrFac") {
    component <- paste0(rpu, "_FdrFac_01")
  }
  if (component == "EROMExtension") {
    component <- "EROMExtension_06"
  }
  if (component == "WBDSnapshot") {
    component <- "WBDSnapshot_03"
  }
  if (component == "NHDPlusAttributes") {
    component <- "NHDPlusAttributes_09"
  }


  url_components <- wbd_lookup[wbd_lookup$RPU == rpu, ]
  url_components <- select(url_components, DrainageID, VPUID, RPU)
  url_components <- unique(url_components)
  baseurl <- paste0(
    "https://s3.amazonaws.com/edap-nhdplus/NHDPlusV21/Data/NHDPlus",
    url_components$DrainageID
  )
  url1 <- paste0(
    baseurl,
    "/NHDPlusV21_",
    url_components$DrainageID, "_",
    url_components$VPUID, "_",
    component, ".7z"
  )
  url2 <- paste0(
    baseurl,
    "/NHDPlus",
    url_components$VPUID,
    "/NHDPlusV21_",
    url_components$DrainageID, "_",
    url_components$VPUID, "_",
    component, ".7z"
  )

  if (!httr::http_error(url1)) {
    return(url1)
  } else if (!httr::http_error(url2)) {
    return(url2)
  } else {
    stop(paste("Neither", url1, "or", url2, "is a valid URL"))
  }
}


#' Finds 7-zip
#'
#' This code is modified from https://github.com/jsta/nhdR/blob/master/R/utils.R
#' and https://github.com/jsta/nhdR/blob/master/R/get.R to determine if 7 zip is
#' available.  If available it unzips a 7z zipfile to a destination directory.
#' This avoids needing to use archive package which is only available via
#' GitHub.  Need to acknowledge jsta as original author.
#'
#' @param zipfile The zipfile to be extracted
#' @param destdir Where to put the extracted files
#' @param force Whether or not to extract again if the destination files
#'                      already exist
#' @keywords internal

nsink_run_7z <- function(zipfile, destdir, extract_again = FALSE) {
  paths_7z <- c(
    "7z",
    path.expand("~/usr/bin/7z"),
    "C:\\PROGRA~1\\7-Zip\\7za",
    "C:\\PROGRA~1\\7-Zip\\7z.exe"
  )

  if (!any(nchar(Sys.which(paths_7z)) > 0)) {
    stop("The 7-zip program is needed to unpack NHDPlus downloads (https://www.7-zip.org/).")
  }

  path_7z <- paths_7z[nchar(Sys.which(paths_7z)) > 0][1]
  if (!dir.exists(destdir) | extract_again) {
    system(paste0(path_7z, " e ", shQuote(zipfile), " -aos -o", shQuote(destdir)))
  } else {
    message(paste0("It appears you have already extracted", zipfile, "\nIf you would like to force another extraction, set force = TRUE."))
  }
}

#' Fix the data directory
#'
#' This funciton takes the data directory and checks for existence, creates it
#' if it doesn't exist, then adds a trailing slash and normalizes the path for
#' the operating system
#'
#' @param data_dir the data directory
#' @return a string with the normalized path
#' @keywords internal
nsink_fix_data_directory <- function(data_dir) {
  if (!dir.exists(data_dir)) {
    dir.create(data_dir)
  }
  if (!grepl("\\\\$|/$", data_dir)) {
    data_dir <- paste0(data_dir, "/")
  }
  data_dir <- normalizePath(data_dir)
  data_dir
}
