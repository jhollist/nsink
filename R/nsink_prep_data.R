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
#' @param projection EPSG code as an integer or proj4 string as a character
#' @param data_dir Base directory that contains N-Sink data folders.  Data may
#'                 be downloaded with the \code{\link{nsink_get_data}} function.
#' @return returns a list of sf, raster, or tabular objects for each of the
#'         required datasets plus the huc.
#' @export
#' @import sf
#' @examples
#' \dontrun{
#' library(nsink)
#' aea <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
#' epsg <- 3748L
#' niantic_huc <- nsink_get_huc_id("Niantic River")$huc_12
#' nsink_prep_data(huc = niantic_huc, projection = aea)
#' nsink_prep_data(huc = niantic_huc, projection = epsg)
#' }
nsink_prep_data <- function(huc, projection,
                            data_dir = normalizePath("nsink_data/")) {
  # Check for/create/clean data directory
  data_dir <- nsink_fix_data_directory(data_dir)
  dirs <- list.dirs(data_dir, full.names = FALSE, recursive = FALSE)
  if (all(c("attr", "erom", "fdr", "imperv", "nhd", "ssurgo", "wbd", "nlcd") %in% dirs)) {
    huc_sf <- st_read(paste0(data_dir, "wbd/WBD_Subwatershed.shp"))
    huc_sf <- huc_sf[huc_sf$HUC_12 == huc, ]
    huc_sf <- group_by(huc_sf, HUC_12)
    huc_sf <- summarize(huc_sf, huc_12 = unique(as.character(HUC_12)))
    huc_sf <- ungroup(huc_sf)
    huc_sf <- st_transform(huc_sf, crs = projection)
    huc_raster <- fasterize::raster(as(huc_sf, "Spatial"), resolution = 30)
    list(
      streams = nsink_prep_streams(huc_sf, data_dir),
      lakes = nsink_prep_lakes(huc_sf, data_dir),
      fdr = nsink_prep_fdr(huc_sf, huc_raster, data_dir),
      impervious = nsink_prep_impervious(huc_sf, huc_raster, data_dir),
      nlcd = nsink_prep_nlcd(huc_sf, huc_raster, data_dir),
      ssurgo = nsink_prep_ssurgo(huc_sf, data_dir),
      q = nsink_prep_q(data_dir),
      tot = nsink_prep_tot(data_dir),
      lakemorpho = nsink_prep_lakemorpho(data_dir),
      huc = huc_sf,
      raster_template = huc_raster
    )
  } else {
    stop(paste0(
      "The required data does not appear to be available in ",
      data_dir, ". Run nsink_get_data()."
    ))
  }
}

#' Prepare streams data for N-Sink
#'
#' Standardizes streams data by transforming data, clipping to HUC, ...
#'
#' @param huc_sf An sf object of the Watershed Boundaries Dataset HUC12
#' @param data_dir Base directory that contains N-Sink data folders.  Data may
#'                 be downloaded with the \code{\link{nsink_get_data}} function.
#' @return returns an sf object of the NHDPlus streams for the huc_sf
#' @import dplyr sf
#' @keywords  internal
nsink_prep_streams <- function(huc_sf, data_dir) {
  if (file.exists(paste0(data_dir, "nhd/NHDFlowline.shp"))) {
    streams <- st_read(paste0(data_dir, "nhd/NHDFlowline.shp"))
    streams <- st_transform(streams, st_crs(huc_sf))
    streams <- st_zm(streams)
    streams <- rename_all(streams, tolower)
    streams <- rename(streams,
      stream_comid = comid,
      lake_comid = wbareacomi
    )
    streams <- slice(streams, st_contains(huc_sf, streams)[[1]])
    streams <- st_crop(streams, st_bbox(huc_sf))
    streams <- mutate_if(streams, is.factor, as.character())
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
#' @import dplyr sf
#' @keywords  internal
nsink_prep_lakes <- function(huc_sf, data_dir) {
  if (file.exists(paste0(data_dir, "nhd/NHDWaterbody.shp"))) {
    lakes <- st_read(paste0(data_dir, "nhd/NHDWaterbody.shp"))
    lakes <- st_transform(lakes, st_crs(huc_sf))
    lakes <- rename_all(lakes, tolower)
    lakes <- rename(lakes, lake_comid = comid)
    lakes <- filter(lakes, ftype == "LakePond")
    lakes <- slice(lakes, st_contains(huc_sf, lakes)[[1]])
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
nsink_prep_fdr <- function(huc_sf, huc_raster, data_dir) {
  if (dir.exists(paste0(data_dir, "fdr"))) {
    fdr <- raster::raster(paste0(data_dir, "fdr"))
    fdr <- raster::projectRaster(fdr, huc_raster, method = "ngb")
    fdr <- raster::crop(fdr, as(huc_sf, "Spatial"))
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
nsink_prep_impervious <- function(huc_sf, huc_raster, data_dir) {
  huc12 <- unique(as.character(huc_sf$HUC_12))
  if (file.exists(paste0(
    data_dir, "imperv/",
    "NLCD_2016_Impervious_L48.tif"
  ))) {
    impervious <- raster::raster(paste0(
      data_dir, "imperv/",
      "NLCD_2016_Impervious_L48.tif"
    ))
    impervious <- raster::projectRaster(impervious, huc_raster)
  } else {
    stop("The required data file does not exist.  Run nsink_get_data().")
  }
  impervious
}

#' Prepare NLCD data for N-Sink
#'
#' Standardizes NLCD data by transforming data, clipping to HUC, ...
#'
#' @param huc_sf An sf object of the Watershed Boundaries Dataset HUC12
#' @param huc_raster A raster object of the Watershed Boundaries Dataset HUC12
#' @param data_dir Base directory that contains N-Sink data folders.  Data may
#'                 be downloaded with the \code{\link{nsink_get_data}} function.
#' @return returns a raster object of the NLCD for the huc_sf
#' @keywords  internal
nsink_prep_nlcd <- function(huc_sf, huc_raster, data_dir) {
  huc12 <- unique(as.character(huc_sf$HUC_12))
  if (file.exists(paste0(
    data_dir, "nlcd/",
    "NLCD_2016_Land_Cover_L48.tif"
  ))) {
    nlcd <- raster::raster(paste0(
      data_dir, "nlcd/",
      "NLCD_2016_Land_Cover_L48.tif"
    ))
    nlcd <- raster::projectRaster(nlcd, huc_raster, method = "ngb")
  } else {
    stop("The required data file does not exist.  Run nsink_get_data().")
  }
  nlcd
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
#' @import dplyr sf
#' @keywords  internal
nsink_prep_ssurgo <- function(huc_sf, data_dir) {
  huc12 <- unique(as.character(huc_sf$HUC_12))
  if (file.exists(paste0(
    data_dir, "ssurgo/", huc12,
    "_SSURGO_Mapunits.shp"
  ))) {
    ssurgo <- st_read(paste0(
      data_dir, "ssurgo/",
      huc12,
      "_SSURGO_Mapunits.shp"
    ))
    ssurgo <- st_transform(ssurgo, st_crs(huc_sf))
    ssurgo <- rename_all(ssurgo, tolower)
    ssurgo <- mutate(ssurgo, mukey = as(mukey, "character"))
    ssurgo_tbl <- read.csv(paste0(
      data_dir, "ssurgo/", huc12,
      "_SSURGO_component.csv"
    ))
    ssurgo_tbl <- mutate(ssurgo_tbl, mukey = as(mukey, "character"))
    ssurgo_tbl <- select(
      ssurgo_tbl, mukey, cokey, hydricrating,
      comppct.r
    )
    ssurgo_tbl <- filter(ssurgo_tbl, hydricrating == "Yes")
    ssurgo_tbl <- group_by(ssurgo_tbl, mukey, hydricrating)
    ssurgo_tbl <- summarize(ssurgo_tbl, hydric_pct = sum(comppct.r))
    ssurgo_tbl <- ungroup(ssurgo_tbl)
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
#' @import dplyr
#' @keywords  internal
nsink_prep_q <- function(data_dir) {
  if (file.exists(paste0(data_dir, "erom/EROM_MA0001.DBF"))) {
    q <- foreign::read.dbf(paste0(data_dir, "erom/EROM_MA0001.DBF"))
    q <- select(q, stream_comid = ComID, q_cfs = Q0001E)
    q <- mutate(q,
      q_cms = q_cfs * 0.028316846592,
      mean_reach_depth = 0.2612 * (q_cms^0.3966)
    )
    q <- mutate_if(q, is.factor, as.character())
  } else {
    stop("The required data file does not exist.  Run nsink_get_data().")
  }
  as_tibble(q)
}

#' Prepare time of travel data for N-Sink
#'
#' Standardizes time of travel from the VAA tables.
#'
#' @param data_dir Base directory that contains N-Sink data folders.  Data may
#'                 be downloaded with the \code{\link{nsink_get_data}} function.
#' @return returns a tibble of the time of travel data
#' @import dplyr
#' @keywords  internal
nsink_prep_tot <- function(data_dir) {
  if (file.exists(paste0(data_dir, "attr/PlusFlowlineVAA.dbf"))) {
    tot <- foreign::read.dbf(paste0(data_dir, "attr/PlusFlowlineVAA.dbf"))
    tot <- rename_all(tot, tolower)
    tot <- select(tot, stream_comid = comid, totma = totma, fromnode, tonode)
    tot <- mutate_if(tot, is.factor, as.character())
  } else {
    stop("The required data file does not exist.  Run nsink_get_data().")
  }
  as_tibble(tot)
}

#' Prepare lake morphology data for N-Sink
#'
#' Standardizes lake morphology  from the lake morphology tables.
#'
#' @param data_dir Base directory that contains N-Sink data folders.  Data may
#'                 be downloaded with the \code{\link{nsink_get_data}} function.
#' @return returns a tibble of the lake morphology data
#' @import dplyr
#' @keywords  internal
nsink_prep_lakemorpho <- function(data_dir) {
  if (file.exists(paste0(data_dir, "attr/PlusWaterbodyLakeMorphology.dbf"))) {
    lakemorpho <- foreign::read.dbf(paste0(
      data_dir,
      "attr/PlusWaterbodyLakeMorphology.dbf"
    ))
    lakemorpho <- rename_all(lakemorpho, tolower)
    lakemorpho <- rename(lakemorpho, lake_comid = comid)
    lakemorpho <- mutate_if(lakemorpho, is.factor, as.character())
  } else {
    stop("The required data file does not exist.  Run nsink_get_data().")
  }
  as_tibble(lakemorpho)
}
