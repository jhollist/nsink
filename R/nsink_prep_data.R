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
#' @param projection EPSG code as an numeric or WKT as a character.
#'                   This much be a projected CRS and not geographic as many of
#'                   the measurements required for the nsink analysis require
#'                   reliable length and area measurments.
#' @param data_dir Base directory that contains N-Sink data folders.  Data may
#'                 be downloaded with the \code{\link{nsink_get_data}} function.
#' @return returns a list of sf, raster, or tabular objects for each of the
#'         required datasets plus the huc.
#' @importFrom methods as
#' @importFrom rlang .data
#' @export
#' @import sf
#' @examples
#' \dontrun{
#' library(nsink)
#' aea <- 5072
#' niantic_huc <- nsink_get_huc_id("Niantic River")$huc_12
#' niantic_nsink_data <- nsink_prep_data(huc = niantic_huc, projection = aea,
#' data_dir = "nsink_data")
#' # Example using EPSG code for projection
#' epsg <- 3748L
#' niantic_nsink_data <- nsink_prep_data(huc = niantic_huc, projection = epsg,
#'                 data_dir = "nsink_data")
#' }
nsink_prep_data <- function(huc, projection,
                            data_dir = normalizePath("nsink_data/", winslash = "/")) {

  # Get vpu
  rpu <- unique(wbd_lookup[grepl(paste0("^", huc), wbd_lookup$HUC_12),]$RPU)
  if(length(rpu) > 1){stop("More than 1 rpu selected.  This is not yet supported")}
  rpu <- rpu[!is.na(rpu)]

  #Add RPU to data_dir
  data_dir_orig <- data_dir
  while(grepl(rpu, basename(data_dir))){
    data_dir <- dirname(data_dir)
    message("The RPU should not be included in the data directory as it is handled internally by nsink.")
  }
  data_dir <- paste(basename(data_dir), rpu, sep = "/")

  # Check for/create/clean data directory
  data_dir <- nsink_fix_data_directory(data_dir)

  # Check for/create/clean data directory
  message("Preparing data for nsink analysis...")

  dirs <- list.dirs(data_dir, full.names = FALSE, recursive = FALSE)
  if (all(c("attr", "erom", "fdr", "imperv", "nhd", "ssurgo", "wbd", "nlcd") %in% dirs)) {
    huc_sf <- st_read(paste0(data_dir, "wbd/WBD_Subwatershed.shp"),
                      quiet = TRUE)

    huc_sf <- huc_sf[grepl(paste0("^", huc), huc_sf$HUC_12), ]
    huc_sf <- mutate(huc_sf, selected_huc = huc)
    huc_sf <- group_by(huc_sf, .data$selected_huc)
    huc_sf <- summarize(huc_sf, selected_huc = unique(.data$selected_huc))
    huc_sf <- ungroup(huc_sf)
    huc_sf <- st_transform(huc_sf, crs = projection)
    # Use SSURGO to pull out salt water ssurgo poly's
    huc_sf <- nsink_remove_openwater(huc_sf, data_dir)
    res <- units::set_units(30, "m")
    res <- units::set_units(res, st_crs(huc_sf, parameters = TRUE)$ud_unit,
                            mode = "standard")
    huc_raster <- raster::raster(as(huc_sf, "Spatial"),
                                                     resolution = as.numeric(res),
                                                     crs = projection(huc_sf))

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
#' @importFrom rlang .data
#' @keywords  internal
nsink_prep_streams <- function(huc_sf, data_dir) {

  if (file.exists(paste0(data_dir, "nhd/NHDFlowline.shp"))) {

    message("Preparing streams...")
    streams <- st_read(paste0(data_dir, "nhd/NHDFlowline.shp"), quiet = TRUE)
    streams <- st_transform(streams, st_crs(huc_sf))
    streams <- st_zm(streams)
    streams <- rename_all(streams, tolower)
    streams <- rename(streams,
      stream_comid = .data$comid,
      lake_comid = .data$wbareacomi
    )
    # Remove coastline
    streams <- filter(streams, .data$ftype != "Coastline")
    streams <- mutate(streams,
                      percent_length =
                        units::set_units(st_length(.data$geometry), "km")/
                        units::set_units(.data$lengthkm, "km"))
    suppressWarnings(streams <- st_intersection(huc_sf, streams))
    streams <- filter(streams, percent_length >= units::as_units(0.75))
    streams <- select(streams, -.data$lengthkm, -.data$shape_leng, -.data$percent_length)
    st_agr(streams) <- "constant"
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
#' @importFrom rlang .data
#' @keywords  internal
nsink_prep_lakes <- function(huc_sf, data_dir) {
  if (file.exists(paste0(data_dir, "nhd/NHDWaterbody.shp"))) {
    message("Preparing lakes...")
    lakes <- st_read(paste0(data_dir, "nhd/NHDWaterbody.shp"), quiet = TRUE)
    lakes <- st_transform(lakes, st_crs(huc_sf))
    lakes <- rename_all(lakes, tolower)
    lakes <- rename(lakes, lake_comid = .data$comid)
    lakes <- filter(lakes, .data$ftype == "LakePond")
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
#' @return returns a raster object of the flow direction for the huc_sf but in
#'         the original fdr projection
#' @importFrom methods as
#' @keywords  internal
nsink_prep_fdr <- function(huc_sf, huc_raster, data_dir) {

  if (dir.exists(paste0(data_dir, "fdr"))) {
    message("Preparing flow direction...")
    fdr <- raster::raster(paste0(data_dir, "fdr"))
    huc_sf <- st_transform(huc_sf, st_crs(fdr))
    fdr <- raster::crop(fdr, as(huc_sf, "Spatial"))
    fdr <- raster::mask(fdr, as(huc_sf, "Spatial"))
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
  huc12 <- unique(as.character(huc_sf$selected_huc))
  file <- list.files(paste0(data_dir, "imperv/"), pattern = ".tif")
  if (any(grepl("NLCD_2016_Impervious_L48", file))){
    message("Preparing impervious...")

    if(length(file)>1){
      file <- file[grepl(paste0("^",huc12,"_"),file)]
    }
    impervious <- raster::raster(paste0(data_dir, "imperv/", file))
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
  huc12 <- unique(as.character(huc_sf$selected_huc))
  file <- list.files(paste0(data_dir, "nlcd/"), pattern = ".tif")
  if (any(grepl("NLCD_2016_Land_Cover_L48", file))){
    message("Preparing NLCD...")
    if(length(file)>1){
      file <- file[grepl(paste0("^",huc12,"_"),file)]
    }
    nlcd <- raster::raster(paste0(data_dir, "nlcd/", file))
    nlcd <- raster::projectRaster(nlcd, huc_raster,method = "ngb")
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
#' @importFrom methods as
#' @importFrom utils read.csv
#' @importFrom rlang .data
#' @keywords  internal
nsink_prep_ssurgo <- function(huc_sf, data_dir) {

  huc12 <- unique(as.character(huc_sf$selected_huc))

  if (file.exists(paste0(data_dir, "ssurgo/", huc12,"_SSURGO_Mapunits.shp"))) {
    message("Preparing SSURGO...")
    ssurgo <- st_read(paste0(data_dir, "ssurgo/", huc12,
                             "_SSURGO_Mapunits.shp"), quiet = TRUE)
    ssurgo_tbl <- read.csv(paste0(
      data_dir, "ssurgo/", huc12,
      "_SSURGO_component.csv"
    ))
  } else if(file.exists(paste0(data_dir, "ssurgo/", huc12, "_ssurgo.gpkg"))){
    message("Preparing SSURGO...")
    ssurgo <- st_read(paste0(data_dir, "ssurgo/", huc12, "_ssurgo.gpkg"),
                      layer = "geometry", quiet = TRUE)
    ssurgo_tbl <- st_read(paste0(data_dir, "ssurgo/", huc12, "_ssurgo.gpkg"),
                          layer = "component", quiet = TRUE)
  } else {
    stop("The required data file does not exist.  Run nsink_get_data().")
  }
  ssurgo <- st_transform(ssurgo, st_crs(huc_sf))
  ssurgo <- rename_all(ssurgo, tolower)
  ssurgo <- mutate(ssurgo, mukey = as(.data$mukey, "character"))
  ssurgo_tbl <- mutate(ssurgo_tbl, mukey = as(.data$mukey, "character"))
  ssurgo_tbl <- select(
    ssurgo_tbl, .data$mukey, .data$cokey, .data$hydricrating,
    .data$comppct.r, .data$compname, .data$drainagecl, .data$compkind,
    .data$localphase
  )



  # Limiting hydric removal to only land-based sources of removal
  # i.e. no removal from water polys in SSURGO and none from subaqueous soils
  ssurgo_tbl <- mutate(ssurgo_tbl, hydricrating =
                         case_when(.data$compname == "Water" ~
                                     "No",
                                   .data$drainagecl == "Subaqueous" ~
                                     "No",
                                   TRUE ~ hydricrating))

  hydric_tbl <- filter(ssurgo_tbl, .data$hydricrating == "Yes")
  hydric_tbl <- group_by(hydric_tbl, .data$mukey, .data$hydricrating)
  hydric_tbl <- summarize(hydric_tbl, hydric_pct = sum(.data$comppct.r))
  hydric_tbl <- ungroup(hydric_tbl)
  ssurgo <- full_join(ssurgo, hydric_tbl, by = "mukey")
  ssurgo <- filter(ssurgo, .data$musym != "Ws")
  ssurgo <- select(ssurgo, .data$areasymbol, .data$spatialver, .data$musym,
                   .data$mukey, .data$hydricrating, .data$hydric_pct)
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
#' @importFrom rlang .data
#' @keywords  internal
nsink_prep_q <- function(data_dir) {
  if (file.exists(paste0(data_dir, "erom/EROM_MA0001.DBF"))) {
    message("Preparing stream flow...")
    q <- foreign::read.dbf(paste0(data_dir, "erom/EROM_MA0001.DBF"))
    q <- select(q, stream_comid = .data$ComID, q_cfs = .data$Q0001E)
    q <- mutate(q,
      q_cms = .data$q_cfs * 0.028316846592,
      mean_reach_depth = 0.2612 * (.data$q_cms^0.3966)
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
#' @importFrom rlang .data
#' @keywords  internal
nsink_prep_tot <- function(data_dir) {
  if (file.exists(paste0(data_dir, "attr/PlusFlowlineVAA.dbf"))) {
    message("Preparing time of travel...")
    tot <- foreign::read.dbf(paste0(data_dir, "attr/PlusFlowlineVAA.dbf"))
    tot <- rename_all(tot, tolower)
    tot <- select(tot, stream_comid = .data$comid, totma = .data$totma,
                  .data$fromnode, .data$tonode, stream_order = .data$streamorde)
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
#' @importFrom rlang .data
#' @keywords  internal
nsink_prep_lakemorpho <- function(data_dir) {
  if (file.exists(paste0(data_dir, "attr/PlusWaterbodyLakeMorphology.dbf"))) {
    message("Preparing lake morphometry...")
    lakemorpho <- foreign::read.dbf(paste0(
      data_dir,
      "attr/PlusWaterbodyLakeMorphology.dbf"
    ))
    lakemorpho <- rename_all(lakemorpho, tolower)
    lakemorpho <- rename(lakemorpho, lake_comid = .data$comid)
    lakemorpho <- mutate_if(lakemorpho, is.factor, as.character())
  } else {
    stop("The required data file does not exist.  Run nsink_get_data().")
  }
  as_tibble(lakemorpho)
}


#' Remove open water portions of the HUC
#'
#' Uses SSURGO polys and the SSURGO musym = Ws to check for and remove any
#' portion of a HUC that is actually salt water.  This should account for the
#' coastal HUCs with large areas of open water.
#'
#' @param huc_sf An sf object of the Watershed Boundaries Dataset HUC12
#' @param data_dir Base directory that contains N-Sink data folders.  Data may
#'                 be downloaded with the \code{\link{nsink_get_data}} function.
#' @return returns a sf object of the HUC without salt water/open water area.
#' @import dplyr sf
#' @importFrom methods as
#' @importFrom utils read.csv
#' @importFrom rlang .data
#' @keywords  internal
nsink_remove_openwater <- function(huc_sf, data_dir){

  huc12 <- unique(as.character(huc_sf$selected_huc))
  if (file.exists(paste0(data_dir, "ssurgo/", huc12,"_SSURGO_Mapunits.shp"))) {

    ssurgo <- st_read(paste0(data_dir, "ssurgo/", huc12,
                             "_SSURGO_Mapunits.shp"), quiet = TRUE)
    ssurgo_tbl <- read.csv(paste0(
      data_dir, "ssurgo/", huc12,
      "_SSURGO_mapunit.csv"
    ))
  } else if(file.exists(paste0(data_dir, "ssurgo/", huc12, "_ssurgo.gpkg"))){
      ssurgo <- st_read(paste0(data_dir, "ssurgo/", huc12, "_ssurgo.gpkg"),
                        layer = "geometry", quiet = TRUE)
      ssurgo_tbl <-
        st_read(paste0(data_dir, "ssurgo/", huc12, "_ssurgo.gpkg"),
                layer = "component", quiet = TRUE)
  } else {
    stop("The required data file does not exist.  Run nsink_get_data().")
  }

  ssurgo <- st_transform(ssurgo, st_crs(huc_sf))
  ssurgo <- rename_all(ssurgo, tolower)
  ssurgo <- mutate(ssurgo, mukey = as(.data$mukey, "character"))
  ssurgo_tbl <- mutate(ssurgo_tbl, mukey = as(.data$mukey, "character"))
  ssurgo <- full_join(ssurgo, ssurgo_tbl, by = "mukey")
  saltwater <- filter(ssurgo, .data$musym == "Ws")
  if(nrow(saltwater) > 0){
    huc_unit <- st_crs(huc_sf, parameters = TRUE)$ud_unit
    tol1 <- units::set_units(2, "m")
    tol1 <- units::set_units(tol1, huc_unit,
                             mode = "standard")
    pixel_area <- units::set_units(900, "m2")
    pixel_area <- units::set_units(pixel_area,
                                   huc_unit*huc_unit,
                                   mode = "standard")
    saltwater_buff <- st_buffer(saltwater, tol1)
    huc_ow_remove <- st_difference(st_union(huc_sf), st_union(saltwater_buff))
    huc_ow_remove <- st_cast(huc_ow_remove, "POLYGON")
    huc_ow_remove <- huc_ow_remove[st_area(huc_ow_remove) > pixel_area,]
  } else {
    huc_ow_remove <- huc_sf
  }
  st_as_sf(huc_ow_remove, data.frame(selected_huc = huc12))
}
