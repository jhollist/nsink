#' Calculates N-Sink nitrogen removal percent
#'
#' Starting with base data layers of NHDPlus, SSURGO, impervious surface, flow
#' velocity, and time of travel, this function calculates percentage of Nitrogen
#' removal.  Details for nitrogen removal calculation are from
#' \href{https://doi.org/10.1016/j.ecoleng.2010.02.006}{Kellogg et al. (2010)}.
#' This function assumes data has been downloaded with
#' \code{\link{nsink_get_data}} and has been prepared with
#' \code{\link{nsink_prep_data}}.
#'
#' @param input_data A list of input datasets created with
#'                   \code{\link{nsink_prep_data}}.
#' @return If the rater method is used,  a raster stack with one layer with
#'         nitrogen removal, a second layer with the type of removal (e.g.
#'         hydric soils, lakes, streams), and a third layer with a clean raster
#'         template is returned.  If the hybrid method is used a list is
#'         returned with land_removal, stream_removal, and lake removal.
#'
#' @references Kellogg, D. Q., Gold, A. J., Cox, S., Addy, K., & August, P. V.
#'             (2010). A geospatial approach for assessing denitrification sinks
#'             within lower-order catchments. Ecological Engineering, 36(11),
#'             1596-1606.
#'             \href{https://doi.org/10.1016/j.ecoleng.2010.02.006}{Link}
#'
#' @export
#' @examples
#' \dontrun{
#' library(nsink)
#' niantic_huc <- nsink_get_huc_id("Niantic River")$huc_12
#' niantic_data <- nsink_get_data(niantic_huc)
#' aea <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
#' niantic_nsink_data <- nsink_prep_data(niantic_huc, projection = aea)
#' nsink_calc_removal(niantic_nsink_data)
#' }
nsink_calc_removal <- function(input_data){
  if(all(names(input_data) %in% c("streams","lakes", "fdr", "impervious", "ssurgo",
                            "q", "tot", "huc", "raster_template",
                            "lakemorpho"))){

    removal <- list(land_removal = nsink_calc_land_removal(input_data[c("ssurgo",
                                                         "impervious",
                                                         "raster_template")]),
    stream_removal_r = nsink_calc_stream_removal(input_data[c("streams",
                                                             "q",
                                                             "tot",
                                                             "raster_template")],
                                                 method = "raster"),
    lake_removal_r = nsink_calc_lake_removal(input_data[c("streams",
                                                         "lakes",
                                                         "tot",
                                                         "lakemorpho",
                                                         "raster_template")],
                                             method = "raster"),
    stream_removal_v = nsink_calc_stream_removal(input_data[c("streams",
                                                              "q",
                                                              "tot",
                                                              "raster_template")],
                                                 method = "hybrid"),
    lake_removal_v = nsink_calc_lake_removal(input_data[c("streams",
                                                          "lakes",
                                                          "tot",
                                                          "lakemorpho",
                                                          "raster_template")],
                                             method = "hybrid"),
    raster_template = input_data$raster_template, huc = input_data$huc)

    merged_removal <- nsink_merge_removal(list(land_removal = removal$land_removal,
                                               stream_removal = removal$stream_removal_r,
                                               lake_removal = removal$lake_removal_r,
                                               raster_template = removal$raster_template,
                                               huc = input_data$huc))
    merged_type <- nsink_calc_removal_type(list(land_removal = removal$land_removal,
                                                stream_removal = removal$stream_removal_r,
                                                lake_removal = removal$lake_removal_r,
                                                raster_template = removal$raster_template,
                                                huc = input_data$huc))

    return(list(raster_method = raster::stack(merged_removal, merged_type),
                land_removal = removal$land_removal,
                network_removal = rbind(removal$stream_removal_v,
                                        removal$lake_removal_v)))
  } else {
    stop("The input data do not contain the expected data.  Check the object and
         re-run with nsink_prep_data().")
  }

}

#' Calculates land-based nitrogen removal
#'
#' @param input_data A named list with "ssurgo", "impervious", "lakes", and
#'                   "raster_template".
#' @return raster of land based nitrogen removal
#' @import dplyr sf
#' @keywords internal
nsink_calc_land_removal <- function(input_data){
  land_removal <- mutate(input_data$ssurgo,
                                n_removal = 0.8 * (hydric_pct/100))
  land_removal <- mutate(land_removal, n_removal = case_when(n_removal == 0 ~
                                                          NA_real_,
                                                        TRUE ~ n_removal))
  land_removal_rast <- fasterize::fasterize(land_removal,
                                            input_data$raster_template,
                                            field = "n_removal", background = 0,
                                            fun = "max")
  impervious <- input_data$impervious
  impervious[impervious > 0] <- NA
  impervious[!is.na(impervious)] <- 1

  raster::mask(land_removal_rast, impervious)
}

#' Calculates stream-based nitrogen removal
#'
#' @param input_data  A named list with "streams", "q", "tot", and
#'                   "raster_template".
#' @param method Character indicating which type of data to return.  See
#'             \code{\link{nsink_calc_removal}} for detals
#' @return raster of stream based nitrogen removal
#' @import dplyr sf
#' @keywords internal
nsink_calc_stream_removal <- function(input_data, method = c("raster", "hybrid")){
  method <- match.arg(method)
  stream_removal <- mutate_if(input_data$streams, is.factor, as.character())
  stream_removal <- left_join(stream_removal,
                                     input_data$q,
                                     by = c("stream_comid" = "stream_comid"))
  stream_removal <- left_join(stream_removal,
                                     input_data$tot,
                                     by = c("stream_comid" = "stream_comid"))
  stream_removal <- filter(stream_removal, ftype != "ArtificialPath")
  # TODO When time of travel not available in NHDPlus, need to use other methods
  #      to estimate time of travel (in Kellogg et al.)
  stream_removal <- mutate(stream_removal, totma = case_when(totma == -9999 ~ NA_real_,
                                                    TRUE ~ totma))
  stream_removal <- mutate(stream_removal, n_removal =
                                    (1 - exp(-0.0513 * (mean_reach_depth^-1.319)
                                             * totma))/100)
  if(method == "raster"){
    return(raster::rasterize(stream_removal, input_data$raster_template,
                    field = "n_removal", fun = "max"))
  } else if (method == "hybrid"){
    stream_removal <- select(stream_removal, stream_comid, lake_comid, gnis_name,
                             ftype, n_removal)
    return(stream_removal)
  }
}

#' Calculates lake-based nitrogen removal
#'
#' @param input_data A named list with "streams", "lakes", "tot", "lakemorpho",
#'                   and "raster_template".
#' @param method Character indicating which type of data to return.  See
#'             \code{\link{nsink_calc_removal}} for detals
#' @return raster of lake based nitrogen removal
#' @import dplyr sf
#' @keywords internal
nsink_calc_lake_removal <- function(input_data, method = c("raster", "hybrid")){
  method <- match.arg(method)
  residence_time <- left_join(input_data$streams, input_data$tot)
  residence_time <- filter(residence_time, lake_comid > 0)
  # TODO When time of travel not available in NHDPlus, need to use other methods
  #      to calculate residence time (in Kellogg et al)
  residence_time <- mutate(residence_time, totma = case_when(totma == -9999 ~
                                                               NA_real_,
                                                             TRUE ~ totma))
  residence_time <- group_by(residence_time, lake_comid)
  residence_time <- summarize(residence_time, lake_residence_time_yrs =
                                       sum(totma*0.002737851))
  residence_time <- ungroup(residence_time)
  residence_time_sf <- residence_time
  st_geometry(residence_time) <- NULL

  lake_removal <- left_join(input_data$lakes, input_data$lakemorpho)
  lake_removal <- left_join(lake_removal, residence_time)
  lake_removal <- mutate(lake_removal, meandused = case_when(meandused < 0 ~ NA_real_,
                                                      TRUE ~ meandused))
  lake_removal <- mutate(lake_removal, n_removal =
                           (79.24 - (33.26 * log10(meandused/lake_residence_time_yrs)))/100)
  lake_removal <- mutate(lake_removal, n_removal = case_when(n_removal < 0 ~ 0,
                                                      TRUE ~ n_removal))

  lake_removal_sf <- lake_removal


  st_geometry(lake_removal) <- NULL
  lake_removal_flowpath <- left_join(residence_time_sf, lake_removal)
  if(method == "raster"){
    return(fasterize::fasterize(lake_removal_sf, input_data$raster_template,
                       field = "n_removal", fun = "max"))
  } else if (method == "hybrid"){
    comids <- select(input_data$streams, stream_comid, lake_comid)
    st_geometry(comids) <- NULL
    lake_removal_flowpath <- left_join(lake_removal_flowpath, comids)
    lake_removal_flowpath <- select(lake_removal_flowpath, stream_comid, lake_comid, gnis_name,
                           ftype, n_removal)
    return(lake_removal_flowpath)
  }
}

#' Merges removal rasters into single raster
#'
#' @param removal_rasters A named list of "land_removal", "stream_removal,
#'                        "lake_removal", and "raster_template" rasters plus a
#'                        sf object "huc".
#' @return raster of landscape nitrogen removal
#' @keywords internal
nsink_merge_removal <- function(removal_rasters){
  removal <- raster::merge(removal_rasters$lake_removal,
                           removal_rasters$land_removal)
  removal <- raster::mask(removal, as(removal_rasters$huc, "Spatial"))
  removal[is.na(removal)] <- 0
  removal <- raster::focal(removal, matrix(1, nrow = 3, ncol = 3), max)
  removal <- raster::projectRaster(removal, removal_rasters$raster_template,
                                   method = "ngb")
  removal <- raster::merge(removal_rasters$stream_removal, removal)
  removal
}

#' Create removal type raster
#'
#' @param removal_rasters A named list of "land_removal", "stream_removal,
#'                        "lake_removal", and "raster_template" rasters plus a
#'                        sf object "huc".
#' @return raster of landscape nitrogen removal
#' @keywords internal
nsink_calc_removal_type <- function(removal_rasters){
  type_it <- function(removal_rast, type = c("hydric","stream", "lake")){
    type <- match.arg(type)
    if(type == "hydric"){
      val <- raster::getValues(removal_rast)
      val[val > 0] <- 1
      val[val == 0] <- NA
    } else if(type == "stream"){
      val <- raster::getValues(removal_rast)
      val[!is.na(val)] <- 2
    } else if(type == "lake"){
      val <- raster::getValues(removal_rast)
      val[!is.na(val)]  <- 3
    }
    raster::setValues(removal_rast, val)
  }

  hydric_type <- type_it(removal_rasters$land_removal, "hydric")
  stream_type <- type_it(removal_rasters$stream_removal, "stream")
  lake_type <- type_it(removal_rasters$lake_removal, "lake")

  types <- raster::merge(lake_type, hydric_type)
  types <- raster::mask(types, removal_rasters$huc)
  types[is.na(types)] <- 0
  types <- raster::focal(types,  matrix(1,nrow = 3, ncol = 3), max)
  types <- raster::projectRaster(types, removal_rasters$raster_template,
                                 method = "ngb")
  types <- raster::merge(stream_type, types)
  types
}
