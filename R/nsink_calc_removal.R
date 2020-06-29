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
#' @param off_network_lakes Optional argument to set removal for waterbodies
#'   that are not part of the hydrologic network in NHDPlus. Default value is to
#'   use the 75th percentile of removal from other lakes in the HUC.  If another
#'   value is desired provide a single numeric ranging from 0 to 1.
#' @param off_network_streams Optional argument to set removal for streams that
#'   are not part of the hydrologic network in NHDPlus. Default value is to use
#'   the median removal from first order streams in the HUC.  If another value
#'   is desired provide a single numeric ranging from 0 to 1.
#' @param off_network_canalsditches Optional argument to set removal for canals
#'   and ditches that are not part of the hydrologic network in NHDPlus. Default
#'   value is to use the 25th percentile of removal from third order streams in
#'   the HUC. If another value is desired provide a single numeric ranging from
#'   0 to 1.
#' @return A list with three items, 1) a raster stack with one layer with
#'         nitrogen removal, a second layer with the type of removal (e.g.
#'         hydric soils, lakes, streams), 2) a polygon representing removal from
#'         land, and 3) removal from the stream network, including stream
#'         removal, and lake removal.
#'
#' @references Kellogg, D. Q., Gold, A. J., Cox, S., Addy, K., & August, P. V.
#'             (2010). A geospatial approach for assessing denitrification sinks
#'             within lower-order catchments. Ecological Engineering, 36(11),
#'             1596-1606.
#'             \href{https://doi.org/10.1016/j.ecoleng.2010.02.006}{Link}
#'
#' @export
#' @importFrom stars st_rasterize st_as_stars
#' @importFrom stars st_rasterize st_as_stars
#' @importFrom methods as
#' @importFrom stats median quantile
#' @examples
#' \dontrun{
#' library(nsink)
#' niantic_huc <- nsink_get_huc_id("Niantic River")$huc_12
#' niantic_data <- nsink_get_data(niantic_huc, data_dir = "nsink_data")
#' aea <- "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0
#' +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
#' niantic_nsink_data <- nsink_prep_data(niantic_huc, projection = aea ,
#'                                       data_dir = "nsink_data")
#' removal <- nsink_calc_removal(niantic_nsink_data)
#' }
nsink_calc_removal <- function(input_data,off_network_lakes = NULL,
                               off_network_streams = NULL,
                               off_network_canalsditches = NULL){
  if (all(names(input_data) %in% c("streams", "lakes", "fdr", "impervious",
                                   "nlcd", "ssurgo", "q", "tot", "huc",
                                   "raster_template", "lakemorpho"))) {

    message("Calculating land-based removal...")
    land_removal <- nsink_calc_land_removal(input_data[c("ssurgo", "impervious",
                                                         "raster_template")])
    message("Calculating stream-based removal...")
    stream_removal <- nsink_calc_stream_removal(input_data[c("streams","q", "tot",
                                                              "raster_template")])
    message("Calculating lake-based removal...")
    lake_removal <- nsink_calc_lake_removal(input_data[c("streams", "lakes",
                                                         "tot","lakemorpho",
                                                         "raster_template")])
    message("Calculating off network removal...")
    off_network_removal <- nsink_calc_off_network_removal(
      list(streams = input_data$streams, lakes = input_data$lakes,
           network_removal = rbind(stream_removal$stream_removal_v,
                                   lake_removal$lake_removal_v),
           tot = input_data$tot,
           raster_template = input_data$raster_template), off_network_lakes,
      off_network_streams, off_network_canalsditches)

    message("Combining all removal...")
    removal <- list(
      land_removal_r = land_removal$land_removal_r,
      land_removal_v = land_removal$land_removal_v,
      off_network_removal_r = off_network_removal$off_network_removal_r,
      off_network_removal_v = off_network_removal$off_network_removal_v,
      off_network_lakes_v = off_network_removal$off_network_lakes_v,
      off_network_streams_v = off_network_removal$off_network_streams_v,
      off_network_canal_ditch_v = off_network_removal$off_network_canal_ditch_v,
      stream_removal_r = stream_removal$stream_removal_r,
      stream_removal_v = stream_removal$stream_removal_v,
      lake_removal_r = lake_removal$lake_removal_r,
      lake_removal_v = lake_removal$lake_removal_v,
      off_network_removal = off_network_removal,
      raster_template = input_data$raster_template, huc = input_data$huc
    )

    merged_removal <- nsink_merge_removal(list(
      land_removal = removal$land_removal_r,
      off_network_removal = removal$off_network_removal_r,
      stream_removal = removal$stream_removal_r,
      lake_removal = removal$lake_removal_r,
      raster_template = removal$raster_template,
      huc = input_data$huc
    ))
    merged_type <- nsink_calc_removal_type(list(
      land_removal = removal$land_removal_r,
      off_network_removal = removal$off_network_removal_r,
      stream_removal = removal$stream_removal_r,
      lake_removal = removal$lake_removal_r,
      raster_template = removal$raster_template,
      huc = input_data$huc
    ))

    # Suppressing warning from raster on proj
    land_off_network_removal_r <- suppressWarnings(raster::merge(removal$off_network_removal_r,
                  removal$land_removal_r))

    # Very ugly way to handle off network types that may or may not exist.

    lakesl <- any(class(removal$off_network_lakes_v) == "sf")
    streamsl <- any(class(removal$off_network_streams_v) == "sf")
    canal_ditchl <- any(class(removal$off_network_canal_ditch_v) == "sf")
    # Suppressing warning from raster on proj
    suppressWarnings({
    if(lakesl & streamsl & canal_ditchl){
      land_off_network_removal_type_r <- raster::merge(
        fasterize::fasterize(removal$off_network_lakes_v,
                             input_data$raster_template, field = "segment_type",
                             background = NA, fun = "max"),
        as(st_rasterize(removal$off_network_streams_v["segment_type"],
                        st_as_stars(input_data$raster_template)), "Raster"),
        as(st_rasterize(removal$off_network_canal_ditch_v["segment_type"],
                        st_as_stars(input_data$raster_template)), "Raster"),
        fasterize::fasterize(removal$land_removal_v,
                             input_data$raster_template, field = "segment_type",
                             background = NA, fun = "max"))
      } else if(lakesl & streamsl){
        land_off_network_removal_type_r <- raster::merge(
          fasterize::fasterize(removal$off_network_lakes_v,
                               input_data$raster_template, field = "segment_type",
                               background = NA, fun = "max"),
          as(st_rasterize(removal$off_network_streams_v["segment_type"],
                          st_as_stars(input_data$raster_template)), "Raster"),
          fasterize::fasterize(removal$land_removal_v,
                               input_data$raster_template, field = "segment_type",
                               background = NA, fun = "max"))
      } else if(lakesl & canal_ditchl){
        land_off_network_removal_type_r <- raster::merge(
          fasterize::fasterize(removal$off_network_lakes_v,
                               input_data$raster_template, field = "segment_type",
                               background = NA, fun = "max"),
          as(st_rasterize(removal$off_network_canal_ditch_v["segment_type"],
                          st_as_stars(input_data$raster_template)), "Raster"),
          fasterize::fasterize(removal$land_removal_v,
                               input_data$raster_template, field = "segment_type",
                               background = NA, fun = "max"))
      } else if(streamsl & canal_ditchl){
        land_off_network_removal_type_r <- raster::merge(
          as(st_rasterize(removal$off_network_streams_v["segment_type"],
                          st_as_stars(input_data$raster_template)), "Raster"),
          as(st_rasterize(removal$off_network_canal_ditch_v["segment_type"],
                          st_as_stars(input_data$raster_template)), "Raster"),
          fasterize::fasterize(removal$land_removal_v,
                               input_data$raster_template, field = "segment_type",
                               background = NA, fun = "max"))
      } else if(lakesl){
        land_off_network_removal_type_r <- raster::merge(
          fasterize::fasterize(removal$off_network_lakes_v,
                               input_data$raster_template, field = "segment_type",
                               background = NA, fun = "max"),
          fasterize::fasterize(removal$land_removal_v,
                               input_data$raster_template, field = "segment_type",
                               background = NA, fun = "max"))
      } else if(streamsl){
        land_off_network_removal_type_r <- raster::merge(
          as(st_rasterize(removal$off_network_streams_v["segment_type"],
                          st_as_stars(input_data$raster_template)), "Raster"),
          fasterize::fasterize(removal$land_removal_v,
                               input_data$raster_template, field = "segment_type",
                               background = NA, fun = "max"))
      } else if(canal_ditchl){
        land_off_network_removal_type_r <- raster::merge(
          as(st_rasterize(removal$off_network_canal_ditch_v["segment_type"],
                          st_as_stars(input_data$raster_template)), "Raster"),
          fasterize::fasterize(removal$land_removal_v,
                               input_data$raster_template, field = "segment_type",
                               background = NA, fun = "max"))
      } else{
        land_off_network_removal_type_r <- raster::merge(
          fasterize::fasterize(removal$land_removal_v,
                               input_data$raster_template, field = "segment_type",
                               background = NA, fun = "max"))
      }})



    land_off_network_removal_v <- st_as_sf(
      st_as_stars(land_off_network_removal_r), as_points = FALSE, merge = TRUE)
    land_off_network_removal_v <- st_make_valid(land_off_network_removal_v)
    #land_off_network_removal_v <- st_as_sf(raster::rasterToPolygons(land_off_network_removal_r,
    #                                                                dissolve = TRUE))
    land_off_network_removal_type_v <- st_as_sf(
      st_as_stars(land_off_network_removal_type_r),
      as_points = FALSE, merge = TRUE)
    land_off_network_removal_type_v <- st_make_valid(land_off_network_removal_type_v)
    #land_off_network_removal_type_v <- st_as_sf(raster::rasterToPolygons(land_off_network_removal_type_r,
    #                                                                     dissolve = TRUE))
    # Supressing warnings from raster on proj

    suppressWarnings({
    return(list(
      raster_method = raster::stack(merged_removal, merged_type),
      #land_removal = removal$land_removal_v, # May not need these separately
      #off_network_removal = removal$off_network_removal_v,  # May not need these separately
      land_off_network_removal = land_off_network_removal_v,
      land_off_network_removal_type = land_off_network_removal_type_v,
      network_removal = rbind(
        removal$stream_removal_v,
        removal$lake_removal_v
      )
    ))})
  } else {
    stop("The input data do not contain the expected data.  Check the object and
         re-run with nsink_prep_data().")
  }
}

#' Calculates off network waterbody and stream nitrogen removal
#'
#' @param input_data  A named list with "streams", "lakes",
#'                   "network_removal", "tot", and "raster_template".
#' @param off_network_lakes Optional argument to set removal for waterbodies
#'   that are not part of the hydrologic network in NHDPlus. Default value is to
#'   use the 75th percentile of removal from other lakes in the HUC.  If another
#'   value is desired provide a single numeric ranging from 0 to 1.
#' @param off_network_streams Optional argument to set removal for streams that
#'   are not part of the hydrologic network in NHDPlus. Default value is to use
#'   the median removal from first order streams in the HUC.  If another value
#'   is desired provide a single numeric ranging from 0 to 1.
#' @param off_network_canalsditches Optional argument to set removal for canals
#'   and ditches that are not part of the hydrologic network in NHDPlus. Default
#'   value is to use the 25th percentile of removal from third order streams in
#'   the HUC. If another value is desired provide a single numeric ranging from
#'   0 to 1.
#' @return raster and vectors of off network nitrogen removal
#' @import dplyr sf
#' @importFrom rlang .data
#' @importFrom raster rasterize merge
#' @importFrom fasterize fasterize
#' @keywords internal
nsink_calc_off_network_removal <- function(input_data, off_network_lakes,
                                           off_network_streams,
                                           off_network_canalsditches) {


  if(any(input_data$streams$flowdir == "Uninitialized") |
    any(!input_data$lakes$lake_comid %in% input_data$network_removal$lake_comid)){

    # Calculate removal stats for lakes and streams that have removal
    removal_stats_lakes <- filter(input_data$network_removal, .data$n_removal > 0,
                                  .data$ftype == "LakePond")
    removal_stats_lakes <- group_by(removal_stats_lakes, .data$ftype)
    removal_stats_lakes <- summarize(removal_stats_lakes,
                             avg_n_remove = mean(.data$n_removal, na.rm = TRUE),
                             med_n_remove = median(.data$n_removal, na.rm = TRUE),
                             min_n_remove = min(.data$n_removal, na.rm = TRUE),
                             max_n_remove = max(.data$n_removal, na.rm = TRUE),
                             third_quart_n_remove = quantile(.data$n_removal,
                                                             probs = 0.75,
                                                             na.rm = TRUE),
                             num_lakes = n())

    removal_stats_streams <- filter(input_data$network_removal, .data$n_removal > 0)
    removal_stats_streams <- filter(removal_stats_streams, .data$ftype == "StreamRiver" |
                                      ftype == "CanalDitch")
    removal_stats_streams <- left_join(removal_stats_streams,
                                       input_data$tot, by = "stream_comid")
    removal_stats_streams <- group_by(removal_stats_streams, .data$ftype,
                                      .data$stream_order)
    removal_stats_streams <- summarize(removal_stats_streams,
                                       avg_n_remove = mean(.data$n_removal,
                                                           na.rm = TRUE),
                                       med_n_remove = median(.data$n_removal,
                                                             na.rm = TRUE),
                                       min_n_remove = min(.data$n_removal,
                                                          na.rm = TRUE),
                                       max_n_remove = max(.data$n_removal,
                                                          na.rm = TRUE),
                                       third_quart_n_remove =
                                         quantile(.data$n_removal, probs = 0.75,
                                                  na.rm = TRUE),
                                       low_quart_n_remove =
                                         quantile(.data$n_removal, probs = 0.25,
                                                  na.rm = TRUE),
                                       num_streams = n())

    # Off network streams
    removal_stats_1st <- filter(removal_stats_streams, .data$stream_order == 1)
    if(removal_stats_1st$num_streams == 0 & is.null(off_network_streams)){
      stop("There are no on network streams available to estimate removal for off network streams.  Please specify a removal value with the off_network_streams argument.")
    } else if(removal_stats_1st$num_streams > 0 &
              removal_stats_1st$num_streams <= 3 &
              is.null(off_network_streams)){
      warning("There are three or fewer on network streams available to estimate removal for the off network streams.  It may be advisable to manually set an N removal value via the off_network_streams argument.")
    }
    if(any(input_data$streams$flowdir == "Uninitialized") &
       any(input_data$streams$ftype == "StreamRiver")){
      off_network_streams_sf <- filter(input_data$streams,
                                    input_data$streams$flowdir ==
                                      "Uninitialized")
      off_network_streams_sf <- filter(off_network_streams_sf, .data$ftype == "StreamRiver")
      if(is.null(off_network_streams)){
        med_removal_1st_order <- pull(removal_stats_1st, .data$med_n_remove)
      } else if(is.numeric(off_network_streams)){
        med_removal_1st_order <- off_network_streams
      } else {
        med_removal_1st_order <- NA
      }
      off_network_streams_sf <- transmute(off_network_streams_sf, n_removal =
                                      med_removal_1st_order,
                                      segment_type = 5)
      # Suppressing warnings from raster on proj
      off_network_streams_r <- suppressWarnings(rasterize(off_network_streams_sf,
                                         input_data$raster_template,
                                         field = "n_removal",
                                         background = NA, fun = "max"))
    } else {
      off_network_streams_r <- input_data$raster_template
    }

    # Off network canals/ditches
    removal_stats_high_order <- filter(removal_stats_streams,
                                       .data$stream_order == max(.data$stream_order))
    if(removal_stats_high_order$num_streams == 0 & is.null(off_network_canalsditches)){
      stop("There are no on network streams available to estimate removal for off network canals and ditches  Please specify a removal value with the off_network_canalsditches argument.")
    } else if(removal_stats_high_order$num_streams > 0 &
              removal_stats_high_order$num_streams <= 3 &
              is.null(off_network_canalsditches)){
      warning("There are three or fewer on network streams available to estimate removal for the off network canals and ditches.  It may be advisable to manually set an N removal values via the off_network_canalsditches argument.")
    }

    if(any(input_data$streams$flowdir == "Uninitialized")&
       any(input_data$streams$ftype == "CanalDitch")){
      off_network_canal_ditch_sf <- filter(input_data$streams,
                                    input_data$streams$flowdir ==
                                      "Uninitialized")
      off_network_canal_ditch_sf <- filter(off_network_canal_ditch_sf, .data$ftype ==
                                          "CanalDitch")
      #Something good goes here. For time being use lower quartile of higher
      #order streams

      if(is.null(off_network_canalsditches)){
        low_quart_removal_high_order <- pull(removal_stats_high_order,
                                             .data$low_quart_n_remove)
      } else if(is.numeric(off_network_canalsditches)){
        low_quart_removal_high_order <- off_network_canalsditches
      } else {
        low_quart_removal_high_order <- NA
      }
      off_network_canal_ditch_sf <- transmute(off_network_canal_ditch_sf,
                                        n_removal = low_quart_removal_high_order,
                                        segment_type = 6)
      # Suppressing warnings from rater on proj
      off_network_canal_ditch_r <- suppressWarnings(rasterize(off_network_canal_ditch_sf,
                                           input_data$raster_template,
                                           field = "n_removal",
                                           background = NA, fun = "max"))

    } else {
      off_network_canal_ditch_r <- input_data$raster_template
    }

    # Off network lakes
    if(removal_stats_lakes$num_lakes == 0 & is.null(off_network_lakes)){
      stop("There are no on network lakes available to estimate removal for off network lakes.  Please specify a removal value with the off_network_lakes argument.")
    } else if(removal_stats_lakes$num_lakes > 0 &
              removal_stats_lakes$num_lakes <= 3 &
              is.null(off_network_lakes)){
      warning("There are three or fewer on network lakes available to estimate removal for the off network lakes.  It may be advisable to manually set an N removal values via the off_network_lakes argument.")
    }

    if(any(!input_data$lakes$lake_comid %in% input_data$network_removal$lake_comid)){
      off_network_lakes_sf <- filter(input_data$lakes,!input_data$lakes$lake_comid
                                %in% input_data$network_removal$lake_comid)
      on_network_lakes_df <- filter(input_data$network_removal,
                                    .data$ftype == "LakePond")
      on_network_lakes_df <- st_set_geometry(on_network_lakes_df, NULL)
      on_network_lakes <- full_join(input_data$lakes, on_network_lakes_df,
                                    by = "lake_comid")
      on_network_lakes <- filter(on_network_lakes, .data$n_removal > 0,
                                 !is.na(.data$n_removal))
      third_quart_lake_removal <- filter(removal_stats_lakes, .data$ftype == "LakePond")
      if(is.null(off_network_lakes)){
        third_quart_lake_removal <- pull(third_quart_lake_removal,
                                         .data$third_quart_n_remove)
      } else if(is.numeric(off_network_lakes)){
        third_quart_lake_removal <- off_network_lakes
      } else {
        third_quart_lake_removal <- NA
      }
      off_network_lakes_sf <- transmute(off_network_lakes_sf,
                                     n_removal = third_quart_lake_removal,
                                     segment_type = 4)
      # Suppressing warnings on proj from fasterize
      off_network_lakes_r <- suppressWarnings(fasterize::fasterize(off_network_lakes_sf,
                                               input_data$raster_template,
                                               field = "n_removal",
                                               background = NA, fun = "max"))

    } else {
      off_network_lakes_r <- input_data$raster_template
    }
  } else {
    return(NA)
  }

  # Create raster
  # Suppress warnings from raster on proj
  off_network_removal_r <- suppressWarnings(raster::merge(off_network_lakes_r,
                                         off_network_streams_r,
                                         off_network_canal_ditch_r))

  if(!exists("off_network_lakes_sf")){off_network_lakes_sf <- NA}
  if(!exists("off_network_streams_sf")){off_network_streams_sf <- NA}
  if(!exists("off_network_canal_ditch_sf")){off_network_canal_ditch_sf <- NA}
  # Suppress warnings from raster on proj
  off_network_removal_v <- st_as_sf(
    st_as_stars(off_network_removal_r), as_points = FALSE, merge = TRUE)
  off_network_removal_v <- st_make_valid(off_network_removal_v)
  #off_network_removal_v = suppressWarnings(st_as_sf(raster::rasterToPolygons(
  #off_network_removal_r, dissolve = TRUE)))
  list(off_network_removal_r = off_network_removal_r,
       off_network_removal_v = off_network_removal_v,
       off_network_lakes_v = off_network_lakes_sf,
       off_network_streams_v = off_network_streams_sf,
       off_network_canal_ditch_v = off_network_canal_ditch_sf)
}

#' Calculates land-based nitrogen removal
#'
#' @param input_data A named list with "ssurgo", "impervious", "lakes", and
#'                   "raster_template".
#' @return list with raster and vector versions of land based nitrogen removal
#' @import dplyr sf
#' @importFrom rlang .data
#' @importFrom stars st_as_stars
#' @keywords internal
nsink_calc_land_removal <- function(input_data) {
  #browser()
  land_removal <- mutate(input_data$ssurgo,
    n_removal = 0.8 * (.data$hydric_pct / 100)
  )
  land_removal <- mutate(land_removal, n_removal = case_when(
    .data$n_removal == 0 ~
    NA_real_,
    TRUE ~ .data$n_removal
  ))
  land_removal <- group_by(land_removal, .data$hydric_pct)
  land_removal <- summarize(land_removal, n_removal = unique(.data$n_removal))
  land_removal <- ungroup(land_removal)
  # Suppressing warnings because fasterisze uses raster and proj
  suppressWarnings({
  land_removal_rast <- fasterize::fasterize(land_removal,
    input_data$raster_template,
    field = "n_removal", background = 0,
    fun = "max")
  impervious <- input_data$impervious
  impervious[impervious >= 0] <- 0
  impervious[is.na(impervious)] <- 1
  imp_land_removal <- land_removal_rast * impervious

  land_removal_v <- st_as_sf(st_as_stars(imp_land_removal), as_points = FALSE,
                             merge = TRUE)
  #land_removal_v <- st_as_sf(raster::rasterToPolygons(imp_land_removal,
  #                                                  dissolve = TRUE))
  })
  land_removal_v <- mutate(land_removal_v, segment_type =
                             case_when(layer > 0 ~ 1,
                                       TRUE ~ 0))

  list(land_removal_r = imp_land_removal,
       land_removal_v = land_removal_v)
}

#' Calculates stream-based nitrogen removal
#'
#' @param input_data  A named list with "streams", "q", "tot", and
#'                   "raster_template".
#' @return raster and vector versions of stream based nitrogen removal
#' @import dplyr sf
#' @importFrom rlang .data
#' @importFrom stars st_rasterize st_as_stars
#' @importFrom stars st_rasterize st_as_stars
#' @importFrom methods as
#' @keywords internal
nsink_calc_stream_removal <- function(input_data) {

  stream_removal <- mutate_if(input_data$streams, is.factor, as.character())
  #stream_removal <- filter(stream_removal, flowdir != "Uninitialized")
  stream_removal <- suppressMessages(left_join(stream_removal,
    input_data$q,
    by = c("stream_comid" = "stream_comid")
  ))
  stream_removal <- suppressMessages(left_join(stream_removal,
    input_data$tot,
    by = c("stream_comid" = "stream_comid")
  ))
  stream_removal <- filter(stream_removal, .data$ftype != "ArtificialPath")
  stream_removal <- mutate(stream_removal, totma = case_when(
    .data$totma == -9999 ~ NA_real_,
    TRUE ~ .data$totma
  ))
  stream_removal <- mutate(stream_removal,
    n_removal =
      (1 - exp(-0.0513 * (.data$mean_reach_depth^-1.319)
        * .data$totma)) / 100
  )
  # When time of travel not available in NHDPlus, use median removal of other
  # streams of the same order.
  stream_removal_stats <- group_by(st_set_geometry(stream_removal, NULL),
                                   .data$stream_order)
  stream_removal_stats <- summarize(stream_removal_stats,
                                    median_removal = median(.data$n_removal,
                                                            na.rm = TRUE))
  stream_removal_stats <- ungroup(stream_removal_stats)
  stream_removal_stats <- filter(stream_removal_stats, !is.na(.data$median_removal))

  # add existing order (not sure if this is necessary, just being cautious)
  stream_removal <- mutate(stream_removal, order = seq_along(.data$n_removal))
  stream_removal_missing <- filter(stream_removal, is.na(.data$n_removal) &
                                     .data$stream_order > 0 &
                                     !is.na(.data$stream_order))
  stream_removal_not_missing <- filter(stream_removal,
                                       !(is.na(.data$n_removal) &
                                           .data$stream_order > 0 &
                                           !is.na(.data$stream_order)))
  stream_removal_missing <- left_join(stream_removal_missing,
                                      stream_removal_stats,
                                      by = "stream_order")
  stream_removal_missing <- mutate(stream_removal_missing, n_removal = .data$median_removal)
  stream_removal_missing <- select(stream_removal_missing, -.data$median_removal)

  stream_removal <- rbind(stream_removal_not_missing, stream_removal_missing)
  stream_removal <- arrange(stream_removal, .data$order)
  stream_removal <- select(stream_removal, -.data$order)
  stream_removal <- filter(stream_removal, !is.na(.data$n_removal))
  # Suppressing warnings from raster on proj
  stream_removal_r <- stars::st_rasterize(stream_removal["n_removal"],
                                          st_as_stars(input_data$raster_template))
  stream_removal_r <- suppressWarnings(as(stream_removal_r, "Raster"))
  list(stream_removal_r = stream_removal_r,
       stream_removal_v = select(stream_removal, .data$stream_comid, .data$lake_comid,
                                 .data$gnis_name, .data$ftype, .data$n_removal))
}

#' Calculates lake-based nitrogen removal
#'
#' @param input_data A named list with "streams", "lakes", "tot", "lakemorpho",
#'                   and "raster_template".
#' @return raster and vector versions of lake based nitrogen removal
#' @import dplyr sf
#' @importFrom rlang .data
#' @keywords internal
nsink_calc_lake_removal <- function(input_data) {
  residence_time <- suppressMessages(left_join(input_data$streams, input_data$tot))
  residence_time <- filter(residence_time, .data$lake_comid > 0)
  # TODO When time of travel not available in NHDPlus, need to use other methods
  #      to calculate residence time (in Kellogg et al)
  #      Don't think this is possible as drainage area unavailble...
  residence_time <- mutate(residence_time, totma = case_when(
    .data$totma == -9999 ~
    NA_real_,
    TRUE ~ .data$totma
  ))
  residence_time <- group_by(residence_time, .data$lake_comid)
  residence_time <- summarize(residence_time,
    lake_residence_time_yrs =
      sum(.data$totma * 0.002737851)
  )
  residence_time <- ungroup(residence_time)
  residence_time_sf <- residence_time
  st_geometry(residence_time) <- NULL

  lake_removal <- suppressMessages(left_join(input_data$lakes, input_data$lakemorpho))
  lake_removal <- suppressMessages(left_join(lake_removal, residence_time))
  lake_removal <- mutate(lake_removal, meandused = case_when(
    .data$meandused < 0 ~ NA_real_,
    TRUE ~ .data$meandused
  ))
  lake_removal <- mutate(lake_removal,
    n_removal =
      (79.24 - (33.26 * log10(.data$meandused / .data$lake_residence_time_yrs))) / 100
  )
  lake_removal <- mutate(lake_removal, n_removal = case_when(
    .data$n_removal < 0 ~ 0,
    TRUE ~ .data$n_removal
  ))

  # When time of travel not available in NHDPlus, use median removal of other
  # lakes of the same order.
  lake_removal_stats <- st_set_geometry(lake_removal, NULL)
  lake_removal_stats <- summarize(lake_removal_stats,
                                    median_removal = median(.data$n_removal,
                                                            na.rm = TRUE))
  lake_removal_stats <- filter(lake_removal_stats, !is.na(.data$median_removal))
  lake_removal_median <- pull(lake_removal_stats, .data$median_removal)

  # add existing order (not sure if this is necessary, just being cautious)
  lake_removal <- mutate(lake_removal, order = seq_along(.data$n_removal))
  lake_removal_missing <- filter(lake_removal, is.na(.data$n_removal))
  lake_removal_not_missing <- filter(lake_removal, !is.na(.data$n_removal))
  lake_removal_missing <- mutate(lake_removal_missing,
                                 n_removal = lake_removal_median)

  lake_removal <- rbind(lake_removal_not_missing, lake_removal_missing)
  lake_removal <- arrange(lake_removal, .data$order)
  lake_removal <- select(lake_removal, -.data$order)
  lake_removal <- filter(lake_removal, !is.na(.data$n_removal))

  lake_removal_sf <- lake_removal


  st_geometry(lake_removal) <- NULL
  lake_removal_flowpath <- suppressMessages(left_join(residence_time_sf, lake_removal))
  # Supressing proj warnings from fasterize
  lake_removal_r <- suppressWarnings(fasterize::fasterize(lake_removal_sf,
                                         input_data$raster_template,
                                         field = "n_removal", fun = "max"))
  comids <- select(input_data$streams, .data$stream_comid, .data$lake_comid)
  st_geometry(comids) <- NULL
  lake_removal_flowpath <- suppressMessages(left_join(lake_removal_flowpath,
                                                      comids))
  lake_removal_flowpath <- select(lake_removal_flowpath, .data$stream_comid,
                                  .data$lake_comid, .data$gnis_name, .data$ftype, .data$n_removal)
  list(lake_removal_r = lake_removal_r, lake_removal_v = lake_removal_flowpath)
}

#' Merges removal rasters into single raster
#'
#' @param removal_rasters A named list of "land_removal", "stream_removal,
#'                        "lake_removal", and "raster_template" rasters plus a
#'                        sf object "huc".
#' @importFrom methods as
#' @return raster of landscape nitrogen removal
#' @keywords internal
nsink_merge_removal <- function(removal_rasters) {

  # Supressing warnings from raster on proj
  suppressWarnings({
  lake_removal <- raster::reclassify(removal_rasters$lake_removal,
                             cbind(-Inf, 0, NA), right=FALSE)
  land_removal <- raster::reclassify(removal_rasters$land_removal,
                             cbind(-Inf, 0, NA), right=FALSE)
  off_network_removal <- raster::reclassify(removal_rasters$off_network_removal,
                                    cbind(-Inf, 0, NA), right=FALSE)


  removal <- raster::merge(removal_rasters$lake_removal,
                           removal_rasters$off_network_removal,
                           removal_rasters$land_removal)
  removal <- raster::mask(removal, as(removal_rasters$huc, "Spatial"))
  removal[is.na(removal)] <- 0
  removal <- raster::focal(removal, matrix(1, nrow = 3, ncol = 3), max)
  removal <- raster::projectRaster(removal, removal_rasters$raster_template,
                                   method = "ngb")
  removal <- raster::merge(removal_rasters$stream_removal, removal)
  })
  removal
}

#' Create removal type raster
#'
#' @param removal_rasters A named list of "land_removal", "stream_removal,
#'                        "lake_removal", and "raster_template" rasters plus a
#'                        sf object "huc".
#' @return raster of landscape nitrogen removal
#' @keywords internal
nsink_calc_removal_type <- function(removal_rasters) {
  type_it <- function(removal_rast, type = c("hydric", "stream", "lake",
                                             "off_network")) {
    type <- match.arg(type)
    if (type == "hydric") {
      val <- raster::getValues(removal_rast)
      val[val > 0] <- 1
      val[val == 0] <- NA
    } else if (type == "stream") {
      val <- raster::getValues(removal_rast)
      val[!is.na(val)] <- 2
    } else if (type == "lake") {
      val <- raster::getValues(removal_rast)
      val[!is.na(val)] <- 3
    } else if (type == "off_network") {
      val <- raster::getValues(removal_rast)
      val[!is.na(val)] <- 4
    }
    # Suppress warnings from raster on proj
    suppressWarnings({
    raster::setValues(removal_rast, val)
    })
  }

  hydric_type <- type_it(removal_rasters$land_removal, "hydric")
  stream_type <- type_it(removal_rasters$stream_removal, "stream")
  lake_type <- type_it(removal_rasters$lake_removal, "lake")
  off_network_type <- type_it(removal_rasters$off_network_removal, "off_network")
  suppressWarnings({
  types <- raster::merge(lake_type, off_network_type, hydric_type)
  types <- raster::mask(types, removal_rasters$huc)
  types[is.na(types)] <- 0
  types <- raster::focal(types, matrix(1, nrow = 3, ncol = 3), max)
  types <- raster::projectRaster(types, removal_rasters$raster_template,
                                 method = "ngb")
  types <- raster::merge(stream_type, types)
  })
  types
}
