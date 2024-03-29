#' Summarize nitrogen removal along a flowpath
#'
#' Nitrogen removal varies along a flowpath as it may include different land
#' cover and waterbody types that have different nitrogen reduction capabilities.
#' This function requires a flowpath generated by
#' \code{\link{nsink_generate_flowpath}} as input and returns an
#' estimate of total flow path removal as well as removal by type.
#'
#'
#' @param flowpath A flowpath to summarize nitrogen removal
#' @param removal The removal raster stack or removal list, generated by
#'                \code{\link{nsink_calc_removal}}
#' @return A data frame is returned with a summary of nitrogen removal.  The
#'
#'
#' @importFrom zoo rollmax
#' @importFrom raster rasterize extract
#' @importFrom rlang .data
#' @importFrom stats median quantile
#' @importFrom dplyr near
#' @export
#' @examples
#' \dontrun{
#' library(nsink)
#' niantic_huc <- nsink_get_huc_id("Niantic River")$huc_12
#' niantic_data <- nsink_get_data(niantic_huc, data_dir = "nsink_data")
#' aea <- 5072
#' niantic_nsink_data <- nsink_prep_data(niantic_huc, projection = aea,
#'                                       data_dir = "nsink_data")
#' removal <- nsink_calc_removal(niantic_nsink_data)
#' pt <- c(1948121, 2295822)
#' start_loc <- st_sf(st_sfc(st_point(c(pt)), crs = aea))
#' fp <- nsink_generate_flowpath(start_loc, niantic_nsink_data)
#' flow_summary <- nsink_summarize_flowpath(fp, removal)
#' flow_summary
#' }
nsink_summarize_flowpath <- function(flowpath, removal) {
  # Off Network based removal in flowpath ends
  type_poly <- st_cast(removal$land_off_network_removal_type, "POLYGON")
  type_poly <- mutate(type_poly, type_id = paste0("type_", seq_along(.data$layer)))
  st_agr(type_poly) <- "constant"
  removal_poly <- st_cast(removal$land_off_network_removal, "POLYGON")
  removal_poly <- mutate(removal_poly,
                         remove_id = paste0("remove_", seq_along(.data$layer)))
  st_agr(removal_poly) <- "constant"

  # Change from 2021-04-12
  # old method
  land_off_network_type <- st_intersection(flowpath$flowpath_ends[1,],
                                           type_poly)
  st_agr(land_off_network_type) <- "constant"
  land_off_network_remove <- st_intersection(removal_poly,
                                             land_off_network_type)
  if(!all(st_is(land_off_network_remove, "LINESTRING"))){
    land_off_network_removal <- suppressWarnings(st_collection_extract(land_off_network_remove,
                                                      "LINESTRING"))
  } else {
    land_off_network_removal <- land_off_network_remove
  }

  st_agr(land_off_network_removal) <- "constant"
  land_off_network_removal <- suppressWarnings(
    st_cast(land_off_network_removal, "LINESTRING"))

  type_length <- as.numeric(sum(st_length(land_off_network_type), na.rm = TRUE))
  remove_length <- as.numeric(sum(st_length(land_off_network_remove),
                                  na.rm = TRUE))
  removal_length <- as.numeric(sum(st_length(land_off_network_removal),
                                   na.rm = TRUE))
  # Bug fix method #1 if the above are not the same length
  if(!dplyr::near(type_length, removal_length) |
     !dplyr::near(remove_length, removal_length)) {
    land_off_network_type <- st_intersection(flowpath$flowpath_ends[1,],
                                             type_poly)
    st_agr(land_off_network_type) <- "constant"
    land_off_network_remove <- st_intersection(flowpath$flowpath_ends[1,],
                                               removal_poly)
    st_agr(land_off_network_remove) <- "constant"
    land_off_network_removal <- st_intersection(land_off_network_remove,
                                                land_off_network_type)
    st_agr(land_off_network_removal) <- "constant"

    if(!all(st_is(land_off_network_removal, "LINESTRING"))){
      land_off_network_removal <- suppressWarnings(
        st_collection_extract(land_off_network_removal,"LINESTRING"))
    }
    st_agr(land_off_network_removal) <- "constant"
    land_off_network_removal <- st_cast(land_off_network_removal, "LINESTRING")
  }
  type_length <- as.numeric(sum(st_length(land_off_network_type), na.rm = TRUE))
  remove_length <- as.numeric(sum(st_length(land_off_network_remove),
                                  na.rm = TRUE))
  removal_length <- as.numeric(sum(st_length(land_off_network_removal),
                                   na.rm = TRUE))
  # Bug fix method #2 if the above are still not the same length
  if(!dplyr::near(type_length, removal_length) |
     !dplyr::near(remove_length, removal_length)) {

    # Deals with the very rare case when we drop sections...
    snap_dist <- units::set_units(1, "m")
    snap_dist <- units::set_units(snap_dist, st_crs(land_off_network_removal,
                                                    parameters = TRUE)$ud_unit,
                                  mode = "standard")
    land_off_network_type <- st_intersection(flowpath$flowpath_ends[1,],
                                             type_poly)
    st_agr(land_off_network_type) <- "constant"
    land_off_network_remove <- st_intersection(flowpath$flowpath_ends[1,],
                                               removal_poly)
    land_off_network_type <- st_snap(land_off_network_type,
                                        land_off_network_remove,
                                        tolerance = snap_dist)
    land_off_network_remove <- st_snap(land_off_network_remove,
                                        land_off_network_type,
                                        tolerance = snap_dist)
    st_agr(land_off_network_remove) <- "constant"
    land_off_network_removal <- st_intersection(land_off_network_remove,
                                                land_off_network_type)
    st_agr(land_off_network_removal) <- "constant"

    if(!all(st_is(land_off_network_removal, "LINESTRING"))){
      land_off_network_removal <- suppressWarnings(
        st_collection_extract(land_off_network_removal,"LINESTRING"))
    }
    st_agr(land_off_network_removal) <- "constant"
    land_off_network_removal <- st_cast(land_off_network_removal, "LINESTRING")
  }
  # Bug fix method #3 if the above are still not the same length
  if(!dplyr::near(type_length, removal_length) |
     !dplyr::near(remove_length, removal_length)) {

    # Deals with the very rare case when we drop sections...
    snap_dist <- units::set_units(60, "m")
    snap_dist <- units::set_units(snap_dist, st_crs(land_off_network_removal,
                                                    parameters = TRUE)$ud_unit,
                                  mode = "standard")
    land_off_network_type <- st_intersection(flowpath$flowpath_ends[1,],
                                             type_poly)
    st_agr(land_off_network_type) <- "constant"
    land_off_network_remove <- st_intersection(flowpath$flowpath_ends[1,],
                                               removal_poly)
    land_off_network_type <- st_snap(land_off_network_type,
                                     land_off_network_remove,
                                     tolerance = snap_dist)
    land_off_network_remove <- st_snap(land_off_network_remove,
                                       land_off_network_type,
                                       tolerance = snap_dist)
    st_agr(land_off_network_remove) <- "constant"
    land_off_network_removal <- st_intersection(land_off_network_remove,
                                                land_off_network_type)
    st_agr(land_off_network_removal) <- "constant"

    if(!all(st_is(land_off_network_removal, "LINESTRING"))){
      land_off_network_removal <- suppressWarnings(
        st_collection_extract(land_off_network_removal,"LINESTRING"))
    }
    st_agr(land_off_network_removal) <- "constant"
    land_off_network_removal <- st_cast(land_off_network_removal, "LINESTRING")
  }
  if(nrow(land_off_network_removal) > 0){
    land_off_network_removal <- mutate(land_off_network_removal,
                                       edge_id = c(1:n()))

    # Section orders the data frame along the flowpath
    land_off_network_removal <- nsink_generate_from_to_nodes(
      land_off_network_removal)
    lonr_g_df <- select(land_off_network_removal, .data$fromnode,
                        .data$tonode, .data$edge_id)
    lonr_g_df <- st_set_geometry(lonr_g_df, NULL)
    lonr_g <- graph_from_data_frame(lonr_g_df)
    start_pt <- lwgeom::st_startpoint(flowpath$flowpath_ends[1,])
    end_pt <- lwgeom::st_endpoint(flowpath$flowpath_ends[1,])
    from_nd_idx <- st_is_within_distance(start_pt, land_off_network_removal,
                                         dist = 0.000001, sparse = FALSE)
    to_nd_idx <- st_is_within_distance(end_pt, land_off_network_removal,
                                       dist = 0.000001, sparse = FALSE)
    from_nd <- as.character(lonr_g_df[from_nd_idx,]$fromnode)
    to_nd <- as.character(lonr_g_df[to_nd_idx,]$tonode)
    idx <- suppressWarnings(shortest_paths(lonr_g, from_nd, to_nd,
                                           output = "epath",
                                           mode = "out")$epath[[1]])
    lonr_ids <- edge_attr(lonr_g, "edge_id", idx)
    land_off_network_removal <- slice(land_off_network_removal,
                                      match(lonr_ids,.data$edge_id))
    land_off_network_removal <- mutate(land_off_network_removal,
                                       ordered = seq_along(.data$edge_id))

    land_off_network_removal_df <- data.frame(
      stream_comid = 0, lake_comid = 0,
      n_removal = land_off_network_removal$layer,
      segment_type = land_off_network_removal$layer.1,
      remove_id = land_off_network_removal$remove_id
    )

    land_off_network_removal_df <- mutate(land_off_network_removal_df,
                                          n_removal = case_when(
                                            is.na(.data$n_removal) ~ 0,
                                            .data$segment_type == 0 ~0,
                                            TRUE ~ .data$n_removal),
                                          segment_id = nsink_create_segment_ids(
                                            paste(.data$segment_type,
                                                  .data$n_removal)),
                                          length = as.numeric(units::set_units(
                                            st_length(land_off_network_removal),
                                            "m")))
  } else {
    land_off_network_removal_df <- data.frame(
      stream_comid = 0, lake_comid = 0,
      n_removal = 0,
      segment_type = 0,
      remove_id = "remove_0",
      segment_id = 1,
      length = as.numeric(units::set_units(
        st_length(flowpath$flowpath_ends[1,]),"m")))
  }

  if (!is.null(flowpath$flowpath_network)) {
    n_removal_df <- select(
      st_drop_geometry(removal$network_removal),
      .data$stream_comid, .data$n_removal
    )
    flowpath_removal <- suppressMessages(left_join(flowpath$flowpath_network,
                                                   n_removal_df))
    flowpath_removal <- mutate(flowpath_removal, length =
                                 as.numeric(units::set_units(st_length(
                                   flowpath_removal), "m")))
    flowpath_removal_df <- st_drop_geometry(flowpath_removal)
    flowpath_removal_df <- unique(flowpath_removal_df)
    flowpath_removal_df <- mutate(flowpath_removal_df,
      segment_type =
        case_when(
          .data$ftype == "ArtificialPath" ~
          "Lake/Pond",
          .data$ftype == "StreamRiver" ~
          "Stream",
          TRUE ~ "Unknown"
        )
    )

    flowpath_removal_df <- select(
      flowpath_removal_df, .data$stream_comid,
      .data$lake_comid, .data$n_removal, .data$segment_type, length
    )
    flowpath_removal_df <- mutate(flowpath_removal_df,
      segment_id =
        nsink_create_segment_ids(paste(
          .data$segment_type,
          .data$n_removal
        ))
    )
  } else {
    flowpath_removal_df <- NULL
  }

  removal_summary <- nsink_create_summary(land_off_network_removal_df,
                                          flowpath_removal_df)
  removal_summary <- mutate(removal_summary, length = round(.data$length,0),
                            percent_removal = signif(.data$percent_removal, 3),
                            n_in = signif(.data$n_in, 3),
                            n_out = signif(.data$n_out, 3))
  removal_summary <- rename(removal_summary, length_meters = length)
  return(removal_summary)
}


#' Create ID's for unique values in a vector
#'
#' This functions takes a vector of values and creates a unique ID for adjacent
#' and identical values.
#'
#' @param x A vector of values to create unique ID's
#'
#' @return A vector of unique ID's
#'
#' @keywords internal
nsink_create_segment_ids <- function(x) {
  y <- vector("numeric", length(x))
  y_id <- vector("numeric", length(x))
  for (i in seq_along(x)) {
    if (i == 1) {
      y[i] <- i
    } else {
      y[i] <- ifelse(x[i] == x[i - 1], FALSE, i)
    }
  }
  for (i in seq_along(y)) {
    if (i == 1) {
      y[1] <- y[1]
    } else {
      y[i] <- ifelse(y[i] == 0, y[i - 1], y[i])
    }
  }
  y
}

#' Create a nitrogen removal summary
#'
#' This functions takes a nitrogen removal and removal type data frame for land
#' and the flowpath network and creates a data frame that summarizes the
#' removal along that flowpath.
#'
#' @param land_removal A data frame of land based nitrogen removal via the
#'                     generated flowpath and hydric removal raster.
#' @param network_removal A data frame of stream network nitrogen removal via
#'                        calculated stream and lake removal.
#' @return A data frame summarizing nitrogen removal along a flowpath
#' @import dplyr
#' @importFrom stats median quantile
#' @importFrom rlang .data
#' @keywords internal
nsink_create_summary <- function(land_removal, network_removal) {
    land_removal_df <- mutate(land_removal,
      segment_type = case_when(
        .data$segment_type == 0 ~ "No Removal",
        .data$segment_type == 1 ~ "Hydric",
        .data$segment_type == 2 ~ "Stream",
        .data$segment_type == 3 ~ "Lake/Pond",
        .data$segment_type == 4 ~ "Off Network Lake",
        .data$segment_type == 5 ~ "Off Network Stream",
        .data$segment_type == 6 ~ "Off Network Canal/Ditch"
      )
    )
    land_removal_df <- group_by(land_removal_df, .data$segment_id, .data$segment_type, .data$remove_id)
    land_removal_df <- summarize(land_removal_df,
      length = sum(.data$length),
      n_removal = max(.data$n_removal)
    )
    land_removal_df <- ungroup(land_removal_df)

    # Passing in and out of different segment types can trigger removal at each
    # step. This would result in unusually high removal.  Instead for types likely
    # to have a flow path weave in and out, set as weighted average removal for
    # that type.  For lakes keep each in as its own entity.
    # Departure from original method in Kellogg et al.

    # Create Groups here
    land_removal_df <- nsink_group_land_off_network(land_removal_df)

    # Weighted average
    wgt_avg_removal <- function(length, removal) {
      sum(length * removal, na.rm = TRUE) / sum(length, na.rm = TRUE)
    }

    land_removal_df <- group_by(land_removal_df, .data$segment_type, .data$group_id)
    land_removal_df <- summarize(land_removal_df,
      segment_id = min(.data$segment_id),
      n_removal = wgt_avg_removal(length, .data$n_removal),
      length = sum(.data$length))
    land_removal_df <- ungroup(land_removal_df)
    land_removal_df <- arrange(land_removal_df, .data$segment_id)
    land_removal_df <- select(land_removal_df, .data$segment_id,
                              .data$segment_type, .data$length,.data$n_removal)

  if (!is.null(network_removal)) {
    # Multiple stream segments in a single lake also increases removal for a
    # lake by a factor of the number of segments.  This code results in only a
    # single n removal per lake based on that lakes estimated percent removal
    network_removal_df <- select(network_removal, .data$segment_id, .data$segment_type,
                                 .data$length, .data$n_removal)
    network_removal_df <- group_by(network_removal_df, .data$segment_id)
    network_removal_df <- mutate(network_removal_df, length = sum(.data$length))
    network_removal_df <- filter(network_removal_df, !duplicated(.data$segment_id))
    network_removal_df <- ungroup(network_removal_df)
    flowpath_removal_df <- rbind(land_removal_df, network_removal_df)
  } else {
    flowpath_removal_df <- land_removal_df
  }

  # Converting NA removal to 0
  flowpath_removal_df <- mutate(flowpath_removal_df,
    n_removal =
      case_when(
        is.na(.data$n_removal) ~ 0,
        TRUE ~ .data$n_removal
      )
  )

  n <- nrow(flowpath_removal_df)
  flowpath_removal_summary <- mutate(flowpath_removal_df,
    n_in = cumprod(c(100, 1 - .data$n_removal))[-n],
    n_out = cumprod(c(100, 1 - .data$n_removal))[-1],
    percent_removal = .data$n_removal * 100
  )
  flowpath_removal_summary <- select(
    flowpath_removal_summary, .data$segment_type,
    length, .data$percent_removal, .data$n_in, .data$n_out
  )
  #round 2, 2, and 3
  flowpath_removal_summary <- mutate(flowpath_removal_summary,
                                     n_in = round(.data$n_in, 2),
                                     n_out = round(.data$n_out, 2),
                                     percent_removal =
                                       round(.data$percent_removal, 3))

  flowpath_removal_summary
}

#' nsink generate from land to nodes
#'
#' Code borrowed from https://www.r-spatial.org/r/2019/09/26/spatial-networks.html
#' @param land_off_network The land off the network path
#' @importFrom dplyr group_indices
#' @keywords internal
nsink_generate_from_to_nodes <- function(land_off_network){

  # Transforming to standardize units and then rounding helps with very small
  # differences that occassionally pop up with the same node.
  land_off_network_geo <- sf::st_transform(land_off_network, crs = 4326)
  nodes <- as_tibble(st_coordinates(land_off_network_geo))
  nodes <- mutate(nodes, X = round(.data$X, 8), Y = round(.data$Y, 8))
  nodes <- rename(nodes, edge_id = .data$L1)
  nodes <- group_by(nodes, .data$edge_id)
  nodes <- slice(nodes, c(1, n()))
  nodes <- ungroup(nodes)
  nodes <- mutate(nodes, start_end = rep(c('start', 'end'), times = n()/2))
  nodes <- mutate(nodes, xy = paste(.data$X, .data$Y))
  nodes <- mutate(nodes, grouping = as.character(
    factor(.data$xy, levels = unique(.data$xy))))
  nodes <- group_by(nodes, .data$grouping)
  nodes <- mutate(nodes, node_id = cur_group_id())
  nodes <- ungroup(nodes)
  nodes <- select(nodes, -.data$xy, -.data$grouping)

  source_nodes <- filter(nodes, .data$start_end == 'start')
  source_nodes <-pull(source_nodes, .data$node_id)

  target_nodes <- filter(nodes, .data$start_end == 'end')
  target_nodes <- pull(target_nodes, .data$node_id)

  land_off_network <- mutate(ungroup(land_off_network), fromnode = source_nodes,
                             tonode = target_nodes)
  land_off_network
}

#' nsink group land off network
#'
#' @param land_removal_df Land and off network summary df
#' @keywords internal
nsink_group_land_off_network <- function(land_removal_df){

  land_removal_df <- mutate(land_removal_df, group_id = case_when(segment_type == "No Removal" ~
                                                 "1",
                                               segment_type == "Hydric" ~
                                                 "2",
                                               segment_type == "Off Network Stream" ~
                                                 .data$remove_id,
                                               segment_type == "Off Network Canal/Ditch" ~
                                                 .data$remove_id,
                                               segment_type == "Off Network Lake" ~
                                                 .data$remove_id,
                                               TRUE ~ as.character(.data$segment_id)))
  land_removal_df
}
