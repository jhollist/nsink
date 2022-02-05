#' Generate and clean a flowpath for N-Sink
#'
#' This function takes an XY location as a starting point and generates a
#' flowpath for use in the N-Sink nitrogen removal analysis. The flowpath is a
#' combination of a flow direction derived flowpath on land plus NHDPlus derived
#' stream-reach flowpath.
#'
#' @param starting_location An \code{\link{sf}} point location as a starting
#'                          point for the flowpath.  Projection must match
#'                          projection in input_data.
#' @param input_data A list of input data with (at least) "fdr", "streams",
#'                  "tot", and "raster_template". These may be generated with
#'                  \code{\link{nsink_prep_data}}.
#' @return An \code{\link{sf}} LINESTRING object of the flowpath that starts at
#'         the \code{starting_location} and ends at the ouflow of the HUC.
#' @export
#' @import sf
#' @examples
#' \dontrun{
#' library(nsink)
#' niantic_huc <- nsink_get_huc_id("Niantic River")$huc_12
#' niantic_data <- nsink_get_data(niantic_huc, data_dir = "nsink_data")
#' aea <- 5072
#' niantic_nsink_data <- nsink_prep_data(niantic_huc, projection = aea,
#'                                       data_dir = "nsink_niantic_data")
#' pt <- c(1948121, 2295822)
#' start_loc <- st_sf(st_sfc(st_point(c(pt)), crs = aea))
#' fp <- nsink_generate_flowpath(start_loc, niantic_nsink_data)
#' }
nsink_generate_flowpath <- function(starting_location, input_data){

  if(st_crs(starting_location) != st_crs(input_data$streams)){
    stop(paste0("The coordinate reference systems for your starting location and the input data do not match.  Re-project to a common reference system."))
  }

  starting_location <- st_transform(starting_location, st_crs(input_data$fdr))
  fp <- raster::flowPath(input_data$fdr, st_coordinates(starting_location))
  fp <- raster::xyFromCell(input_data$fdr, fp)
  # Fixes cases with a single point flowpath: rare but annoying
  if(nrow(fp) == 1){
    dist <- units::set_units(1, "m")
    dist <- units::set_units(dist, st_crs(input_data$streams,
                                          parameters = TRUE)$ud_unit,
                             mode = "standard")
    fp <- rbind(fp, fp + as.numeric(dist))
  }
  fp <- st_sfc(st_linestring(fp), crs = st_crs(input_data$fdr))
  fp <- st_transform(fp, st_crs(input_data$streams))

  fp_ends <- nsink_get_flowpath_ends(fp, input_data$streams, input_data$tot)

  # This is for cases where flowpath doesn't intersect existing flowlines
  dist <- units::set_units(0.001, "m")
  dist <- units::set_units(dist, st_crs(input_data$streams,
                                        parameters = TRUE)$ud_unit,
                           mode = "standard")
  if(nrow(st_as_sf(fp_ends)) >= 2){
    if(length(unlist(st_is_within_distance(fp_ends, input_data$streams,
                                           as.numeric(dist)))) > 0 &
           any(fp_ends[1,] != fp_ends[2,])){
      fp_flowlines <- nsink_get_flowline(fp_ends, input_data$streams, input_data$tot)
    } else {
      fp_flowlines <- NULL
    }
  } else {
    fp_flowlines <- NULL
  }

  fp_ends <- st_sfc(st_geometry(fp_ends))
  fp_ends <- st_sf(fp_ends, crs = st_crs(input_data$streams))

  if(!is.null(fp_flowlines)){
    fp_flowlines <- nsink_split_flowline(fp_ends, fp_flowlines)
  }
  list(flowpath_ends = fp_ends, flowpath_network = fp_flowlines)
}

#' Get flowpath beginning and ends
#'
#' Flowpath from land is only portion that needs to be generated from the flow
#' direction grid.  This function extracts those portions of the generated
#' flowpath.  There may be issues with off network waterbodies...  May need to
#' find all sections without connected flowpaths...
#'
#' @param flowpath An \code{sf} LINESTRING of the flowpath, generated with \code{\link{nsink_generate_flowpath}}
#' @param streams NHDPlus streams from \code{\link{nsink_prep_data}}
#' @param tot NHDPlus time of travel from \code{\link{nsink_prep_data}} which
#'            provides the from and to nodes.
#' @return An \code{sf} object of the portions of the flowpath that are not
#'         represented by the NHDPlus flowlines
#' @import sf
#' @importFrom dplyr filter
#' @keywords internal
nsink_get_flowpath_ends <- function(flowpath, streams, tot){

  #drops streams without traced network from and to nodes
  #Removal from these is dealt with via off_network removal

  if(nrow(streams) > 0){
    streams <- suppressMessages(left_join(streams, tot))
    streams <- filter(streams, !is.na(.data$fromnode))
    streams <- filter(streams, !is.na(.data$tonode))
    streams <- st_difference(st_combine(streams), st_combine(flowpath))
    splits <- lwgeom::st_split(flowpath, st_combine(streams))
    splits <- suppressWarnings(st_collection_extract(splits, "LINESTRING"))
    splits <- nsink_generate_from_to_nodes(
      st_sf(splits, data = data.frame(id = seq_along(splits))))
    first <- which(!splits$fromnode %in% splits$tonode)
    last <- which(!splits$tonode %in% splits$fromnode)
    ends <- splits[c(first, last),]
  } else {ends <- flowpath}
  ends
}

#' Get flowlines that intersect with a flowpath
#'
#' Extract flowlines that intersect with flowpath ends.  This uses the actual
#' flowlines as a part of the flowpath instead of simply using the raster
#' derived flowpaths which do not follow the flowlines exactly.
#'
#'
#' @param flowpath_ends An \code{sf} LINESTRING of the flowpath ends, generated
#'                      with \code{\link{nsink_get_flowpath_ends}}
#' @param streams NHDPlus streams from \code{\link{nsink_prep_data}}
#' @param tot NHDPlus time of travel from \code{\link{nsink_prep_data}} which
#'            provides the from and to nodes.
#' @return An \code{sf} object of the NHDPlus flowlines that occur after a
#'         raster flowpath intersects the stream network.
#' @import sf dplyr
#' @importFrom utils tail
#' @importFrom igraph graph_from_data_frame shortest_paths edge_attr
#' @importFrom rlang .data
#' @keywords internal
nsink_get_flowline <- function(flowpath_ends, streams, tot){

  #filtering out streams without network from and to nodes
  #These are dealt with via off_network removal
  streams_tot <- suppressMessages(left_join(streams, tot))
  streams_tot <- filter(streams_tot, !is.na(.data$fromnode))
  streams_tot <- filter(streams_tot, !is.na(.data$tonode))
  streams_df <- select(streams_tot, .data$fromnode, .data$tonode, .data$stream_comid)
  st_geometry(streams_df) <- NULL
  streams_df <- mutate_all(streams_df, as.character)
  streams_g <- graph_from_data_frame(streams_df, directed = TRUE)
  dist <- units::set_units(0.001, "m")
  dist <- units::set_units(dist, st_crs(streams, parameters = TRUE)$ud_unit,
                           mode = "standard")
  from_nd_idx <- unlist(st_is_within_distance(flowpath_ends[1,], streams_tot, dist))
  to_nd_idx <- unlist(st_is_within_distance(flowpath_ends[2,], streams_tot, dist))
  from_nd <- streams_df[from_nd_idx,]$fromnode
  to_nd <- streams_df[to_nd_idx,]$tonode
  #to_nd <- filter(streams_df, !.data$tonode %in% .data$fromnode)
  #to_nd <- unique(pull(to_nd, .data$tonode))
  idx <- shortest_paths(streams_g, from_nd, to_nd, output = "epath",
                        mode = "out")$epath[[1]]
  if(length(idx) == 0){
    idx <- shortest_paths(streams_g, from_nd, output = "epath",
                          mode = "out")$epath
    idx_lng <- unlist(lapply(idx, length))
    idx_idx <- which(idx_lng == max(idx_lng))
    idx <- shortest_paths(streams_g, from_nd, output = "epath",
                          mode = "out")$epath[[idx_idx]]
  }
  fl_comids <- edge_attr(streams_g, "stream_comid", idx)
  st_agr(flowpath_ends) <- "constant"
  fp_end_pt <- tail(st_cast(flowpath_ends[1,], "POINT"), 1)
  tol1 <- units::set_units(1, "m")
  tol1 <- units::set_units(tol1, st_crs(streams, parameters = TRUE)$ud_unit,
                           mode = "standard")
  tol01 <- units::set_units(0.1, "m")
  tol01 <- units::set_units(tol01, st_crs(streams, parameters = TRUE)$ud_unit,
                            mode = "standard")
  fp_flowlines <- slice(streams_tot, match(fl_comids, streams_tot$stream_comid))
  fp_flowlines <- st_snap(fp_flowlines, fp_end_pt, tolerance = tol1)
  fp_flowlines <- lwgeom::st_split(fp_flowlines, st_combine(fp_end_pt))
  fp_flowlines <- suppressWarnings(st_collection_extract(fp_flowlines,
                                                         "LINESTRING"))
  #browser()
  #fp_flowlines1 <- filter(fp_flowlines, !st_overlaps(st_snap(fp_flowlines,
  #                                                          flowpath_ends[1,],
  #                                                          tol01),
  #                                                  flowpath_ends[1,],
  #                                                  sparse = FALSE))
  fp_flowlines
}

#' Split flowlines where they intersect with a flowpath
#'
#' Takes a flowpath input
#'
#'
#' @param flowpath_ends The ends of the flowpath that are not a part of the
#'                      network
#' @param flowpath_network The flowpath network
#' @return An \code{sf} object of the NHDPlus flowlines split where the fp ends
#'         intersect with the flowlines.
#' @import sf dplyr
#' @importFrom rlang .data
#' @importFrom lwgeom st_split
#' @importFrom sf st_snap st_crs st_collection_extract st_intersects
#' @keywords internal
nsink_split_flowline <- function(flowpath_ends, flowpath_network){

  tol1 <- units::set_units(1, "m")
  tol1 <- units::set_units(tol1, st_crs(flowpath_network, parameters = TRUE)$ud_unit,
                           mode = "standard")
  splits <- st_collection_extract(lwgeom::st_split(flowpath_network,
                                                   flowpath_ends[1,]),
                                  "LINESTRING")
  if(nrow(splits) == nrow(flowpath_network)){
    flowpath_network <- st_snap(flowpath_network, flowpath_ends[1,],
                                tolerance = 0)
    splits <- st_collection_extract(lwgeom::st_split(flowpath_network,
                                                     flowpath_ends[1,]),
                                    "LINESTRING")
  }
  if(nrow(flowpath_network) == 1){
    return(splits)
  } else if(nrow(splits) == nrow(flowpath_network) &
            any(st_is_within_distance(flowpath_network, flowpath_ends[1,], tol1,
                                  sparse = FALSE))){
    return(splits)
  }
  splits <- mutate(splits, split_id = seq_along(.data$stream_comid))
  split_reach_comid <- splits$stream_comid[duplicated(splits$stream_comid)]
  split_reach <- filter(splits, .data$stream_comid == split_reach_comid)
  split_reach_tonode <- unique(split_reach$tonode)
  next_reach <- filter(flowpath_network, .data$fromnode == split_reach_tonode)
  split_reach <- st_snap(split_reach, flowpath_ends, tolerance = tol1)
  split_idx <- !st_intersects(split_reach, next_reach, sparse = FALSE)
  split_ditch <- split_reach$split_id[split_idx]
  splits <- filter(splits, .data$split_id != split_ditch)
  splits
}
