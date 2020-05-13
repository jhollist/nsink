#' Generate and clean a flowpath for N-Sink
#'
#' This function takes an XY location as a starting point and generates a
#' flowpath for use in the N-Sink nitrogen removal analysis. The flopath is a
#' combination of a flow direction derived flowpath on land plus NHDPlus derived
#' stream-reach flowpath.
#'
#' @param starting_location An \code{\link{sf}} point location as a starting
#'                          point for the flowpath.  Projection must match
#'                          projection in input_data
#' @param input_data A list of input data with (at least) "fdr", "streams" and
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
#' aea <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0
#' +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
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
  fp <- suppressWarnings(raster::flowPath(input_data$fdr, st_coordinates(starting_location)))
  fp <- raster::xyFromCell(input_data$raster_template, fp)
  fp <- st_sfc(st_linestring(fp), crs =st_crs(input_data$streams))
  fp_ends <- nsink_get_flowpath_ends(fp, input_data$streams, input_data$tot)
  # This is for cases where flowpath doesn't intersect existing flowlines
  if(fp_ends[1] != fp_ends[2]){
    fp_flowlines <- nsink_get_flowline(fp_ends, input_data$streams, input_data$tot)
  } else {
    fp_flowlines <- NULL
  }

  fp_ends <- st_sfc(fp_ends)
  fp_ends <- st_sf(fp_ends, crs = st_crs(input_data$streams))

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
  #browser()
  streams <- suppressMessages(left_join(streams, tot))
  streams <- filter(streams, !is.na(.data$fromnode))
  streams <- filter(streams, !is.na(.data$tonode))
  streams <- st_difference(st_combine(streams), st_combine(flowpath))
  splits <- lwgeom::st_split(flowpath, st_combine(streams))
  splits <- st_collection_extract(splits, "LINESTRING")
  ends <- splits[c(1,length(splits))]
  ends
}

#' Get flowlines that intersect with a flowpath
#'
#' Extract flowlines that intersect with flowpath ends.  This uses the actual
#' flowlines as a part for the flowpath instead of simply using the raster
#' derived flowpaths wich do not follow the flowlines exactly.
#'
#'
#' @param flowpath_ends An \code{sf} LINESTRING of the flowpath ends, generated
#'                      with \code{\link{nsink_get_flowpath_ends}}
#' @param streams NHDPlus streams from \code{\link{nsink_prep_data}}
#' @param tot NHDPlus time of travel from \code{\link{nsink_prep_data}} which
#'            provides the from and to nodes.
#' @return an \code{sf} object of the NHDPlus flowlines that occur after a
#'         raster flowpath intersects the stream network.
#' @import sf dplyr
#' @importFrom utils tail
#' @importFrom igraph graph_from_data_frame shortest_paths edge_attr
#' @importFrom rlang .data
#' @keywords internal
nsink_get_flowline <- function(flowpath_ends, streams, tot){
  #filtering our streams without network from and to nodes
  streams_tot <- suppressMessages(left_join(streams, tot))
  streams_tot <- filter(streams_tot, !is.na(.data$fromnode))
  streams_tot <- filter(streams_tot, !is.na(.data$tonode))
  streams_df <- select(streams_tot, .data$fromnode, .data$tonode, .data$stream_comid)
  st_geometry(streams_df) <- NULL
  streams_df <- mutate_all(streams_df, as.character)
  streams_g <- graph_from_data_frame(streams_df, directed = TRUE)

  from_nd_idx <- st_is_within_distance(flowpath_ends[1], streams_tot, 0.01)[[1]]
  to_nd_idx <- st_is_within_distance(flowpath_ends[2], streams_tot, 0.01)[[1]]
  from_nd <- streams_df[from_nd_idx,]$fromnode
  to_nd <- streams_df[to_nd_idx,]$tonode
  idx <- shortest_paths(streams_g, from_nd, to_nd, output = "epath",
                        mode = "out")$epath[[1]]
  fl_comids <- edge_attr(streams_g, "stream_comid", idx)
  fp_end_pt <- tail(st_cast(flowpath_ends[1], "POINT"), 1)

  fp_flowlines <- slice(streams_tot, match(fl_comids, streams_tot$stream_comid))
  fp_flowlines <- st_snap(fp_flowlines, fp_end_pt, tolerance = 1)
  fp_flowlines <- lwgeom::st_split(fp_flowlines, st_combine(fp_end_pt))
  fp_flowlines <- st_collection_extract(fp_flowlines, "LINESTRING")
  fp_flowlines <- filter(fp_flowlines, !st_overlaps(st_snap(fp_flowlines,
                                                            flowpath_ends[1],
                                                            0.1),
                                                    flowpath_ends[1],F))
  fp_flowlines
}
