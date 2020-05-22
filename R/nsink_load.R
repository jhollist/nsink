#' Load an existing N-Sink analysis folder
#'
#'
#' @param input_folder Folder to that contains nsink files produced by
#'                     \code{\link{nsink_build}}
#' @param base_name a base name used to assign objects to the global environment.
#' @export
#' @return Creates several lists in the global environment that would normally
#'         be created when running an N-Sink analysis.  These include:
#'         a \code{\link{nsink_prep_data}} object,
#'         a \code{\link{nsink_calc_removal}} object, and a
#'         \code{\link{nsink_generate_static_maps}} object
#' @importFrom sf st_read
#' @importFrom raster raster
#' @examples
#' \dontrun{
#' library(nsink)
#'
#' aea <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0
#' +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
#' nsink_build(nsink_get_huc_id("Niantic River")$huc_12, aea,
#'             output_folder = "nsink_output", samp_dens = 300)
#' nsink_load(input_folder = "nsink_output",
#'            basename = "nsink_")
#' }
nsink_load <- function(input_folder, base_name = "nsink_"){

  if(!dir.exists(input_folder)){
    stop(paste("The input folder,", input_folder, "does not currently exist, please create it with nsink_build."))
  }

  input_folder <- nsink_fix_data_directory(input_folder)
  huc_sf <- st_read(paste0(input_folder, "huc.shp"))
  prep <- list(streams = st_read(paste0(input_folder,"streams.shp")),
               lakes = st_read(paste0(input_folder,"lakes.shp")),
               fdr = raster(paste0(input_folder, "fdr.tif")),
               impervious = raster(paste0(input_folder, "impervious.tif")),
               nlcd = raster(paste0(input_folder, "nlcd.tif")),
               ssurgo = st_read(paste0(input_folder,"ssurgo.shp")),
               q = read.csv(paste0(input_folder, "q.csv")),
               tot = read.csv(paste0(input_folder, "tot.csv")),
               lakemorpho = read.csv(paste0(input_folder, "lakemorpho.csv")),
               huc = huc_sf,
               raster_template = fasterize::raster(as(huc_sf, "Spatial"),
                                                   resolution = 30,
                                                   crs = st_crs(huc_sf))
               )
  # The shapefile driver butchers output names, need to restore them.
  names(prep$streams) <- c("stream_comid", "fdate", "resolution", "gnis_id",
                           "gnis_name", "lengthkm", "reachcode", "flowdir",
                           "lake_comid", "ftype", "fcode", "shape_leng",
                           "enabled", "gnis_nbr", "geometry")
  names(prep$lakes) <- c("lake_comid", "fdate", "resolution", "gnis_id", "gnis_name",
                         "areasqkm", "elevation", "reachcode", "ftype", "fcode", "shape_leng",
                         "shape_area", "geometry")
  names(prep$ssurgo) <- c("areasymbol", "spatialver", "musym", "mukey", "hydricrating",
                          "hydric_pct", "geometry")


  removal <- nsink_calc_removal(prep)
  static <- list(removal_effic = raster(paste0(input_folder, "removal_effic.tif")),
                 loading_idx = raster(paste0(input_folder, "loading_idx.tif")),
                 transport_idx = raster(paste0(input_folder, "transport_idx.tif")),
                 delivery_idx = raster(paste0(input_folder, "delivery_idx.tif")))





  assign(paste0(base_name,"data"), prep, envir = parent.frame())
  assign(paste0(base_name,"removal"), removal, envir = parent.frame())
  assign(paste0(base_name,"static_maps"), static, envir = parent.frame())
}


