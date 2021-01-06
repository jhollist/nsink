#' N-Sink plot function for the a list of nsink static maps
#'
#' This function creates a simple plot with pre-selected color palettes from a
#' list of static maps created by \code{\link{nsink_generate_static_maps}}.
#' This is meant as a quick means to visualize the various static maps.
#'
#' @param static_maps A list of \code{raster} objectsm ost likely created
#'                      via \code{\link{nsink_generate_static_maps}}.  The list
#'                      should have removal_effic, delivery_idx, and
#'                      transport_idx.
#' @param map A character of either, "removal","transport", or "delivery."
#' @examples
#' \dontrun{
#' library(nsink)
#' niantic_huc <- nsink_get_huc_id("Niantic River")$huc_12
#' niantic_data <- nsink_get_data(niantic_huc, data_dir = "nsink_data")
#' aea <- 5072
#' niantic_nsink_data <- nsink_prep_data(niantic_huc, projection = aea,
#'                                       data_dir = "nsink_data")
#' removal <- nsink_calc_removal(niantic_nsink_data)
#' static_maps <- nsink_generate_static_maps(niantic_nsink_data, removal,
#' samp_dens = 900)
#' nsink_plot(static_maps, "transport")
#' }
#' @export
nsink_plot <- function(static_maps, map = c("removal", "transport",
                                            "delivery")){
  map <- match.arg(map)
  switch(map,
    "removal" = nsink_plot_removal(static_maps$removal_effic),
    "transport" = nsink_plot_transport(static_maps$transport_idx),
    "delivery" = nsink_plot_delivery(static_maps$delivery_idx)
  )
}

#' N-Sink plot function for the nitrogen delivery index
#'
#' This function creates a simple plot with a pre-selected color palette,
#' although different breaks and colors can be provided by the user.  This is
#' meant as a quick means to visualize delivery index.
#'
#' @param delivery_idx A delivery index \code{raster} most likely created
#'                      via \code{\link{nsink_generate_static_maps}}.
#' @param breaks A vector of values specifying breakpoints to break the
#'               delivery \code{raster}.  Must be one more than number of
#'               colors.
#' @param colors A vector of hexcodes for the colors
#' @importFrom raster plot
#' @export
nsink_plot_delivery <- function(delivery_idx, breaks = c(20, 40, 60, 80, 100),
                                colors = c("#FFBEBE", "#F57A7A","#A80405",
                                           "#652600")){
  suppressWarnings({
  raster::plot(delivery_idx, breaks = breaks, col = colors,
               main = "Nitrogen Delivery Index")
  })
}


#' N-Sink plot function for removal efficiency
#'
#' This function creates a simple plot with a pre-selected color palette,
#' although different breaks and colors can be provided by the user.  This is
#' meant as a quick means to visualize removal efficiency.
#'
#' @param removal_effic A removal efficiency \code{raster} most likely created
#'                      via \code{\link{nsink_generate_static_maps}}.
#' @param breaks A vector of values specifying breakpoints to break the
#'               removal \code{raster}.  Must be one more than number of
#'               colors.
#' @param colors A vector of hexcodes for the colors
#' @importFrom raster plot
#' @export
nsink_plot_removal <- function(removal_effic, breaks = c(0.2, 0.4, 0.6, 0.8),
                               colors = c("#D3FFBE", "#70A800", "#267300")){
  suppressWarnings({
  raster::plot(removal_effic, breaks = breaks, col = colors,
               main = "Nitrogen Removal Efficiency")
  })
}


#' N-Sink plot function for the nitrogen transport index
#'
#' This function creates a simple plot with a pre-selected color palette,
#' although different breaks and colors can be provided by the user.  This is
#' meant as a quick means to visualize removal efficiency.
#'
#' @param transport_idx A transport index \code{raster} most likely created
#'                      via \code{\link{nsink_generate_static_maps}}.
#' @param breaks A vector of values specifying breakpoints to break the
#'               transport \code{raster}.  Must be one more than number of
#'               colors.
#' @param colors A vector of hexcodes for the colors
#' @importFrom raster plot
#' @export
nsink_plot_transport <- function(transport_idx, breaks = c(0, 10, 20, 30, 40, 50,
                                                           60, 70, 80, 90, 100),
                                 colors = c("#38A1D0", "#7AB4C0", "#A2C8B0",
                                            "#C3DC9D", "#E2F088", "#F5E871",
                                            "#F9C159", "#F99844", "#F56A2E",
                                            "#EF2820")){
  suppressWarnings({
  raster::plot(transport_idx, breaks = breaks, col = colors,
               main = "Nitrogen Transport Index")
  })
}


