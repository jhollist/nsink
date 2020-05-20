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
  raster::plot(delivery_idx, breaks = breaks, col = colors,
               main = "Nitrogen Delivery Index")
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
  raster::plot(removal_effic, breaks = breaks, col = colors,
               main = "Nitrogen Removal Efficiency")
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
  raster::plot(transport_idx, breaks = breaks, col = colors,
               main = "Nitrogen Transport Index")
}


