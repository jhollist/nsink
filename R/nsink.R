#' nsink: A package that implements flow path analysis of nitrogen removal
#'
#' The N-Sink approach is based off of research outlined in
#' \href{https://doi.org/10.1016/j.ecoleng.2010.02.006}{Kellogg et al (2010)}.
#' This approach builds on peer-reviewed literature in the form of reviews and
#' meta-analyses (i.e., \href{https://doi.org/doi:10.2134/jeq2006.0462}{Mayer
#' et al (2007)}, \href{https://doi.org/10.1111/j.1752-1688.2007.00005.x}{Alexander et al (2007)},
#' and \href{https://doi.org/10.1890/1051-0761(2006)016[2064:DALAWA]2.0.CO;2}{Seitzinger et al (2006)})
#' to estimate nitrogen (N) removal within three
#' types of landscape sinks -- wetlands, streams and lakes -- along any given
#' flow path within a HUC12 basin. The \code{nsink} package implements this
#' approach, using publicly available spatial data to identify flow paths and
#' estimate N removal in landscape sinks. Removal rates depend on retention time,
#' which is influenced by physical characteristics identified using publicly
#' available spatial data -- National Hydrography Dataset (NHD), Watershed
#' Boundary Dataset (WBD), National Land Cover Dataset (NLCD), and Soil Survey
#' Geographic Dataset (SSURGO). Static maps of a specified HUC-12 basin are
#' generated -- N Removal Efficiency, N Transport Efficiency, and N Delivery
#' Index. These maps may be used to inform local decision-making by highlighting
#' areas that are more prone to N "leakiness" and areas that contribute to N
#' removal.
#'
#'
#' @references Kellogg, D. Q., Gold, A. J., Cox, S., Addy, K., & August, P. V.
#'             (2010). A geospatial approach for assessing denitrification sinks
#'             within lower-order catchments. Ecological Engineering, 36(11),
#'             1596-1606.
#'             \href{https://doi.org/10.1016/j.ecoleng.2010.02.006}{Link}
#'
#'             Mayer, P. M., Reynolds Jr., S. K., McCutchen, M. D., & Canfield, T. J.
#'             (2007). Meta-analysis of nitrogen removal in riparian buffers.
#'             J. Environ. Qual. 36, 1172-1180.
#'             \href{https://doi.org/doi:10.2134/jeq2006.0462}{Link}
#'
#'             Alexander, R. B., Boyer, E. W., Smith, R.A., Schwarz, G.E. & Moore, R. B.
#'             (2007). The role of headwater streams in downstream water quality.
#'             J. Am. Water Resou.Assoc. 43, 41-59.
#'             \href{https://doi.org/10.1111/j.1752-1688.2007.00005.x}{Link}
#'
#'             Seitzinger, S. P., Harrison, J.A., Hohlke, J.K., Bouwman, A.F., Lowrance, R.,
#'             Peterson, B., Tobias, C., & Van Drecht, G. (2006). Denitrification across
#'             landscapes and waterscapes: a synthesis. Ecol. Appl. 16, 1064-2090.
#'             \href{https://doi.org/10.1890/1051-0761(2006)016[2064:DALAWA]2.0.CO;2}{Link}
#'
#' @name nsink
NULL
