#' Get remote path for NHDPlus components
#'
#' Code modified from https://github.com/jsta/nhdR/blob/master/R/utils.R.  Still
#' need to figure out best way to acknowledge jsta as author and include GPL
#' @export

nsink_get_plus_remotepath <- function (vpu, component = c("NHDSnapshot",
                                                          "FdrFac",
                                                          "EROMExtension",
                                                          "WBDSnapshot",
                                                          "NHDPlusAttributes")){
  component <- match.arg(component)
  baseurl <- paste0("http://www.horizon-systems.com/nhdplus/NHDPlusV2_", vpu,
                    ".php")
  res <- rvest::html_attrs(rvest::html_nodes(xml2::read_html(baseurl),
                                             "a"))
  res <- unlist(res[grepl(component, res)])
  res <- res[!grepl("FGDB", res)]
  res <- res[!grepl(".pdf", res)]
  res <- res[!grepl("ftp://", res)]
  res
}
