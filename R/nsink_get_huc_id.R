#' Look up HUC 12 ID from a HUC name
#'
#' This function takes a HUC Name and returns all possible matching HUC 12 IDs.
#'
#' @param huc_name character string of a HUC Name
#' @return A data frame with HUC_12 and HU_12_NAME that match the huc_name
#' @importFrom dplyr tibble
#' @export
#' @examples
#' nsink_get_huc_id(huc_name = "Niantic River")
nsink_get_huc_id <- function(huc_name){
  wbd_match <- wbd_lookup[grepl(huc_name, wbd_lookup$HU_12_NAME),]
  tibble(huc_12 = wbd_match$HUC_12, huc_12_name = wbd_match$HU_12_NAME,
            state = wbd_match$STATES)
}
