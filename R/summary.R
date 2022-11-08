#' Summarise a recording
#'
#' @param o
#'
#' @return
#' @exportS3Method
#'
#' @examples
#' r <- get_recording("NIR_ABh_Puriya", fps = 25)
#' summary(r)
summary.Recording <- function(o) {
  o$data_files
}


#' Summarise a RawView
#'
#' @param o
#'
#' @return
#' @exportS3Method
#'
#' @examples
#' r <- get_recording("NIR_ABh_Puriya", fps = 25)
#' v <- get_raw_view(r, "Central", "", "Sitar")
#' summary(v)
summary.RawView <- function(o) {
  colnames(o$df)
}
