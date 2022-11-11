#' Summarise Recording object
#'
#' @param obj
#'
#' @return
#' @exportS3Method
#'
#' @examples
#' r <- get_recording("NIR_ABh_Puriya", fps = 25)
#' summary(r)
summary.Recording <- function(obj) {
  obj
}

#' Summarise OnsetsSelected object
#'
#' @param obj
#'
#' @return
#' @exportS3Method
#'
#' @examples
#' r <- get_recording("NIR_ABh_Puriya", fps = 25)
#' o <- get_onsets_selected_data(r)
#' summary(o)
summary.OnsetsSelected <- function(obj) {
  lapply(obj, function(x) if (is.data.frame(x)) summary(x))
}


#' Summarise Metre object
#'
#' @param obj
#'
#' @return
#' @exportS3Method
#'
#' @examples
#' r <- get_recording("NIR_ABh_Puriya", fps = 25)
#' m <- get_metre_data(r)
#' summary(m)
summary.Metre <- function(obj) {
  lapply(obj, function(x) if (is.data.frame(x)) summary(x))
}


#' Summarise a View object
#'
#' @param obj
#'
#' @return
#' @exportS3Method
#'
#' @examples
#' r <- get_recording("NIR_ABh_Puriya", fps = 25)
#' rv <- get_raw_view(r, "Central", "", "Sitar")
#' pv <- get_processed_view(rv)
#' fv <- apply_filter_sgolay(pv, c("Nose", "RWrist", "LWrist"), n=19, p=4)
#' summary(rv)
#' summary(pv)
#' summary(fv)
summary.View <- function(obj) {
  summary(obj$df)
}


