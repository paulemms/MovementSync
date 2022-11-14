#' Subset a view
#'
#' @param obj
#' @param subset
#' @param data_point
#' @param by
#'
#' @return
#' @exportS3Method
#'
#' @examples
#' r <- get_recording("NIR_ABh_Puriya", fps = 25)
#' v <- get_raw_view(r, "Central", "", "Sitar")
#' vv <- subset(v, Time < 10, data_point = "Nose")
subset.View <- function(obj, subset, data_point, by = NULL) {
  stopifnot("View" %in% class(obj))

  df <- obj$df
  e <- substitute(subset)
  is_row_included <- eval(e, df)

  x_dp <- paste(data_point, "x", sep = "_")
  y_dp <- paste(data_point, "y", sep = "_")
  d_dp <- paste(data_point, "d", sep = "_")

  sdf <- df[is_row_included, c("X" , "Time", rbind(x_dp, y_dp, d_dp)), drop = FALSE]
  if (!is.null(by)) sdf <- sdf[seq(1, nrow(sdf), by = by),,drop = FALSE]

  obj$df <- sdf
  obj
}
