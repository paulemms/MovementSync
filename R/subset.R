#' Subset a View
#'
#' Simple time and column subsetting of views.
#'
#' @param obj
#' @param data_point
#' @param by
#' @param expr
#' @param column
#'
#' @return
#' @exportS3Method
#'
#' @examples
#' r <- get_sample_recording()
#' v <- get_raw_view(r, "Central", "", "Sitar")
#' vv <- subset(v, Time < 10, data_point = "Nose")
#' plot(vv)
subset.View <- function(obj, expr = NULL, data_point = NULL, column = NULL, by = NULL) {
  stopifnot("View" %in% class(obj))

  df <- obj$df
  e <- substitute(expr)
  if (!is.null(e)) {
    is_row_included <- eval(e, df)
  } else {
    is_row_included <- TRUE
  }

  if ("Segment" %in% colnames(df)) {
    leading_col_names <- c("Segment", "Frame" , "Time")
  } else {
    leading_col_names <- c("Frame" , "Time")
  }

  if (is.null(column)) {
    if (is.null(data_point)) data_point <- get_data_points(obj)
    col_names <- setdiff(colnames(df), leading_col_names)
    col_names <- col_names[sub("(.*?)_.*", "\\1", col_names) %in% data_point]
  } else {
    col_names <- column
  }

  sdf <- df[is_row_included, c(leading_col_names, col_names), drop = FALSE]
  if (!is.null(by)) sdf <- sdf[seq(1, nrow(sdf), by = by),,drop = FALSE]

  obj$df <- sdf
  obj
}

