#' Plot a set of data points over time
#'
#' @param v view object
#'
#' @return
#' @export
#'
#' @examples
#' r1 <- get_recording("NIR_ABh_Puriya", fps = 25)
#' rv1 <- get_raw_view(r1, "Central", "", "Sitar")
#' xy_plot(rv1)
#' pv1 <- get_processed_view(rv1)
#' xy_plot(pv1)
xy_plot <- function(v, features = c("LElbow", "RElbow"), maxpts=1000) {
  x_features <- paste(features, "x", sep = "_")
  y_features <- paste(features, "y", sep = "_")

  df <- v$df[seq_len(maxpts), c("Time", x_features, y_features), drop=FALSE]
  df_list <- list()
  for (i in seq_along(features)) {
    df_list[[features[i]]] <- df[c("Time", x_features[i], y_features[i])]
    names(df_list[[features[i]]]) <- c("Time", "x", "y")
  }
  df1 <- dplyr::bind_rows(df_list, .id = "Feature")

  ggplot2::ggplot(df1, ggplot2::aes(x, y, col=Time)) +
    ggplot2::geom_point() + ggplot2::geom_line() + ggplot2::facet_wrap(~Feature)
}
