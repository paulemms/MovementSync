#' Plot a Metre S3 object
#'
#' @param x
#'
#' @return
#' @exportS3Method
#'
#' @examples
#' o <- get_metre_data("NIR_ABh_Puriya")
#' plot(o)
plot.Metre <- function(o) {
  zoo_list <- lapply(o, function(x) zoo::zoo(x$Cycle, order.by = x$Time))
  z <- do.call(merge, zoo_list)
  plot(z, yax.flip = TRUE, main = "Metre")
}

