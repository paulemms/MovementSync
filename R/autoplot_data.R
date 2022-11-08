#' Autoplot methods
#'
#' @importFrom ggplot2 autoplot
#' @name autoplot
#' @export
NULL

#' Autoplot a Metre S3 object
#'
#' @importFrom ggplot2 autoplot
#' @param obj
#'
#' @return
#' @export
#'
#' @examples
#' r <- get_recording("NIR_ABh_Puriya", fps = 25)
#' m <- get_metre_data(r)
#' autoplot(m)
#' @exportS3Method
autoplot.Metre <- function(obj) {
  zoo_list <- lapply(obj, function(x) zoo::zoo(diff(x$Time), order.by = x$Time))
  z <- do.call(merge, zoo_list)
  rects <- data.frame(xstart = seq(1000,3000,500), xend = seq(1500,3500,500), col = letters[1:5])

  autoplot(z) +
    ggplot2::labs(title = "Metre Object - Time Between Cycles") +
    ggplot2::xlab("Time / s") +
    ggplot2::geom_rect(data = rects, ggplot2::aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf, fill = col), alpha = 0.4)
}


