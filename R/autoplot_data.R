#' Autoplot methods
#'
#' @importFrom ggplot2 autoplot
#' @name autoplot
#' @export
NULL

#' Autolayer methods
#'
#' @importFrom ggplot2 autolayer
#' @name autolayer
#' @export
NULL

#' Autoplot a OnsetsSelected S3 object
#'
#' @importFrom ggplot2 autoplot
#' @param obj
#'
#' @return
#' @export
#'
#' @examples
#' r <- get_recording("NIR_ABh_Puriya", fps = 25)
#' o <- get_onsets_selected_data(r)
#' autoplot(o)
#' @exportS3Method
autoplot.OnsetsSelected <- function(obj) {
  class(obj) <- NULL
  df <- dplyr::bind_rows(obj, .id = "Rhythm")

  ggplot2::ggplot(df, ggplot2::aes(Inst, Inst.Peak)) +
    ggplot2::geom_point() +
    ggplot2::facet_grid(Rhythm ~ Inst.Name, scales="free_y") +
    ggplot2::labs(title = "OnsetsSelected Object") +
    ggplot2::xlab("Inst")
}

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

  if (is.null(ncol(z))) {
    autoplot(z) +
      ggplot2::labs(title = "Metre Object", subtitle = "Time Between Cycles") +
      ggplot2::xlab("Time / s") + ggplot2::ylab("")
  } else {
    autoplot(z) +
      ggplot2::facet_grid(Series ~ ., scales="free_y") +
      ggplot2::labs(title = "Metre Object", subtitle = "Time Between Cycles") +
      ggplot2::xlab("Time / s")
  }
}


#' Metre layer for ggplot
#'
#' @param obj
#' @param xmin
#' @param xmax
#' @param ...
#' @param color
#' @param alpha
#'
#' @return ggplot geom_vline object
#'
#' @examples
#' r <- get_recording("NIR_ABh_Puriya", fps = 25)
#' m <- get_metre_data(r)
#' v <- get_raw_view(r, "Central", "", "Sitar")
#' autoplot(v, columns = c("LEar_x", "LEar_y"), maxpts=5000) +
#' ggplot2::xlim(1000, 2000) +
#' autolayer(m)
#' @exportS3Method
autolayer.Metre <- function(obj, xmin = -Inf, xmax = Inf, color = "hotpink", alpha = 0.4, ...) {
  x <- unlist(lapply(obj, function(y) y$Time))
  x[x < xmin] <- NA
  x[x > xmax] <- NA

  ggplot2::geom_vline(xintercept = x, color = color, alpha = alpha, ...)
}


#' Plot a View S3 object
#'
#' @param obj
#' @param columns
#' @param ... passed to plot.zoo
#'
#' @return
#'
#' @examples
#' r <- get_recording("NIR_ABh_Puriya", fps = 25)
#' v <- get_raw_view(r, "Central", "", "Sitar")
#' autoplot(v, columns = c("LEar_x", "LEar_y"))
#' @exportS3Method
autoplot.View <- function(obj, columns=NULL, maxpts=1000, ...) {

  # Restrict points and columns to plot
  columns <- if (is.null(columns)) seq_len(min(ncol(obj$df), 11))[-1] else c("Time", columns)
  sp <- if (nrow(obj$df) > maxpts) sample(nrow(obj$df), maxpts) else seq_len(nrow(obj$df))

  df <- obj$df[sp, columns, drop = FALSE]
  zoo_list <- lapply(df[-1], function(x) zoo::zoo(x, order.by = df$Time))
  z <- do.call(merge, zoo_list)

  title <- c(obj$recording$stem, obj$vid, obj$direct, obj$inst)
  title <- paste(title[title != ""], collapse="_")

  autoplot(z) +
    ggplot2::facet_wrap(Series ~ ., scales="free_y") +
    ggplot2::labs(title = class(obj)[1], subtitle = title) +
    ggplot2::xlab("Time / s")
}


#' Duration layer for ggplot
#'
#' @param obj
#' @param expr
#' @param ...
#'
#' @return
#'
#' @examples
#' r <- get_recording("NIR_ABh_Puriya", fps = 25)
#' m <- get_metre_data(r)
#' d <- get_duration_annotation_data(r)
#' autoplot(m)
#' autoplot(m) + autolayer(d)
#' v <- get_raw_view(r, "Central", "", "Sitar")
#' autoplot(v, columns = c("LEar_x", "LEar_y")) + autolayer(d)
#' autoplot(v, columns = c("LEar_x", "LEar_y")) + autolayer(d, 'Tier == "FORM" & substr(Comments, 1, 1) == "J"')
#' @exportS3Method
autolayer.Duration <- function(obj, expr = 'Tier == "FORM"') {
  expr <- rlang::parse_expr(expr)
  rects <- dplyr::filter(obj, !!expr)
  xmin <- min(obj$In, na.rm = TRUE)
  xmax <- max(obj$Out, na.rm = TRUE)

  ggplot2::geom_rect(
    data = rects,
    ggplot2::aes(xmin = In, xmax = Out, ymin = -Inf, ymax = Inf, fill = Comments),
    alpha = 0.4)
}


#' Get a ggplot2 xlim object based on duration data
#'
#' @param obj
#'
#' @param expr
#'
#' @examples
#' r <- get_recording("NIR_ABh_Puriya", fps = 25)
#' m <- get_metre_data(r)
#' d <- get_duration_annotation_data(r)
#' autoplot(m)
#' autoplot(m) + autolayer(d)
#' v <- get_raw_view(r, "Central", "", "Sitar")
#' autoplot(v, columns = c("LEar_x", "LEar_y")) + autolayer(d)
#' autoplot(v, columns = c("LEar_x", "LEar_y")) +
#' xlim_duration(d, 'Tier == "Form" & substr(Comments, 1, 1) == "J"') +
#' autolayer(d, 'Tier == "Form" & substr(Comments, 1, 1) == "J"')
#' @export
xlim_duration <- function(obj, expr = 'Tier == "Form"') {
  expr <- rlang::parse_expr(expr)
  rects <- dplyr::filter(obj, !!expr)
  xmin <- min(rects$In, na.rm = TRUE)
  xmax <- max(rects$Out, na.rm = TRUE)

  ggplot2::xlim(xmin, xmax)
}
