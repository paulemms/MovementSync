# Diagnostic plots of S3 objects

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
#' @exportS3Method
#'
#' @examples
#' r <- get_recording("NIR_ABh_Puriya", fps = 25)
#' o <- get_onsets_selected_data(r)
#' autoplot(o)
autoplot.OnsetsSelected <- function(obj) {
  class(obj) <- NULL
  df <- dplyr::bind_rows(obj, .id = "Rhythm")

  ggplot2::ggplot(df, ggplot2::aes(Inst, Inst.Peak)) +
    ggplot2::geom_point() +
    ggplot2::facet_grid(Rhythm ~ Inst.Name, scales="free_y") +
    ggplot2::labs(title = "OnsetsSelected Object") +
    ggplot2::xlab("Inst / min:sec") +
    ggplot2::scale_x_time(labels = function(l) strftime(l, '%M:%S'))

}


#' OnsetsSelected layer for ggplot
#'
#' @param obj
#' @param xmin
#' @param xmax
#' @param ...
#' @param color
#' @param alpha
#'
#' @return ggplot geom_rect object
#' @exportS3Method
#'
#' @examples
#' r <- get_recording("NIR_ABh_Puriya", fps = 25)
#' o <- get_onsets_selected_data(r)
#' v <- get_raw_view(r, "Central", "", "Sitar")
#' autoplot(v, columns = c("LEar_x", "LEar_y"), maxpts=5000) + autolayer(o)
autolayer.OnsetsSelected <- function(obj, colour = "Inst.Name", fill = "Tala",
                                     alpha = 0.4, ...) {
  class(obj) <- NULL
  df <- dplyr::bind_rows(obj, .id = "Tala")
  df <- dplyr::group_by(df, Tala, Inst.Name)
  rects <- dplyr::summarise(df, Inst_Min=min(Inst, na.rm=TRUE), Inst_Max=max(Inst, na.rm=TRUE))

  ggplot2::geom_rect(
    data = rects,
    ggplot2::aes(xmin = Inst_Min, xmax = Inst_Max, ymin = -Inf, ymax = Inf,
                 colour = !!ggplot2::sym(colour), fill = !!ggplot2::sym(fill)),
    alpha = alpha)
}


#' Autoplot a Metre S3 object
#'
#' @importFrom ggplot2 autoplot
#' @param obj
#'
#' @return
#' @exportS3Method
#'
#' @examples
#' r <- get_recording("NIR_ABh_Puriya", fps = 25)
#' m <- get_metre_data(r)
#' autoplot(m)
autoplot.Metre <- function(obj) {
  zoo_list <- lapply(obj, function(x) zoo::zoo(diff(x$Time), order.by = x$Time))
  z <- do.call(merge, zoo_list)

  if (is.null(ncol(z))) {
    autoplot(z) +
      ggplot2::labs(title = "Metre Object", subtitle = "Time Between Cycles") +
      ggplot2::xlab("Time / min:sec") + ggplot2::ylab("")  +
      ggplot2::scale_x_time(labels = function(l) strftime(l, '%M:%S'))
  } else {
    autoplot(z) +
      ggplot2::facet_grid(Series ~ ., scales="free_y") +
      ggplot2::labs(title = "Metre Object", subtitle = "Time Between Cycles") +
      ggplot2::xlab("Time / min:sec") +
      ggplot2::scale_x_time(labels = function(l) strftime(l, '%M:%S'))
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
#' @exportS3Method
#'
#' @examples
#' r <- get_recording("NIR_ABh_Puriya", fps = 25)
#' m <- get_metre_data(r)
#' v <- get_raw_view(r, "Central", "", "Sitar")
#' autoplot(v, columns = c("LEar_x", "LEar_y"), maxpts=5000) +
#' ggplot2::xlim(1000, 2000) +
#' autolayer(m)
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
#' @exportS3Method
#'
#' @examples
#' r <- get_recording("NIR_ABh_Puriya", fps = 25)
#' v <- get_raw_view(r, "Central", "", "Sitar")
#' autoplot(v, columns = c("LEar_x", "LEar_y"))
autoplot.View <- function(obj, columns=NULL, maxpts=1000, ...) {

  # Restrict points and columns to plot
  columns <- if (is.null(columns)) seq_len(min(ncol(obj$df), 11))[-1] else c("Time", columns)
  sp <- if (nrow(obj$df) > maxpts) sample(nrow(obj$df), maxpts) else seq_len(nrow(obj$df))

  df <- obj$df[sp, columns, drop = FALSE]
  zoo_list <- lapply(df[-1], function(x) zoo::zoo(x, order.by = df$Time))
  z <- do.call(merge, zoo_list)

  subtitle <- c(obj$recording$stem, obj$vid, obj$direct, obj$inst)
  subtitle <- paste(subtitle[subtitle != ""], collapse="_")

  autoplot(z) +
    ggplot2::facet_wrap(Series ~ ., scales="free_y") +
    ggplot2::labs(title = class(obj)[1], subtitle = subtitle) +
    ggplot2::xlab("Time / min:sec") +
    ggplot2::scale_x_time(labels = function(l) strftime(l, '%M:%S'))
}


#' Plot a SplicedView S3 object
#'
#' @param obj
#' @param columns
#' @param maxpts
#'
#' @return
#' @exportS3Method
#'
#' @examples
#' r <- get_recording("NIR_ABh_Puriya", fps = 25)
#' rv <- get_raw_view(r, "Central", "", "Sitar")
#' pv <- get_processed_view(rv)
#' l <- list(a = c(0, 300), b = c(300, 600), c = c(600, 900))
#' splicing_df <- splice_time(l)
#' sv <- get_spliced_view(pv, splicing_df)
#' autoplot(sv, columns = c("LEar_x", "LEar_y"), maxpts = 1000)
autoplot.SplicedView <- function(obj, columns=NULL, maxpts=1000) {

  # Restrict points, columns, splices to plot
  columns <- if (is.null(columns)) {
    if (ncol(obj$df) > 9) warning("Only plotting first six data columns")
    colnames(obj$df)[seq_len(min(ncol(obj$df), 9))]
  } else c("Tier", "Frame", "Time", columns)

  stopifnot(all(columns %in% colnames(obj$df_list)))

  sp <- if (nrow(obj$df) > maxpts) {
    warning("Sampling rows for plotting")
    sample(nrow(obj$df), maxpts)
  } else seq_len(nrow(obj$df))

  df <- obj$df[sp, columns, drop = FALSE]
  tiers <- unique(obj$df$Tier)
  if (length(tiers) > 10) {
    warning("Only plotting the first 10 splices")
    df <- df[df$Tier %in% tiers[1:10], , drop=FALSE]
  }

  long_df <- tidyr::pivot_longer(df, cols = columns[-(1:3)],
                                 names_to = "Series", values_to = "Value")
  start_df <- dplyr::group_by(long_df, Tier)
  start_df <- dplyr::summarize(start_df, Start = min(Frame, na.rm=TRUE))
  start_df <- dplyr::arrange(start_df, Start)

  long_df$Tier_f <- factor(long_df$Tier, levels = start_df$Tier)

  subtitle <- c(obj$recording$stem, obj$vid, obj$direct, obj$inst)
  subtitle <- paste(subtitle[subtitle != ""], collapse="_")

  ggplot2::ggplot(long_df, ggplot2::aes(x = Time, y = Value, col = Series)) +
    ggplot2::geom_point() + ggplot2::geom_line() +
    ggplot2::labs(title = class(obj)[1], subtitle = subtitle) +
    ggplot2::xlab("Time / min:sec") +
    ggplot2::scale_x_time(labels = function(l) strftime(l, '%M:%S')) +
    ggplot2::facet_wrap(~Tier_f, scales = "free_x")
}


#' Duration layer for ggplot
#'
#' @param obj
#' @param fill_column
#' @param geom
#' @param vline_column
#' @param expr
#' @param ... passed to geom_vline
#'
#' @return
#' @exportS3Method
#'
#' @examples
#' r <- get_recording("NIR_ABh_Puriya", fps = 25)
#' m <- get_metre_data(r)
#' d <- get_duration_annotation_data(r)
#' autoplot(m)
#' autoplot(m) + autolayer(d)
#' autoplot(m) + autolayer(d, fill_col = "Tier")
#'
#' v <- get_raw_view(r, "Central", "", "Sitar")
#' autoplot(v, columns = c("LEar_x", "LEar_y")) + autolayer(d)
#' autoplot(v, columns = c("LEar_x", "LEar_y")) + autolayer(d, 'Tier == "FORM" & substr(Comments, 1, 1) == "J"')
#' autoplot(v, columns = c("LEar_x", "LEar_y")) + autolayer(d, geom = "vline", nudge_x = -60, size = 3, colour = "blue")
autolayer.Duration <- function(obj, expr = 'Tier == "FORM"', fill_column = "Comments",
                               geom = "rect", vline_column = "In", ...) {
  expr <- rlang::parse_expr(expr)
  df <- as.data.frame(obj)
  rects <- dplyr::filter(df, !!expr)
  # order the fill column for legend
  rects[fill_column] <- factor(rects[[fill_column]], levels = unique(rects[[fill_column]]))

  l <- list(...)

  if (geom == "rect") {
    ggplot2::geom_rect(
      data = rects,
      ggplot2::aes(xmin = In, xmax = Out, ymin = -Inf, ymax = Inf, fill = .data[[fill_column]]),
      alpha = 0.4)
  } else if (geom == "vline") {
    colour <-  if ("colour" %in% names(l)) l[["colour"]] else "black"
    c(ggplot2::geom_vline(data = rects, linetype = 3, colour = colour, ggplot2::aes(xintercept = .data[[vline_column]])),
      ggplot2::geom_text(
        data = rects,
        ggplot2::aes(x = .data[[vline_column]], y = Inf, angle = 90, hjust = "inward", label = paste(vline_column, .data[[fill_column]])),
        ...)
      )
  } else stop("Unsupported geom")
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


