# Diagnostic plots of S3 objects

#' Diagnostic plots
#'
#' Autoplot methods for S3 objects in the movementsync package.
#' @param obj S3 object
#' @param columns names of columns in input data
#' @param maxpts maximum number of points to plot
#' @param ... passed to [zoo::plot.zoo()]
#' @param segments
#'
#' @return ggplot object
#' @importFrom ggplot2 autoplot
#' @name autoplot
#' @export
#' @examples
#' r <- get_sample_recording()
#' d <- get_duration_annotation_data(r)
#' autoplot(d)
#' o <- get_onsets_selected_data(r)
#' autoplot(o)
#' m <- get_metre_data(r)
#' autoplot(m)
#' v <- get_raw_view(r, "Central", "", "Sitar")
#' autoplot(v, columns = c("LEar_x", "LEar_y"))
#' l <- list(a = c(0, 10), b = c(20, 30), c = c(30, 60))
#' splicing_df <- splice_time(l)
#' sv <- get_spliced_view(v, splicing_df)
#' autoplot(sv, columns = c("LEar_x", "LEar_y", "Nose_x", "Nose_y"), maxpts = 1000)
NULL


#' @exportS3Method
#' @rdname autoplot
autoplot.Duration <- function(obj) {
  ggplot2::ggplot(obj) +
    ggplot2::geom_col(ggplot2::aes(x = Tier, y = Duration, fill = In),
                      position = "stack") +
    ggplot2::labs(title = "Duration Object") +
    ggplot2::scale_fill_viridis_b() +
    ggplot2::ylab("Duration / min:sec") +
    ggplot2::scale_y_time(labels = function(l) strftime(l, '%M:%S'))
}


#' @exportS3Method
#' @rdname autoplot
autoplot.OnsetsSelected <- function(obj, instrument = 'Inst', matra = 'Matra') {

  dfr_list <- obj[sapply(obj, class) == 'data.frame']
  df <- dplyr::bind_rows(dfr_list, .id = "Tala")
  stopifnot(instrument %in% colnames(df), matra %in% colnames(df))

  df <- dplyr::rename(df, 'Matra' = matra)
  df <- df[!is.na(df[instrument]),,drop=FALSE]

  ggplot2::ggplot(df) +
    ggplot2::geom_bar(ggplot2::aes(x = Matra, fill = Tala), stat = "count",
                      position = ggplot2::position_dodge2(width = 0.9, preserve = 'single')) +
    ggplot2::ylab("Onset Count") +
    ggplot2::labs(title = paste("OnsetsSelected Object:", instrument))

}


#' @exportS3Method
#' @rdname autoplot
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


#' @exportS3Method
#' @rdname autoplot
autoplot.View <- function(obj, columns=NULL, maxpts=1000, ...) {

  max_num_cols <- 9

  # Restrict points and columns to plot
  columns <- if (is.null(columns)) {
    if (ncol(obj$df) > max_num_cols + 2)
      warning(paste("Only plotting first", max_num_cols, "data columns"))
    seq_len(min(ncol(obj$df), max_num_cols + 2))[-1]
  } else c("Time", columns)
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


#' @exportS3Method
#' @rdname autoplot
autoplot.SplicedView <- function(obj, columns=NULL, segments=NULL, maxpts=1000) {

  max_num_segments <- 10
  max_num_cols <- 9
  df <- obj$df

  # Restrict points, columns, splices to plot
  columns <- if (is.null(columns)) {
    if (ncol(df) > max_num_cols + 3)
      warning(paste("Only plotting first", max_num_cols, "data columns"))
    colnames(df)[seq_len(min(ncol(df), max_num_cols + 3))]
  } else c("Segment", "Frame", "Time", columns)

  stopifnot(all(columns %in% colnames(df)))

  df_segments <- unique(df$Segment)
  num_segments <- length(df_segments)
  if (is.null(segments)) {
    if (num_segments > max_num_segments) {
      warning(paste("Only plotting the first", max_num_segments, "segments"))
      df <- df[df$Segment %in% df_segments[seq_len(max_num_segments)], , drop=FALSE]
      num_segments <- max_num_segments
    }
  } else {
    if (!all(segments %in% df_segments)) stop('Segments not found in SplitView')
    df <- df[df$Segment %in% segments, , drop=FALSE]
  }

  sp <- if (nrow(df) > maxpts) {
    warning("Sampling rows for plotting")
    sample(seq_len(nrow(df)), maxpts)
  } else seq_len(nrow(df))
  df <- df[sp, columns, drop = FALSE]

  # Convert data to long form
  columns_to_remove <- match(c("Segment", "Frame", "Time"), colnames(df), nomatch = 0)
  long_df <- tidyr::pivot_longer(df, cols = -columns_to_remove,
                                 names_to = "Series", values_to = "Value")

  # Find Start and Duration of each Segment
  start_df <- dplyr::group_by(long_df, Segment)
  start_df <- dplyr::summarize(start_df, Start = min(Time, na.rm=TRUE),
                               Duration = max(Time, na.rm=TRUE) - Start)
  start_df <- dplyr::arrange(start_df, Start)

  long_df$Segment_f <- factor(long_df$Segment, levels = start_df$Segment)

  subtitle <- c(obj$recording$stem, obj$vid, obj$direct, obj$inst)
  subtitle <- paste(subtitle[subtitle != ""], collapse="_")

  # Use seconds to time scale if max Duration less than a minute
  if (max(start_df$Duration, na.rm = TRUE) < 60) {
    xlab <- ggplot2::xlab("Time / sec")
    scale_x_time <- NULL
  } else {
    xlab <- ggplot2::xlab("Time / min:sec")
    scale_x_time <- ggplot2::scale_x_time(labels = function(l) strftime(l, '%M:%S'))
  }

  ggplot2::ggplot(long_df, ggplot2::aes(x = Time, y = Value, col = Series)) +
    ggplot2::geom_point() + ggplot2::geom_line() +
    ggplot2::labs(title = class(obj)[1], subtitle = subtitle) +
    xlab + scale_x_time +
    ggplot2::facet_wrap(~Segment_f, scales = "free_x")
}


#' Autolayer methods
#'
#' Layers of annotation data to add to ggplots.
#' @param obj S3 object
#'
#' @return ggplot geom object
#'
#' @importFrom ggplot2 autolayer
#' @name autolayer
#' @export
#' @examples
#' \dontrun{
#' r <- get_recording("NIR_ABh_Puriya", fps = 25)
#' o <- get_onsets_selected_data(r)
#' v <- get_raw_view(r, "Central", "", "Sitar")
#' autoplot(v, columns = c("LEar_x", "LEar_y"), maxpts=5000) + autolayer(o)
#'
#' m <- get_metre_data(r)
#' autoplot(v, columns = c("LEar_x", "LEar_y"), maxpts=5000) +
#' ggplot2::xlim(1000, 2000) + autolayer(m)
#'
#' d <- get_duration_annotation_data(r)
#' autoplot(m)
#' autoplot(m) + autolayer(d)
#' autoplot(m) + autolayer(d, fill_col = "Tier")
#'
#' v <- get_raw_view(r, "Central", "", "Sitar")
#' autoplot(v, columns = c("LEar_x", "LEar_y")) +
#'   autolayer(d)
#' autoplot(v, columns = c("LEar_x", "LEar_y")) +
#'   autolayer(d, 'Tier == "FORM" & substr(Comments, 1, 1) == "J"')
#' autoplot(v, columns = c("LEar_x", "LEar_y")) +
#'   autolayer(d, geom = "vline", nudge_x = -60, size = 3, colour = "blue")
#' }
NULL


#' @param alpha
#'
#' @param colour
#' @param fill
#' @param obj
#' @param instrument_cols
#' @param ...
#'
#' @exportS3Method
#' @rdname autolayer
autolayer.OnsetsSelected <- function(obj, colour = "Inst.Name", fill = "Tala",
                                     alpha = 0.4, instrument_cols = NULL, ...) {
  dfr_list <- obj[sapply(obj, class) == 'data.frame']
  df <- dplyr::bind_rows(dfr_list, .id = "Tala")
  if (!is.null(instrument_cols)) {
    df <- tidyr::pivot_longer(df, cols = instrument_cols, names_to = "Inst.Name",
                              values_to = "Inst")
  }
  df <- dplyr::group_by(df, Tala, Inst.Name)
  rects <- dplyr::summarise(df, Inst_Min=min(Inst, na.rm=TRUE), Inst_Max=max(Inst, na.rm=TRUE))

  ggplot2::geom_rect(
    data = rects,
    ggplot2::aes(xmin = Inst_Min, xmax = Inst_Max, ymin = -Inf, ymax = Inf,
                 colour = !!ggplot2::sym(colour), fill = !!ggplot2::sym(fill)),
    alpha = alpha)
}


#' @param xmin
#' @param xmax
#' @param colour
#' @param alpha
#' @param ...
#' @exportS3Method
#' @rdname autolayer
autolayer.Metre <- function(obj, xmin = -Inf, xmax = Inf, colour = "hotpink", alpha = 0.4, ...) {
  x <- unlist(lapply(obj, function(y) y$Time))
  x[x < xmin] <- NA
  x[x > xmax] <- NA

  ggplot2::geom_vline(xintercept = x, colour = colour, alpha = alpha, ...)
}

#' @param expr logical expression for filtering
#' @param fill_column data column used for fill
#' @param geom
#' @param vline_column
#' @param ...
#' @exportS3Method
#' @rdname autolayer
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
#' \dontrun{
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
#' }
#' @export
xlim_duration <- function(obj, expr = 'Tier == "Form"') {
  expr <- rlang::parse_expr(expr)
  rects <- dplyr::filter(obj, !!expr)
  xmin <- min(rects$In, na.rm = TRUE)
  xmax <- max(rects$Out, na.rm = TRUE)

  ggplot2::xlim(xmin, xmax)
}


