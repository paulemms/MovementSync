#' Estimate the periodicity of data points in a view
#'
#' @param view ProcessedView or FilteredView object
#' @param data_points Data points to process
#' @param ...
#'
#' @return PeriodicityView object
#' @export
#' @examples
#' r <- get_recording("NIR_ABh_Puriya", fps = 25)
#' rv <- get_raw_view(r, "Central", "", "Sitar")
#' pv <- get_processed_view(rv)
#' per <- periodicity(pv, data_points = c("LEar", "REar"), spans = 5)
#'
#' fv <- apply_filter_sgolay(pv, data_points = c("LEye"), n = 19, p = 4)
#' per <- periodicity(fv, data_points = c("LEye"), spans = 5)
#'
periodicity <- function(view, data_points = NULL, ...) {
  stopifnot(any(c("ProcessedView", "FilteredView") %in% class(view)))

  df <- view$df
  if (!is.null(data_points)) {
    x_colnames <- paste0(data_points, "_x")
    y_colnames <- paste0(data_points, "_y")
    d_colnames <- paste0(data_points, "_d")
    cn <- c(colnames(df)[1:2], rbind(x_colnames, y_colnames, d_colnames))
    df <- df[, cn, drop=FALSE]
  }

  sampling_rate <-view$recording$fps

  # periodigram using fft with smoothing
  spec <-
    spectrum(
      df[, -(1:2), drop=FALSE],
      plot = FALSE,
      na.action = na.omit,
      ...
    )
  spx <- spec$freq * sampling_rate # to get cycles per seconds
  spy <- spec$spec
  colnames(spy) <- colnames(df)[-(1:2)]
  output_df <- data.frame(Time = 1 / spx, spy)

  l <- list(df = output_df, view = view)
  class(l) <- "PeriodicityView"

  l
}


#' Autoplot a PeriodicityView S3 object
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
#' per1 <- periodicity(pv, data_points = c("LElbow", "LEye"), spans = 5)
#' autoplot(per1)
#'
#' fv <- apply_filter_sgolay(pv, data_points = c("LElbow", "LEye"), n = 19, p = 4)
#' per2 <- periodicity(fv, data_points = c("LElbow", "LEye"), spans = 5)
#' autoplot(per2)
autoplot.PeriodicityView <- function(obj, time_range = c(0, 10), colour = "blue") {

  view <- obj$view
  df <- obj$df
  long_df <- tidyr::pivot_longer(df, colnames(df[-1]))
  colnames(long_df) <- c("Time", "DataPoint", "Ampl")

  title <- c(view$recording$stem, view$vid, view$direct, view$inst)
  title <- paste(title[title != ""], collapse="_")

  # Restrict time domain
  long_df <- dplyr::filter(long_df, Time >= time_range[1] & Time <= time_range[2])

  ggplot2::ggplot(long_df, ggplot2::aes(x = Time, y = Ampl)) +
  ggplot2::geom_line(colour = colour) +
  ggplot2::xlab('Time / min:sec') +
  ggplot2::ylab('Amplitude') +
  # ggplot2::scale_x_continuous(
  #   limits = c(time_range[1], time_range[2]),
  #   breaks = seq(time_range[1], time_range[2], by = (time_range[2]-time_range[1]) / 5)
  # ) +
  ggplot2::labs(title = class(obj)[1], subtitle = title) +
  ggplot2::facet_wrap(~DataPoint, scales = "free") +
  ggplot2::scale_x_time(labels = function(l) strftime(l, '%M:%S'))
}


#' Specgram Plot
#'
#' @param obj
#' @param ... passed to signal::specgram
#'
#' @return
#' @export
#'
#' @examples
#' r <- get_recording("NIR_ABh_Puriya", fps = 25)
#' rv <- get_raw_view(r, "Central", "", "Sitar")
#' pv <- get_processed_view(rv)
#' fv <- apply_filter_sgolay(pv, data_points = c("RWrist"), n = 11, p = 4)
#' sub_fv <- subset(fv, Time >= 15*60 & Time <= 25*60, column = c("RWrist_x", "RWrist_y"))
#' specgram_plot(sub_fv)
#'
#' r2 <- get_recording("NIR_DBh_Malhar_2Gats", fps = 25)
#' rv_list <- get_raw_views(r2)
#' pv_list <- lapply(rv_list, get_processed_view)
#' fv_list <- lapply(pv_list, apply_filter_sgolay, data_points = "RWrist", n = 11, p = 3)
#' sub_fv <- subset(fv_list$SideL_Tabla, Time <= 1700, column = c("RWrist_x", "RWrist_y"))
#' specgram_plot(sub_fv)
#'
#' specgram_plot(sub_fv, window = 200) + ggplot2::scale_fill_gradient(low = "white", high = "black")
specgram_plot <- function(obj, ...) {
  stopifnot("View" %in% class(obj))

  view <- obj$view
  start_time <- obj$df[1, "Time"]
  df <- obj$df[-(1:2)]

  sp_list <- lapply(df, signal::specgram, Fs = obj$recording$fps, ...)
  df_list <- list()
  for (i in seq_along(data_point)) {
    df_list[[data_point[i]]] <- expand.grid(X = sp_list[[i]]$t + start_time, Y = sp_list[[i]]$f)
    df_list[[data_point[i]]]$Z <- as.numeric(20 * log10(t(abs(sp_list[[i]]$S))))
  }
  long_df <- dplyr::bind_rows(df_list, .id = "DataPoint")

  subtitle <- c(obj$recording$stem, obj$vid, obj$direct, obj$inst)
  subtitle <- paste(subtitle[subtitle != ""], collapse="_")

  ggplot2::ggplot(long_df, aes(X, Y, fill= Z)) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_gradientn(colours = jet(20)) +
    ggplot2::labs(title = "Specgram", subtitle = subtitle) +
    ggplot2::theme(legend.position = "none") +
    ggplot2::xlab("Time / min:sec") +
    ggplot2::ylab("Frequency") +
    ggplot2::scale_x_time(expand = c(0,0), labels = function(l) strftime(l, '%M:%S')) +
    ggplot2::scale_y_continuous(expand = c(0,0)) +
    ggplot2::facet_grid(rows = vars(DataPoint))
}


