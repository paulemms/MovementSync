# Functions for wavelet analysis

#' Analyze Wavelet from View object
#'
#' @param obj View object
#' @param column Column in view to analyse
#' @param loess.span
#' @param dj
#' @param lowerPeriod in seconds
#' @param upperPeriod in seconds
#' @param make.pval
#' @param method
#' @param params
#' @param n.sim
#' @param date.format
#' @param date.tz
#' @param verbose
#'
#' @return
#' @export
#'
#' @examples
#' r <- get_sample_recording()
#' rv <- get_raw_view(r, "Central", "", "Sitar")
#' pv <- get_processed_view(rv)
#' w <- analyze_wavelet(pv, "Nose_y")
analyze_wavelet <- function(obj, column, loess.span = 0, dj = 1/20,
                            lowerPeriod = 1 / obj$recording$fps, upperPeriod = 5,
                            make.pval = TRUE, method = "white.noise", params = NULL,
                            n.sim = 1, date.format = NULL, date.tz = NULL, verbose = TRUE) {
  stopifnot("View" %in% class(obj), column %in% colnames(obj$df))

  df <- obj$df
  fps <- obj$recording$fps
  dt <- 1 / fps

  w <- WaveletComp::analyze.wavelet(
    df, column, loess.span = loess.span, dt = dt, dj = dj,
    lowerPeriod = lowerPeriod * fps, upperPeriod = upperPeriod * fps, make.pval = make.pval,
    n.sim = n.sim, date.format = date.format, date.tz = date.tz, verbose = verbose
  )

  subtitle <- c(obj$vid, obj$direct, obj$inst, column)
  subtitle <- paste(subtitle[subtitle != ""], collapse="_")
  w$subtitle <- subtitle

  invisible(w)
}


#' Analyze Coherency from View object
#'
#' @param obj View object
#' @param columns Two column names
#' @param loess.span
#' @param dj
#' @param lowerPeriod in seconds
#' @param upperPeriod in seconds
#' @param window.type.t
#' @param window.type.s
#' @param window.size.t
#' @param window.size.s
#' @param make.pval
#' @param method
#' @param params
#' @param n.sim
#' @param date.format
#' @param date.tz
#' @param verbose
#'
#' @return
#' @export
#'
#' @examples
#' r <- get_sample_recording()
#' rv <- get_raw_view(r, "Central", "", "Sitar")
#' pv <- get_processed_view(rv)
#' co <- analyze_coherency(pv, c("Nose_x", "Nose_y"))
analyze_coherency <- function(obj, columns, loess.span = 0,
  dj = 1/50, lowerPeriod = 1 / obj$recording$fps, upperPeriod = 5, window.type.t = 1,
  window.type.s = 1, window.size.t = 5,
  window.size.s = 1/4, make.pval = TRUE, method = "white.noise",
  params = NULL, n.sim = 1, date.format = NULL, date.tz = NULL,
  verbose = TRUE) {

  stopifnot("View" %in% class(obj), length(columns) == 2,
            all(columns %in% colnames(obj$df)))

  df <- obj$df
  fps <- obj$recording$fps
  dt <- 1 / fps

  co <- WaveletComp::analyze.coherency(
    df, my.pair = columns, loess.span = loess.span, dt = dt,
    dj = dj, lowerPeriod = lowerPeriod * fps, upperPeriod = upperPeriod * fps, window.type.t = window.type.t,
    window.type.s = window.type.s, window.size.t = window.size.t,
    window.size.s = window.size.s, make.pval = make.pval, method = method,
    params = params, n.sim = n.sim, date.format = date.format, date.tz = date.tz,
    verbose = verbose)

  subtitle <- c(obj$vid, obj$direct, obj$inst, columns)
  subtitle <- paste(subtitle[subtitle != ""], collapse="_")
  co$subtitle <- subtitle

  invisible(co)

}


#' Plot a power spectrum of a wavelet object
#'
#' @param obj analyze.wavelet object
#' @param view View object
#' @param ... passed to [WaveletComp::wt.image()]
#'
#' @return
#' @export
#'
#' @examples
#' r <- get_sample_recording()
#' rv <- get_raw_view(r, "Central", "", "Sitar")
#' pv <- get_processed_view(rv)
#' pv1 <- subset(pv, Time >= 30)
#' w <- analyze_wavelet(pv1, "Nose_y")
#' plot_power_spectrum(w, pv1)
#' w <- analyze_wavelet(pv1, "Nose_y", lowerPeriod = 0.01, upperPeriod = 10)
#' plot_power_spectrum(w, pv1)
plot_power_spectrum <- function(obj, view, ...) {
  stopifnot("analyze.wavelet" %in% class(obj), "View" %in% class(view))

  df <- view$df
  fps <- view$recording$fps
  spec_time_axis <- make_time_axis(df, fps)

  axis.2 <- obj$axis.2
  period.tick <- unique(trunc(axis.2))
  period.tick[period.tick < log2(obj$Period[1])] <- NA
  period.tick <- na.omit(period.tick)
  period.tick.label <- 2^(period.tick) / fps
  spec_period_axis <-  list(at = 2^period.tick, labels = period.tick.label)

  WaveletComp::wt.image(
    obj,
    n.levels = 250,
    spec.time.axis = spec_time_axis,
    spec.period.axis = spec_period_axis,
    legend.params = list(lab = "Wavelet Power Levels", mar = 4.7),
    periodlab = "Period / sec",
    timelab = spec_time_axis$time_lab,
    main = paste("Power Spectrum for", view$recording$stem, "-", obj$subtitle),
    ...
  )

}


#' Plot a coherency of a wavelet object
#'
#' @param obj analyze.coherency object
#' @param view View object
#' @param ... passed to [WaveletComp::wc.image()]
#'
#' @return
#' @export
#'
#' @examples
#' r <- get_sample_recording()
#' rv <- get_raw_view(r, "Central", "", "Sitar")
#' pv <- get_processed_view(rv)
#' pv1 <- subset(pv, Time >= 10)
#' co <- analyze_coherency(pv1, c("Nose_x", "Nose_y"))
#' plot_cross_spectrum(co, pv1)
#' plot_coherence(co, pv1)
plot_cross_spectrum <- function(obj, view,  ...) {
  stopifnot("analyze.coherency" %in% class(obj), "View" %in% class(view))

  arg_list <- list(...)
  df <- view$df
  fps <- view$recording$fps
  spec_time_axis <- make_time_axis(df, fps)

  axis.2 <- obj$axis.2
  period.tick <- unique(trunc(axis.2))
  period.tick[period.tick < log2(obj$Period[1])] <- NA
  period.tick <- na.omit(period.tick)
  period.tick.label <- 2^(period.tick) / fps
  spec_period_axis <-  list(at = 2^period.tick, labels = period.tick.label)

  if ("which.image" %in% names(arg_list) && arg_list$which.image == "wc") {
    main_title <-paste("Wavelet Coherence for", view$recording$stem, "-", obj$subtitle)
    legend_label <- "Wavelet Coherence Levels"
  } else {
    main_title <-paste("Cross Wavelet Power Spectrum for", view$recording$stem, "-", obj$subtitle)
    legend_label <- "Cross Wavelet Power Levels"
  }

  WaveletComp::wc.image(obj, n.levels = 250,
    spec.time.axis = spec_time_axis,
    spec.period.axis = spec_period_axis,
    legend.params = list(lab = legend_label, mar = 4.7),
    periodlab = "Period",
    main = main_title,
    timelab = spec_time_axis$time_lab,
    ...
  )
}

#' @rdname plot_cross_spectrum
#' @export
plot_coherence <- function(obj, view, ...) {
  plot_cross_spectrum(obj, view, which.image = "wc")
}

#' Plot average power of a wavelet object
#'
#' @param obj analyze.wavelet object
#' @param view View object
#' @param ... passed to [WaveletComp::wt.avg()]
#'
#' @return
#' @export
#'
#' @examples
#' r <- get_sample_recording()
#' rv <- get_raw_view(r, "Central", "", "Sitar")
#' pv <- get_processed_view(rv)
#' pv1 <- subset(pv, Time >= 10)
#' w <- analyze_wavelet(pv1, "Nose_x")
#' plot_average_power(w, pv1)
#' w <- analyze_wavelet(pv1, "Nose_y")
#' plot_average_power(w, pv1)
plot_average_power <- function(obj, view, ...) {
  stopifnot("analyze.wavelet" %in% class(obj), "View" %in% class(view))

  df <- view$df
  fps <- view$recording$fps

  axis.2 <- obj$axis.2
  period.tick <- unique(trunc(axis.2))
  period.tick[period.tick < log2(obj$Period[1])] <- NA
  period.tick <- na.omit(period.tick)
  period.tick.label <- 2^(period.tick) / fps
  spec_period_axis <-  list(at = 2^period.tick, labels = period.tick.label)

  WaveletComp::wt.avg(
    obj,
    spec.period.axis = spec_period_axis,
    main = paste("Average Wavelet Power for", view$recording$stem, "-", obj$subtitle),
    averagelab = "Average Wavelet Power",
    periodlab = "Period / sec",
    ...
  )
}


#' Plot average coherency of a coherency object
#'
#' @param obj analyze.coherency object
#' @param view View object
#' @param ... passed to [WaveletComp::wc.avg()]
#'
#' @return
#' @export
#'
#' @examples
#' r <- get_sample_recording()
#' rv <- get_raw_view(r, "Central", "", "Sitar")
#' pv <- get_processed_view(rv)
#' co <- analyze_coherency(pv, columns = c("Nose_x", "Nose_y"))
#' plot_average_coherency(co, pv)
plot_average_coherency <- function(obj, view, ...) {
  stopifnot("analyze.coherency" %in% class(obj), "View" %in% class(view))

  df <- view$df
  fps <- view$recording$fps

  axis.2 <- obj$axis.2
  period.tick <- unique(trunc(axis.2))
  period.tick[period.tick < log2(obj$Period[1])] <- NA
  period.tick <- na.omit(period.tick)
  period.tick.label <- 2^(period.tick) / fps
  spec_period_axis <-  list(at = 2^period.tick, labels = period.tick.label)

  WaveletComp::wc.avg(
    obj,
    spec.period.axis = spec_period_axis,
    main = paste("Average Coherency for", view$recording$stem, "-", obj$subtitle),
    averagelab = "Average Coherency",
    periodlab = "Period / sec",
    ...
  )

}


#' Comparison plot of phases of a coherency object
#'
#' @param obj coherency object
#' @param view View object
#' @param sel.period
#' @param sel.upper
#' @param sel.lower
#' @param ... passed to [WaveletComp::wc.sel.phases()]
#'
#' @return
#' @export
#'
#' @examples
#' r <- get_sample_recording()
#' rv <- get_raw_view(r, "Central", "", "Sitar")
#' pv <- get_processed_view(rv)
#' co <- analyze_coherency(pv, columns = c("Nose_x", "Nose_y"))
#' plot_cross_spectrum(co, pv)
#' plot_sel_phases(co, pv, sel.period = 0.64)
#' plot_sel_phases(co, pv, sel.period = NULL, sel.lower = 0.6, sel.upper = 0.8)
plot_sel_phases <- function(obj, view, sel.period, sel.upper = NULL,
                            sel.lower = NULL, ...) {
  stopifnot("analyze.coherency" %in% class(obj), "View" %in% class(view))

  df <- view$df
  fps <- view$recording$fps
  spec_time_axis <- make_time_axis(df, fps)

  sub_title <- if (is.null(sel.period))
    paste("Selected Period Interval:", sel.lower, "to", sel.upper) else ""

  sel_phases <- WaveletComp::wc.sel.phases(
    obj,
    sel.period = sel.period * fps,
    sel.upper = sel.upper * fps,
    sel.lower = sel.lower * fps,
    spec.time.axis = spec_time_axis,
    main = paste("Phase comparison for", view$recording$stem, "-", obj$subtitle),
    phaselab = "Phase",
    timelab = spec_time_axis$time_lab,
    sub = sub_title,
    ...
  )
  sel_phases$subtitle <- paste(view$recording$stem, "-", obj$subtitle)

  sel_phases
}


#' Plot a coherency of a wavelet object
#'
#' @param obj analyze.coherency object
#' @param view View object
#' @param ... passed to [WaveletComp::wc.phasediff.image()]
#'
#' @return
#' @export
#'
#' @examples
#' r <- get_sample_recording()
#' rv <- get_raw_view(r, "Central", "", "Sitar")
#' pv <- get_processed_view(rv)
#' pv1 <- subset(pv, Time >= 10 & Time <= 20)
#' co <- analyze_coherency(pv1, c("Nose_x", "Nose_y"))
#' plot_phase_difference(co, pv1)
plot_phase_difference <- function(obj, view,  ...) {
  stopifnot("analyze.coherency" %in% class(obj), "View" %in% class(view))

  arg_list <- list(...)
  df <- view$df
  fps <- view$recording$fps
  spec_time_axis <- make_time_axis(df, fps)

  axis.2 <- obj$axis.2
  period.tick <- unique(trunc(axis.2))
  period.tick[period.tick < log2(obj$Period[1])] <- NA
  period.tick <- na.omit(period.tick)
  period.tick.label <- 2^(period.tick) / fps
  spec_period_axis <-  list(at = 2^period.tick, labels = period.tick.label)

  main_title <-paste("Global phase differences for", view$recording$stem, "-", obj$subtitle)
  legend_label <- "Phase difference levels"

  WaveletComp::wc.phasediff.image(
    obj, n.levels = 250,
    spec.time.axis = spec_time_axis,
    spec.period.axis = spec_period_axis,
    legend.params = list(lab = legend_label, mar = 4.7),
    periodlab = "Period",
    main = main_title,
    timelab = spec_time_axis$time_lab,
    ...
  )
}


# helper to generate a time axis in min/sec for wavelet plots
make_time_axis <- function(df, fps, num_tlabels = 10) {
  min_time <- min(df$Time, na.rm = TRUE)
  max_time <- max(df$Time, na.rm = TRUE)

  mf <- max(df$Frame, na.rm = TRUE)
  labels_at <- seq(0, mf, by = mf / num_tlabels)
  labels_sec <- labels_at / fps + min_time

  labels_to <- paste0(
    formatC(labels_sec %/% 60, width=2, flag = 0),
    ":",
    formatC(floor(labels_sec %% 60), width = 2, flag = 0))
  time_lab <- "Time / min:sec"

  list(at = labels_at + 1, labels = labels_to, time_lab = time_lab)
}

