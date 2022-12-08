#' Summarise Recording object
#'
#' @param obj
#'
#' @return
#' @exportS3Method
#'
#' @examples
#' r <- get_sample_recording()
#' summary(r)
summary.Recording <- function(obj) {
  obj
}


#' Summarise Duration object
#'
#' @param obj
#'
#' @return
#' @exportS3Method
#'
#' @examples
#' r <- get_sample_recording()
#' d <- get_duration_annotation_data(r)
#' head(summary(d))
summary.Duration <- function(obj) {
  obj
}


#' Summarise OnsetsSelected object
#'
#' @param obj
#'
#' @return
#' @exportS3Method
#'
#' @examples
#' r <- get_sample_recording()
#' o <- get_onsets_selected_data(r)
#' summary(o)
summary.OnsetsSelected <- function(obj) {
  lapply(obj, function(x) if (is.data.frame(x)) summary(x))
}


#' Summarise Metre object
#'
#' @param obj
#'
#' @return
#' @exportS3Method
#'
#' @examples
#' r <- get_sample_recording()
#' m <- get_metre_data(r)
#' summary(m)
summary.Metre <- function(obj) {
  lapply(obj, function(x) if (is.data.frame(x)) summary(x))
}


#' Summarise a View object
#'
#' @param obj
#'
#' @return
#' @exportS3Method
#'
#' @examples
#' r <- get_sample_recording()
#' rv <- get_raw_view(r, "Central", "", "Sitar")
#' pv <- get_processed_view(rv)
#' fv <- apply_filter_sgolay(pv, c("Nose", "RWrist", "LWrist"), n=19, p=4)
#' summary(rv)
#' summary(pv)
#' summary(fv)
summary.View <- function(obj) {
  summary(obj$df)
}


#' Summarise an analyze.wavelet object
#'
#' @param obj
#'
#' @return
#' @exportS3Method
#'
#' @examples
#' r <- get_sample_recording()
#' rv <- get_raw_view(r, "Central", "", "Sitar")
#' pv <- get_processed_view(rv)
#' w <- analyze_wavelet(pv, "Nose_x")
#' summary(w, pv)
summary.analyze.wavelet <- function(obj, v) {
  d <- data.frame(Period = obj$Period / v$recording$fps,
             Average_Power = obj$Power.avg)
  summary(d)
}


#' Summarises a sel.phases object
#'
#' @param obj
#' @param na.rm
#'
#' @return
#' @exportS3Method
#'
#' @examples
#' r <- get_sample_recording()
#' rv <- get_raw_view(r, "Central", "", "Sitar")
#' pv <- get_processed_view(rv)
#' co <- analyze_coherency(pv, columns = c("Nose_x", "Nose_y"))
#' sp <- plot_sel_phases(co, pv, sel.period = NULL, sel.lower = 0.5, sel.upper = 0.7)
#' summary(sp)
summary.sel.phases <- function(obj, na.rm = TRUE) {

  angle_circular <- circular::circular(obj$Angle)

  list(Mean_Phase_Angle = circular::mean.circular(angle_circular, na.rm = na.rm),
       SD_Phase_Angle = circular::sd.circular(angle_circular, na.rm = na.rm),
       Mean_Resultant_Length = circular::rho.circular(angle_circular, na.rm = na.rm))
}


#' Plot windowed resultant length
#'
#' @param window_duration
#' @param smooth
#' @param na.rm
#' @param obj
#'
#' @return
#' @export
#'
#' @examples
#' r <- get_sample_recording()
#' rv <- get_raw_view(r, "Central", "", "Sitar")
#' pv <- get_processed_view(rv)
#' co <- analyze_coherency(pv, columns = c("Nose_x", "Nose_y"))
#' sp <- plot_sel_phases(co, pv, sel.period = 0.64)
#' plot_roll_resultant_length(sp)
plot_roll_resultant_length <- function(obj, window_duration = 5, smooth = FALSE,
                                       by = 1, na.rm = TRUE) {

  angle <- if (smooth) obj$sAngle else obj$Angle
  period <- obj$Period
  tm <- obj$axis.1
  max_tm <- max(tm, na.rm = TRUE)
  min_tm <- min(tm, na.rm = TRUE)

  # Window data to time_window
  # group_length <- round(window_duration / (max_tm - min(tm))  * (length(tm) - 1))
  # grp <- split(seq_along(tm), ceiling(seq_along(tm) / group_length))
  # mrl <- sapply(grp, function(g)
  #   circular::rho.circular(circular::circular(angle[g]), na.rm = na.rm))
  # gtm <- sapply(grp, function(g) mean(tm[g] - 1, na.rm = na.rm))
  # dfr <- data.frame(Time = gtm, Windowed_Mean_Resultant_Length = mrl)
  # ggplot2::ggplot(dfr) +
  #   ggplot2::geom_line(ggplot2::aes(x = Time, y = Windowed_Mean_Resultant_Length)) +
  #   ggplot2::labs(title = "Relative Phase", subtitle = obj$subtitle) +
  #   ggplot2::xlab("Time / min:sec") + ggplot2::ylab("Windowed Mean Resultant Length")  +
  #   ggplot2::scale_x_time(labels = function(l) strftime(l, '%M:%S'))

  # Convert to zoo and rollapply rho.circular
  window_length <- round(window_duration / (max_tm - min(tm))  * (length(tm) - 1))
  z <- zoo::zoo(angle, order.by = tm - 1)
  rollz <- zoo::rollapply(z, width = window_length,
                 FUN = function(x) circular::rho.circular(circular::circular(x), na.rm = na.rm),
                 by = by, align = 'center')

  autoplot(rollz) +
    ggplot2::labs(title = "Relative Phase Analysis", subtitle = obj$subtitle) +
    ggplot2::xlab("Time / min:sec") + ggplot2::ylab("Rolling Mean Resultant Length")  +
    ggplot2::scale_x_time(labels = function(l) strftime(l, '%M:%S'))

}
