#' Plot a Metre S3 object
#'
#' @param obj
#'
#' @return
#' @exportS3Method
#'
#' @examples
#' r <- get_recording("NIR_ABh_Puriya", fps = 25)
#' m <- get_metre_data(r)
#' plot(m)
plot.Metre <- function(obj) {
  zoo_list <- lapply(obj, function(x) zoo::zoo(x$Cycle, order.by = x$Time))
  z <- do.call(merge, zoo_list)
  plot(z, yax.flip = TRUE, xlab = "Time / s", main = "Metre Object")
}


#' Plot a RawView S3 object
#'
#' @param o
#'
#' @return
#' @exportS3Method
#'
#' @examples
#' r <- get_recording("NIR_ABh_Puriya", fps = 25)
#' v <- get_raw_view(r, "Central", "", "Sitar")
#' plot(v)
plot.RawView <- function(o, cols=NULL, ...) {
  # Restrict points to plot
  cols <- if (is.null(cols)) seq_len(min(ncol(o$df), 10))[-1] else c("Time", cols)
  sp <- if (nrow(o$df) > 100) sample(nrow(o$df), 100) else seq_len(nrow(o$df))
  df <- o$df[sp, cols, drop = FALSE]
  zoo_list <- lapply(df[-1], function(x) zoo::zoo(x, order.by = df$Time))
  z <- do.call(merge, zoo_list)

  title <- c(o$recording$stem, o$vid, o$direct, o$inst)
  title <- paste(title[title != ""], collapse="_")
  if (is.null(ncol(z))) {
    plot(z, xlab = "Time / s", ylab = cols[-1], main = paste0("Raw View for ", title), ...)
  } else {
    plot(z, yax.flip = TRUE, xlab = "Time / s", main = paste0("Raw View for ", title), ...)
  }
}


#' Plot a ProcessedView S3 object
#'
#' @param o
#'
#' @return
#' @exportS3Method
#'
#' @examples
#' r <- get_recording("NIR_ABh_Puriya", fps = 25)
#' rv <- get_raw_view(r, "Central", "", "Sitar")
#' pv <- get_processed_view(pv)
#' plot(rv)
#' plot(pv)
plot.ProcessedView <- function(o, cols=NULL) {
  # Restrict points to plot
  cols <- if (is.null(cols)) seq_len(min(ncol(o$df_norm), 10))[-1] else c("Time", cols)
  sp <- if (nrow(o$df_norm) > 100) sample(nrow(o$df_norm), 100) else seq_len(nrow(o$df_norm))
  df <- o$df_norm[sp, cols, drop = FALSE]
  zoo_list <- lapply(df[-1], function(x) zoo::zoo(x, order.by = df$Time))
  z <- do.call(merge, zoo_list)

  title <- c(o$recording$stem, o$vid, o$direct, o$inst)
  title <- paste(title[title != ""], collapse="_")
  if (is.null(ncol(z))) {
    plot(z, xlab = "Time / s", ylab = cols[-1], main = paste0("Processed View for ", title))
  } else {
    plot(z, yax.flip = TRUE, xlab = "Time / s", main = paste0("Processed View for ", title))
  }
}


#' Plot a FilteredView S3 object
#'
#' @param o
#'
#' @return
#' @exportS3Method
#'
#' @examples
#' r <- get_recording("NIR_ABh_Puriya", fps = 25)
#' rv <- get_raw_view(r, "Central", "", "Sitar")
#' pv <- get_processed_view(pv)
#' fv <- apply_filter(pv, c("Nose", "RWrist", "LWrist"), window_size=19, poly_order=4)
#' plot(fv)
plot.FilteredView <- function(o, cols=NULL) {
  # Restrict points to plot
  cols <- if (is.null(cols)) seq_len(min(ncol(o$df_filt), 10)) else c("Time", cols)
  sp <- if (nrow(o$df_filt) > 100) sample(nrow(o$df_filt), 100) else seq_len(nrow(o$df_filt))
  df <- o$df_filt[sp, cols, drop = FALSE]

  zoo_list <- lapply(df[-(1:2)], function(x) zoo::zoo(x, order.by = df$Time))
  z <- do.call(merge, zoo_list)

  title <- c(o$view$recording$stem, o$view$vid, o$view$direct, o$view$inst)
  title <- paste(title[title != ""], collapse="_")
  if (is.null(ncol(z))) {
    plot(z, xlab = "Time / s", ylab = cols[-1], main = paste0("Filtered View for ", title))
  } else {
    plot(z, yax.flip = TRUE, xlab = "Time / s", main = paste0("Filtered View for ", title))
  }
}

