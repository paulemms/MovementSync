# Diagnostic plots of S3 objects


#' Plot a Duration S3 object
#'
#' @param obj
#' @param ... passed to barplot
#'
#' @return
#'
#' @examples
#' r <- get_sample_recording()
#' d <- get_duration_annotation_data(r)
#' plot(d)
#' @exportS3Method
plot.Duration <- function(obj, ...) {
  barplot(Duration ~ In + Tier, main = "Duration Object", data = obj, ...)
}

#' Plot a OnsetsSelected S3 object
#'
#' @param obj
#' @param ... passed to [barplot()]
#' @param instrument
#' @param matra
#'
#' @return
#'
#' @examples
#' r <- get_sample_recording()
#' o <- get_onsets_selected_data(r)
#' plot(o)
#' @exportS3Method
plot.OnsetsSelected <- function(obj, instrument = 'Inst', matra = 'Matra', ...) {

  dfr_list <- obj[sapply(obj, class) == 'data.frame']
  df <- dplyr::bind_rows(dfr_list, .id = 'Tala')
  stopifnot(instrument %in% colnames(df), matra %in% colnames(df))

  df <- dplyr::rename(df, 'Matra' = matra)
  df['is_na_column'] <- !is.na(df[instrument])

  group_df <- dplyr::group_by(df, Matra, Tala)
  output_df <- dplyr::summarise(group_df, Number_of_Onsets = sum(is_na_column))

  barplot(Number_of_Onsets ~ Tala + Matra, beside = T, legend.text = T, data = output_df,
          main = paste("OnsetsSelected Object:", instrument))
}


#' Plot a Metre S3 object
#'
#' @param obj
#'
#' @return
#'
#' @examples
#' r <- get_sample_recording()
#' m <- get_metre_data(r)
#' plot(m)
#' @exportS3Method
plot.Metre <- function(obj, ...) {
  zoo_list <- lapply(obj, function(x) zoo::zoo(diff(x$Time), order.by = x$Time))
  z <- do.call(merge, zoo_list)
  if (is.null(ncol(z))) {
    plot(z, yax.flip = TRUE, xlab = "Time / s", ylab = "", main = "Metre Object - Time Between Cycles", ...)
  } else {
    plot(z, yax.flip = TRUE, xlab = "Time / s", main = "Metre Object - Time Between Cycles", ...)

  }
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
#' r <- get_sample_recording()
#' v <- get_raw_view(r, "Central", "", "Sitar")
#' plot(v, columns = "LEar_x")
#' @exportS3Method
plot.View <- function(obj, columns=NULL, maxpts = 1000, ...) {

  max_num_cols <- 9

  # Restrict points and columns to plot
  columns <- if (is.null(columns)) {
    if (ncol(obj$df) > max_num_cols + 2)
      warning(paste("Only plotting first", max_num_cols, "data columns"))
    seq_len(min(ncol(obj$df), max_num_cols + 2))[-1]
  } else c("Time", columns)
  sp <- if (nrow(obj$df) > maxpts) sample(nrow(obj$df), maxpts) else seq_len(nrow(obj$df))

  df <- obj$df[sp, columns, drop = FALSE]
  df <- df[, colSums(is.na(df)) < (nrow(df) - 1), drop=FALSE] # more than one point
  zoo_list <- lapply(df[-1], function(x) zoo::zoo(x, order.by = df$Time))
  z <- do.call(merge, zoo_list)

  title <- c(obj$recording$stem, obj$vid, obj$direct, obj$inst)
  title <- paste(title[title != ""], collapse="_")
  if (is.null(ncol(z))) {
    plot(z, xlab = "Time / s", ylab = columns[-1], main = paste(class(obj)[1], "Object for", title), ...)
  } else {
    plot(z, xlab = "Time / s", main = paste(class(obj)[1], "for", title), ...)
  }
}
