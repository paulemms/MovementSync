# Functions to return a set of time intervals for analysis

#' Splice
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
splice_time <- function(x, ...) {
  UseMethod("splice_time", x)
}

#' Generate spliced timeline using a list
#'
#' @param x
#' @param ...
#'
#' @return
#' @exportS3Method
#'
#' @examples
#' l <- list(a = c(0, 10), b = c(10, 20), c = c(20, 30))
#' splice_time(l)
splice_time.list <- function(x, ...) {
  df <- t(as.data.frame(x))
  rownames(df) <- NULL
  colnames(df) <- c("Start", "End")
  df <- cbind.data.frame(Tier = names(x), df)
  df[c("Tier", "Start", "End")]
}


#' Generate spliced timeline using a Duration object
#'
#' @param x
#' @param ...
#'
#' @return
#' @exportS3Method
#'
#' @examples
#' r <- get_recording("NIR_DBh_Malhar_2Gats", fps = 25)
#' d <- get_duration_annotation_data(r)
#' splice_time(d)
splice_time.Duration <- function(x, expr = 'Tier == "FORM"', ...) {
  expr <- rlang::parse_expr(expr)
  df <- dplyr::filter(x, !!expr)
  df <- df[c("Comments", "In", "Out")]
  colnames(df) <- c("Tier", "Start", "End")
  df
}


#' Generate spliced timeline using a view
#'
#' @param x
#' @param ...
#'
#' @return
#' @exportS3Method
#'
#' @examples
#' r <- get_recording("NIR_ABh_Puriya", fps = 25)
#' rv <- get_raw_view(r, "Central", "", "Sitar")
#' df <- splice_time(rv, win_size = 3, step_size = 0.5)
#' head(df)
#' tail(df)
splice_time.View <- function(x, win_size, step_size, ...) {
  stopifnot(win_size > 0, step_size > 0)

  tm <- x$df$Time
  min_time <- min(tm, na.rm = TRUE)
  max_time <- max(tm, na.rm = TRUE)

  if (max_time > win_size) {
    offset <- seq(min_time, max_time - win_size, by = step_size)
  } else {
    stop("Time series length too small for window")
  }

  data.frame(Tier = paste0("w", seq_along(offset)), Start = offset, End = offset + win_size)
}


#' Get spliced view from view object
#'
#' @param v View object
#' @param save_output
#' @param folder_out
#'
#' @return ProcessedView object
#' @export
#'
#' @examples
#' r <- get_recording("NIR_ABh_Puriya", fps = 25)
#' rv <- get_raw_view(r, "Central", "", "Sitar")
#' l <- list(a = c(0, 10), b = c(10, 20), c = c(20, 30))
#' splicing_df <- splice_time(l)
#' sv <- get_spliced_view(rv, splicing_df)
get_spliced_view <- function(v, splicing_df) {
  stopifnot("View" %in% class(v))
  df <- v$df

  df_list <- list()
  for (r in seq_len(nrow(splicing_df))) {
    df_list[[splicing_df[r, "Tier"]]] <-
      df[df$Time >= splicing_df[r, "Start"] & df$Time <= splicing_df[r, "End"],, drop = FALSE]
  }

  l <- list(df_list = dplyr::bind_rows(df_list, .id = "Tier"), vid = v$vid,
            direct = v$direct, inst = v$inst, recording = v$recording)
  class(l) <- c("SplicedView", class(v))

  invisible(l)
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

  # Restrict points and columns to plot
  columns <- if (is.null(columns)) seq_len(min(ncol(obj$df), 9)) else c("Tier", "Frame", "Time", columns)
  sp <- if (nrow(obj$df) > maxpts) sample(nrow(obj$df), maxpts) else seq_len(nrow(obj$df))

  df <- obj$df[sp, columns, drop = FALSE]
  long_df <- tidyr::pivot_longer(df, cols = columns[-(1:3)],
                                 names_to = "Series", values_to = "Value")

  #browser()
  subtitle <- c(obj$recording$stem, obj$vid, obj$direct, obj$inst)
  subtitle <- paste(subtitle[subtitle != ""], collapse="_")

  ggplot2::ggplot(long_df, ggplot2::aes(x = Time, y = Value, col = Series)) +
    ggplot2::geom_point() + ggplot2::geom_line() +
    ggplot2::labs(title = class(obj)[1], subtitle = subtitle) +
    ggplot2::xlab("Time / min:sec") +
    ggplot2::scale_x_time(labels = function(l) strftime(l, '%M:%S')) +
    ggplot2::facet_grid( ~ Tier, scales = "free_x")
}

