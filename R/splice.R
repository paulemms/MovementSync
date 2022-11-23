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
  df <- dplyr::filter(as.data.frame(x), !!expr)
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
    tier <- splicing_df[r, "Tier"]
    spliced_df <- df[df$Time >= splicing_df[r, "Start"] & df$Time <= splicing_df[r, "End"],, drop = FALSE]
    df_list[[tier]] <- dplyr::bind_rows(df_list[[tier]], spliced_df)

  }
  output_df <- dplyr::bind_rows(df_list, .id = "Tier")
  output_df <- dplyr::arrange(output_df, Frame, Tier)

  l <- list(df_list = output_df, vid = v$vid,
            direct = v$direct, inst = v$inst, recording = v$recording)
  class(l) <- c("SplicedView", class(v))

  invisible(l)
}


#' Get a list of Views from a SplicedView
#'
#' @param obj SplicedView object
#'
#' @return list of Views
#' @exportS3Method
#'
#' @examples
#' r <- get_recording("NIR_ABh_Puriya", fps = 25)
#' rv <- get_raw_view(r, "Central", "", "Sitar")
#' pv <- get_processed_view(rv)
#' l <- list(a = c(0, 300), b = c(300, 600), c = c(600, 900))
#' splicing_df <- splice_time(l)
#' sv <- get_spliced_view(pv, splicing_df)
#' v_list <- split(sv)
split.SplicedView <- function(obj) {
  df_list <- split(obj$df_list, obj$df_list$Tier)
  v_list <- lapply(df_list, function(x) {
    df <- x[, colnames(x) != "Tier", drop = FALSE]
    l <- list(df = df, vid = obj$vid, direct = obj$direct,
         inst = obj$inst, recording = obj$recording)
    class(l) <- class(obj)[-1]
    l
  })

  v_list
}
