# Functions to return a set of time intervals for analysis

#' S3 generic function to splice a timeline
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


#' Generate spliced timeline using a Metre object
#'
#' @param x
#' @param window_duration
#' @param rhythms
#' @param ...
#'
#' @return
#' @exportS3Method
#'
#' @examples
#' r <- get_recording("NIR_DBh_Malhar_2Gats", fps = 25)
#' m <- get_metre_data(r)
#' splicing_df <- splice_time(m, window_duration = 1)
#' head(splicing_df)
splice_time.Metre <- function(x, window_duration, rhythms = NULL, ...) {
  if (is.null(rhythms)) rhythms <- names(x)
  stopifnot(all(rhythms %in% names(x)))

  df_list <- list()
  for (j in seq_along(rhythms)) {
    df <- x[[j]]
    df$Start <- df$Time - window_duration / 2
    df$End <- df$Time + window_duration / 2
    df <- df[-match(c("Time", "Notes"), colnames(df), nomatch = 0)]
    colnames(df)[1] <- "Tier"
    df$Tier <- paste(rhythms[j], 'Cycle', as.character(df$Tier), sep="_")
    df_list[[j]] <- df
  }
  output_df <- dplyr::bind_rows(df_list)
  output_df <- dplyr::arrange(output_df, Start)
  as.data.frame(output_df)
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
  stopifnot(all(sapply(x, length) == 2), all(sapply(x, function(x) x[1] < x[2])))

  df <- t(as.data.frame(x))
  rownames(df) <- NULL
  colnames(df) <- c("Start", "End")
  df <- cbind.data.frame(Tier = names(x), df)
  df <- df[c("Tier", "Start", "End")]

  stopifnot(is_valid_splice(df))
  df
}


#' Generate spliced timeline using a Duration object
#'
#' @param x
#' @param expr
#' @param make.unique
#' @param ... passed to [make.unique()]
#'
#' @return
#' @exportS3Method
#'
#' @examples
#' r <- get_recording("NIR_ABh_Puriya", fps = 25)
#' d <- get_duration_annotation_data(r)
#' splice_time(d)
#' splice_time(d, tier = 'Event', comments = 'tabla solo')
splice_time.Duration <- function(x, expr = NULL, make.unique = TRUE,
                                 tier = NULL, comments = NULL, ...) {
  stopifnot(!is.null(expr) || (!is.null(tier) || !is.null(comments)))

  if (is.null(expr)) {
    expr_list <- list()
    expr_list[[1]] <- if (!is.null(tier)) paste0('Tier %in% ', '"', tier, '"') else NA
    expr_list[[2]] <- if (!is.null(comments)) paste0('Comments %in% ', '"', comments, '"') else NA
    expr_list <- expr_list[!is.na(expr_list)]
    expr <- if (length(expr_list) > 1) paste0(expr_list, collapse = " & ") else expr_list[[1]]
  }
  expr <- rlang::parse_expr(expr)
  df <- dplyr::filter(as.data.frame(x), !!expr)
  df <- df[c("Comments", "In", "Out")]
  if (make.unique) df$Comments <- make.unique(df$Comments, ...)
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
#' @param splicing_df
#' @param na.pad
#'
#' @return SplicedView object
#' @export
#'
#' @examples
#' r <- get_recording("NIR_ABh_Puriya", fps = 25)
#' rv <- get_raw_view(r, "Central", "", "Sitar")
#' l <- list(a = c(0, 10), b = c(10, 20), c = c(20, 30))
#' splicing_df <- splice_time(l)
#' sv <- get_spliced_view(rv, splicing_df)
get_spliced_view <- function(v, splicing_df) {
  stopifnot("View" %in% class(v), class(splicing_df[['Tier']]) == "character")
  df <- v$df
  fps <- v$recording$fps

  df_list <- list()
  for (r in seq_len(nrow(splicing_df))) {
    tier <- splicing_df[r, "Tier"]
    spliced_df <- df[df$Time >= splicing_df[r, "Start"] & df$Time <= splicing_df[r, "End"],, drop = FALSE]
    # Tiers with the same name get appended to df_list[[tier]]
    df_list[[tier]] <- dplyr::bind_rows(df_list[[tier]], spliced_df)

  }
  output_df <- dplyr::bind_rows(df_list, .id = "Tier")
  output_df <- dplyr::arrange(output_df, Frame, Tier)

  # if (na.pad) {
  #   remove_cols <- match(c('Tier', 'Frame', 'Time'), colnames(output_df), nomatch = 0)
  #   l <- split(output_df, output_df[['Tier']])
  #   for (tier in names(l)) {
  #     df <- l[[tier]]
  #     frame_num <- df[['Frame']]
  #     z <- zoo::zoo(df[,-remove_cols,drop=FALSE], order.by = as.integer(frame_num))
  #     new_index <- seq(min(frame_num, na.rm = TRUE), max(frame_num, na.rm = TRUE))
  #     new_z <- merge(z, zoo::zoo(,new_index))
  #     l[[tier]] <- cbind(Tier = tier, Frame = zoo::index(new_z), Time = zoo::index(new_z) / fps,
  #                        as.data.frame(new_z))
  #   }
  #   output_df <- dplyr::bind_rows(l)
  # }

  l <- list(df = output_df, splicing_df = splicing_df, vid = v$vid,
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
#' sv_list <- split(sv)
split.SplicedView <- function(obj) {
  df_list <- split(obj$df, obj$df[['Tier']])
  v_list <- lapply(df_list, function(x) {
    df <- x[, colnames(x) != "Tier", drop = FALSE]
    l <- list(df = df, vid = obj$vid, direct = obj$direct,
         inst = obj$inst, recording = obj$recording)
    class(l) <- class(obj)[-1]
    l
  })

  v_list
}


#' Sample from a list of Views
#'
#' @param ...
#' @param num_samples
#' @param replace
#'
#' @return
#' @export
#'
#' @examples
#' r1 <- get_recording("NIR_ABh_Puriya", fps = 25)
#' fv1_list <- get_filtered_views(r1, data_points = "Nose", n = 41, p = 3)
#' jv1 <- get_joined_view(fv1_list)
#' l <- list(a=c(1, 2), b = c(2, 3))
#' splicing_df <- splice_time(l)
#' sv <- get_spliced_view(jv1, splicing_df = splicing_df, na.pad = FALSE)
#' autoplot(sv)
#' sv_new <- sample_time_spliced_views(sv, num_samples = 10, replace = FALSE)
#' autoplot(sv_new)
#' sv_new <- sample_time_spliced_views(sv, num_samples = 10, replace = TRUE)
#' autoplot(sv_new)
#' l <- list(a=c(1, 2), a = c(10, 20), b = c(30, 40))
#' splicing_df <- splice_time(l)
#' sv <- get_spliced_view(jv1, splicing_df = splicing_df)
#' sv_new <- sample_time_spliced_views(sv, num_samples = 20, replace = TRUE)
#' autoplot(sv_new)
sample_time_spliced_views <- function(..., num_samples, replace = FALSE, na.action = na.pass) {
  input_sv <- list(...)
  stopifnot(all(sapply(input_sv, function(x) "SplicedView" %in% class(x))))
  stopifnot(num_samples > 0)

  sv_list <- list()
  i <- 1
  for (sv in input_sv) {
    dfr <- sv$df
    keys <- match(c('Tier', 'Frame', 'Time'), colnames(dfr), nomatch = 0)
    data_columns <- colnames(dfr)[-keys]

    # invert the CDF to pick equally between intervals?
    if (replace) {
      # stack the time intervals
      duration_dfr <- dplyr::mutate(
        sv$splicing_df,
        Duration = End - Start,
        Cumulative_Duration = cumsum(Duration),
        Start_Duration = dplyr::lag(Cumulative_Duration, 1, default = 0)
      )
      cumduration_times <- duration_dfr[, 'Cumulative_Duration']
      # Sample from [0, total length of intervals]
      new_cumdurations <- runif(num_samples, 0, cumduration_times[length(cumduration_times)])
      # Find the interval each new time belongs to
      interval_idx <- findInterval(new_cumdurations, c(0, cumduration_times))
      # Generate a list of linear approximation functions for each data column
      new_times <- duration_dfr[interval_idx, 'Start'] + new_cumdurations -
        duration_dfr[interval_idx, 'Start_Duration']

      new_data <- sapply(
        data_columns,
        function(col) approx(x = dfr[['Time']], y = dfr[[col]], xout = new_times, ties = 'ordered')$y
        )

      # Build new sampled data.frame
      new_dfr <- cbind.data.frame(Tier = duration_dfr[interval_idx, 'Tier'], Frame = NA, Time = new_times, new_data)

    } else {
      dfr <- na.action(sv$df)
      row_nums <- sample(seq_len(nrow(dfr)), size = num_samples, replace = replace)
      new_dfr <- dfr[row_nums,,drop = FALSE]
    }

    new_dfr <- new_dfr[order(new_dfr[['Time']]),,drop=FALSE]
    rownames(new_dfr) <- NULL
    sv_list[[i]] <- sv
    sv_list[[i]]$df <- new_dfr
    i <- i + 1
  }
  names(sv_list) <- names(input_sv)

  if (length(sv_list) == 1) sv_list <- sv_list[[1]]
  sv_list
}


#' Checks for valid splice
#'
#' Invalid splices contain overlapping Tiers.
#'
#' @param dfr
#'
#' @return logical
#' @export
#'
#' @examples
#' l1 <- list(a=c(1, 2))
#' is_valid_splice(splice_time(l1))
#' l2 <- list(a=c(1, 15), a = c(10, 20), b = c(30, 40))
#' is_valid_splice(splice_time(l2))
#' l3 <- list(a=c(1, 2), a = c(10, 20), b = c(3, 5))
#' is_valid_splice(splice_time(l2))
is_valid_splice <- function(dfr) {

  duration_dfr <- dplyr::mutate(
    dfr[order(dfr$Start),,drop=FALSE],
    Next_Start = dplyr::lead(Start, 1),
    Valid = End <= Next_Start
  )

  all(duration_dfr[['Valid']], na.rm = TRUE)

}
