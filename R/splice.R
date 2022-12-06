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


#' Generate spliced timeline using an OnsetsDifference object
#'
#' @param x
#' @param window_duration
#' @param ...
#' @param talas
#'
#' @return
#' @exportS3Method
#'
#' @examples
#' r <- get_recording("NIR_DBh_Malhar_2Gats", fps = 25)
#' o1 <- get_onsets_selected_data(r1)
#' po1 <- difference_onsets(o1, instruments = c('Inst', 'Tabla'))
#' splicing_df <- splice_time(po1, window_duration = 1)
#' splicing_df
splice_time.OnsetsDifference <- function(x, window_duration, talas = NULL, make.unique = TRUE, ...) {
  stopifnot(all(talas %in% unique(x$Tala)))
  if (!is.null(talas)) {
    df <- dplyr::filter(x, Tala %in% unique(x$Tala))
  } else {
    df <- x
  }

  # Generate Splicing table
  df$Start <- df$Ref_Beat_Time - window_duration / 2
  df$End <- df$Ref_Beat_Time + window_duration / 2
  df$Segment <- paste('Reference_Beat', as.character(df$Segment), sep="_")
  df <- df[match(c("Segment", "Start", "End"), colnames(df), nomatch = 0)]
  df <- na.omit(df)

  if (make.unique) df$Segment <- make.unique(df$Segment, ...)
  output_df <- dplyr::arrange(df, Start)
  dplyr::as_tibble(output_df)
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
#' splicing_df
splice_time.Metre <- function(x, window_duration, rhythms = NULL, ...) {
  if (is.null(rhythms)) rhythms <- names(x)
  stopifnot(all(rhythms %in% names(x)))

  df_list <- list()
  for (j in seq_along(rhythms)) {
    df <- x[[j]]
    df$Start <- df$Time - window_duration / 2
    df$End <- df$Time + window_duration / 2
    df <- df[-match(c("Time", "Notes"), colnames(df), nomatch = 0)]
    colnames(df)[1] <- "Segment"
    df$Segment <- paste(rhythms[j], 'Cycle', as.character(df$Segment), sep="_")
    df_list[[j]] <- df
  }
  output_df <- dplyr::bind_rows(df_list)
  output_df <- dplyr::arrange(output_df, Start)
  dplyr::as_tibble(output_df)
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
  stopifnot(length(x) > 0, all(sapply(x, length) == 2), all(sapply(x, function(x) x[1] < x[2])))

  df <- t(as.data.frame(x))
  rownames(df) <- NULL
  colnames(df) <- c("Start", "End")
  df <- cbind.data.frame(Segment = names(x), df)
  df <- df[c("Segment", "Start", "End")]

  stopifnot(!is_splice_overlapping(df))
  dplyr::as_tibble(df)
}


#' Generate spliced timeline using a Duration object
#'
#' @param x
#' @param expr
#' @param make.unique
#' @param ... passed to [make.unique()]
#' @param tier
#' @param comments
#'
#' @return
#' @exportS3Method
#'
#' @examples
#' r <- get_recording("NIR_ABh_Puriya", fps = 25)
#' d <- get_duration_annotation_data(r)
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
  colnames(df) <- c("Segment", "Start", "End")

  dplyr::as_tibble(df)
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
#' df
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

  dplyr::tibble(Segment = paste0("w", seq_along(offset)), Start = offset, End = offset + win_size)
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
  stopifnot("View" %in% class(v), class(splicing_df[['Segment']]) == "character")
  df <- v$df
  fps <- v$recording$fps

  df_list <- list()
  for (r in seq_len(nrow(splicing_df))) {
    segment <- splicing_df$Segment[r]
    spliced_df <- df[df$Time >= splicing_df$Start[r] & df$Time <= splicing_df$End[r],, drop = FALSE]
    # Segments with the same name get appended to df_list[[segment]]
    df_list[[segment]] <- dplyr::bind_rows(df_list[[segment]], spliced_df)

  }
  output_df <- dplyr::bind_rows(df_list, .id = "Segment")
  output_df <- dplyr::arrange(output_df, Frame, Segment)

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
  df_list <- split(obj$df, obj$df[['Segment']])
  v_list <- lapply(df_list, function(x) {
    df <- x[, colnames(x) != "Segment", drop = FALSE]
    l <- list(df = df, vid = obj$vid, direct = obj$direct,
         inst = obj$inst, recording = obj$recording)
    class(l) <- class(obj)[-1]
    l
  })

  v_list
}


#' Sample the time line from a list of Views
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
#' sv <- get_spliced_view(jv1, splicing_df = splicing_df)
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
    keys <- match(c('Segment', 'Frame', 'Time'), colnames(dfr), nomatch = 0)
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
      new_dfr <- cbind.data.frame(Segment = duration_dfr[interval_idx, 'Segment'], Frame = NA, Time = new_times, new_data)

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


#' Checks if splicing data.frames overlap
#'
#' @param ... Each argument can be a data frame or a list of data frames
#'
#' @return logical
#' @export
#'
#' @examples
#' l1 <- list(a=c(1, 10), a = c(20, 30), b = c(30, 40))
#' dfr1 <- splice_time(l1)
#' l2 <- list(a=c(10, 15), b = c(15, 25))
#' dfr2 <- splice_time(l2)
#' is_splice_overlapping(dfr1, dfr2)
is_splice_overlapping <- function(...) {

  splice_dfr <- dplyr::bind_rows(...)
  splice_dfr <- dplyr::arrange(splice_dfr, Start)
  overlap <- splice_dfr[['End']] > dplyr::lead(splice_dfr[['Start']], 1)

  any(overlap, na.rm = TRUE)
}


#' Clip a splice so segments are of fixed duration
#'
#' @param splice_dfr
#' @param duration
#' @param location
#'
#' @return
#' @export
#'
#' @examples
#' l <- list(a = c(10, 20), b = c(30, 40),c = c(50, 55))
#' splice_dfr <- splice_time(l)
#' clip_splice(splice_dfr, duration = 1)
#' clip_splice(splice_dfr, duration = 6)
#' clip_splice(splice_dfr, duration = 1, location = 'beginning')
#' clip_splice(splice_dfr, duration = 10, location = 'beginning')
#' clip_splice(splice_dfr, duration = 1, location = 'end')
#' clip_splice(splice_dfr, duration = 10, location = 'end')
clip_splice <- function(splice_dfr, duration, location = 'middle') {
  stopifnot(is.data.frame(splice_dfr), duration > 0, location %in% c('beginning', 'middle', 'end'))
  new_splice_dfr <- splice_dfr

  if (location == 'middle') {
    mid_time <- (splice_dfr$Start + splice_dfr$End) / 2
    start_time <- mid_time - duration / 2
    end_time <- mid_time + duration / 2
    is_before_start <- start_time < splice_dfr$Start
    is_after_end <- end_time > splice_dfr$End
    if (any(is_before_start | is_after_end)) {
      warning('Segments too short to clip to duration - using start or end point of segment')
    }
    new_splice_dfr$Start <- ifelse(is_before_start, splice_dfr$Start, start_time)
    new_splice_dfr$End <- ifelse(is_after_end, splice_dfr$End, end_time)

  } else if (location == 'beginning') {
    end_time <- splice_dfr$Start + duration
    is_after_end <- end_time > splice_dfr$End
    if (any(is_after_end)) {
      warning('Segments too short to clip to duration - using end point of segment')
    }
    new_splice_dfr$Start <- splice_dfr$Start
    new_splice_dfr$End <- ifelse(is_after_end, splice_dfr$End, end_time)

  } else if (location == 'end') {
    start_time <- splice_dfr$End - duration
    is_before_start <- start_time < splice_dfr$Start
    if (any(is_before_start)) {
      warning('Segments too short to clip to duration - using start point of segment')
    }
    new_splice_dfr$Start <- ifelse(is_before_start, splice_dfr$Start, start_time)
    new_splice_dfr$End <- splice_dfr$End
  } else stop()

  dplyr::as_tibble(new_splice_dfr)
}


#' Merge splices together using set operations
#'
#' @param ...
#' @param operation
#'
#' @return
#' @export
#'
#' @examples
#' l1 <- list(a1 = c(100, 200), a2 = c(250, 300), a3 = c(400, 550), a4 = c(600, 650))
#' split1_dfr <- splice_time(l1)
#' split1_dfr
#'
#' l2 <- list(b1 = c(150, 275), b2 = c(610, 640))
#' split2_dfr <- splice_time(l2)
#' split2_dfr
#'
#' l3 <- list(c1 = c(275, 325), c2 = c(600, 675), c3 = c(700, 725))
#' split3_dfr <- splice_time(l3)
#' split3_dfr
#'
#' merge_splice(x = split1_dfr, y = split2_dfr, z = split3_dfr, operation = 'union')
#' merge_splice(x = split1_dfr, y = split2_dfr, z = split3_dfr, operation = 'intersection')
merge_splice <- function(..., operation) {
  l <- list(...)

  # Checks
  stopifnot(length(l) > 1, operation %in% c('union', 'intersection'))
  stopifnot(all(sapply(l, Negate(is_splice_overlapping))))

  overlap <- switch(operation, 'union' = 1, 'intersection' = length(l))
  segment_name <- switch(operation, 'union' = paste(names(l), collapse = " | "),
                         'intersection' = paste(names(l), collapse = " & "))

  dfr <- dplyr::bind_rows(l, .id = 'Splice')
  dd <- rbind(data.frame(pos = dfr$Start, event = 1),
              data.frame(pos = dfr$End, event = -1))

  dd <- aggregate(event ~ pos, dd, sum)
  dd <- dd[order(dd$pos), , drop=FALSE]
  dd$open <- cumsum(dd$event)
  r <- rle(dd$open >= overlap)
  ex <- cumsum(r$lengths - 1 + rep(1, length(r$lengths)))
  sx <- ex - r$lengths + 1

  output_dfr <- cbind.data.frame(
    Segment = make.unique(rep(segment_name, length(sx[r$values]))),
    Start = dd$pos[sx[r$values]],
    End = dd$pos[ex[r$values] + 1]
  )

  dplyr::as_tibble(output_dfr)
}


