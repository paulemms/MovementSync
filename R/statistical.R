# Statistical and analysis functions


#' Apply summary function to the columns in each segment of a SpliceView object
#'
#' Apply summary function to each data point column in a SplicedView and return list of output data.
#' @param sv
#' @param FUN
#' @param simplify
#' @param USE.NAMES
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
apply_column_spliceview <- function(sv, FUN, simplify = FALSE, USE.NAMES = FALSE, ...) {
  v_list <- split(sv)
  sapply(v_list, function(x) {
    keys <- match(c('Segment', 'Frame', 'Time'), colnames(x$df), nomatch = 0)
    dfr <- x$df[-keys]
    apply(dfr, 2, function(y) FUN(y, ...))
  }, simplify = simplify, USE.NAMES = USE.NAMES)
}


#' Apply summary function to the columns in each segment of a SpliceView object
#'
#' Apply summary function to each data point column in a SplicedView and return list of output data.
#' Simplify list to matrix.
#' @param sv
#' @param FUN
#' @param simplify
#' @param USE.NAMES
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
sapply_column_spliceview <- function(sv, FUN, simplify = TRUE, USE.NAMES = TRUE, ...) {
  apply_column_spliceview(sv, FUN, simplify = simplify, USE.NAMES = USE.NAMES, ...)
}


#' Apply complex function to each segment in a SpliceView object
#'
#' @param sv
#' @param FUN
#' @param column
#' @param member
#' @param ...
#'
#' @return list of two elements: 'output' containing results of apply FUN to 'input'
#' @export
#'
#' @examples
apply_segment_spliceview <- function(sv, FUN, ...) {
  view_list <- split(sv)
  output_list <- lapply(view_list, FUN = FUN, ...)
  list(output = output_list, input = view_list)
}

#' @export
#'
#' @examples
pull_segment_spliceview <- function(sv, FUN, element, ...) {
  view_list <- split(sv)
  output_list <- lapply(view_list, FUN = FUN, ...)
  list(output = lapply(output_list, function(x) x[[element]]), input = view_list)
}


#' @export
#'
#' @examples
ave_power_spliceview <- function(sv, ...) {
  wavelet_list <- apply_segment_spliceview(sv, FUN = analyze_wavelet, ...)
  output_mat <- sapply(wavelet_list$output, function(x) x$Power.avg)
  output_mat
}

#' @export
#'
#' @examples
ave_cross_power_spliceview <- function(sv, ...) {
  coherency_list <- apply_segment_spliceview(sv, FUN = analyze_coherency, ...)
  output_mat <- sapply(coherency_list$output, function(x) x$Power.xy.avg)
  output_mat
}


#' @export
#'
#' @examples
sample_ave_power_spliceview <- function(sv, num_samples, replace = TRUE, ...) {
  wavelet_list <- apply_segment_spliceview(sv, FUN = analyze_wavelet, ...)
  output_mat <- sapply(wavelet_list$output, function(x) x$Power.avg)

  period_sample <- sample(nrow(output_mat), num_samples, replace = replace)
  segment_sample <- sample(ncol(output_mat), num_samples, replace = replace)
  sampled_power <- cbind(
    Period = period_sample,
    Power = output_mat[cbind(period_sample, segment_sample)]
  )

  sampled_power
}


#' Randomly create matching segments
#'
#' Add a random offset and use rejection sampling.
#' @param splicing_dfr
#' @param v
#' @param num_samples
#' @param rejection_list
#'
#' @return list of splicing data.frames
#' @export
#'
#' @examples
#' r1 <- get_recording("NIR_ABh_Puriya", fps = 25)
#' d1 <- get_duration_annotation_data(r1)
#' rv1 <- get_raw_view(r1, "Central", "", "Sitar")
#' splicing_df <- splice_time(d1, tier ='INTERACTION', comments = 'Mutual look and smile')
#' x <- sample_splice(splicing_df, rv1, num_samples = 1000)
sample_splice <- function(splicing_dfr, v, num_samples, rejection_list = list()) {
  stopifnot(is.data.frame(splicing_dfr), "View" %in% class(v),
            num_samples > 0, is.list(rejection_list))

  # Discard random splices that appear in the rejection list - includes the original splice
  rejection_splices <- c(list(splicing_dfr), rejection_list)

  # Find max possible offset based on recording length
  max_time <- max(v$df[['Time']], na.rm = TRUE)

  # Total span of segments
  total_span <- max(splicing_dfr[['Start']], na.rm = TRUE) -
    min(splicing_dfr[['Start']], na.rm = TRUE)
  stopifnot(total_span <= max_time)

  splicing_list <- list()
  current_num_samples <- 0

  # Repeat until we get the desired number of samples
  while(current_num_samples < num_samples) {

    # Random start times
    start_times <- runif(num_samples, min = 0, max = max_time - total_span)

    # Generate a list of new sampling data.frames
    new_splicing_list <- lapply(start_times, function(x) {
      splicing_dfr[c('Start', 'End')] <- splicing_dfr[c('Start', 'End')] + x
      splicing_dfr
    })

    # Which ones overlap the original splicing?
    is_overlapped <- rep(FALSE, length(new_splicing_list))
    for (rsp in rejection_splices) {
      is_overlapped <- is_overlapped |
        sapply(new_splicing_list, function(x) is_splice_overlapping(x, rsp))
    }

    # Remove the overlapping ones
    splicing_list <- c(splicing_list, new_splicing_list[!is_overlapped])
    current_num_samples <- length(splicing_list)
    message(current_num_samples)
  }

  splicing_list <- splicing_list[seq_len(num_samples)]
  names(splicing_list) <- paste('Sample splice', seq_along(splicing_list))
  splicing_list
}


#' Get onset differences
#'
#' @param onset_obj
#' @param instruments
#' @param splicing_dfr
#'
#' @return
#' @export
#'
#' @examples
#' r1 <- get_recording("NIR_ABh_Puriya", fps = 25)
#' o1 <- get_onsets_selected_data(r1)
#' head(difference_onsets(o1, instruments = c('Inst', 'Tabla')))
difference_onsets <- function(onset_obj, instruments, splicing_dfr = NULL) {

  dfr_list <- onset_obj[sapply(onset_obj, is.data.frame)]
  dfr <- dplyr::bind_rows(dfr_list, .id = 'Tala')
  dfr <- dfr[,c('Tala', instruments),drop=FALSE]

  # Calculate onset differences for each instrument pair
  instrument_combn <- combn(instruments, 2)
  output_dfr <- data.frame(Tala = dfr['Tala'],
                           Ref_Beat_Time = rowMeans(dfr[instruments], na.rm = TRUE))
  for (j in seq_len(ncol(instrument_combn))) {
    inst1 <- instrument_combn[1, j]
    inst2 <- instrument_combn[2, j]
    col_name <- paste(inst1, inst2, sep = "-")
    output_dfr[col_name] <- dfr[inst1] - dfr[inst2]
  }

  # Splice the time line if required
  if (!is.null(splicing_dfr)) {
    segment_list <- list()
    for (r in seq_len(nrow(splicing_dfr))) {
      a <- splicing_dfr[r, 'Start']
      b <- splicing_dfr[r, 'End']
      segment <-  splicing_dfr[r, 'Segment']
      segment_list[[segment]] <- output_dfr[
        !is.na(output_dfr$Ref_Beat_Time) & output_dfr$Ref_Beat_Time >= a &
          output_dfr$Ref_Beat_Time <= b, ,drop=FALSE]
    }
    output_dfr <- dplyr::bind_rows(segment_list, .id = 'Segment')
  } else {
    output_dfr$Segment <- 'All'
  }

  class(output_dfr) <- c('OnsetsDifference', 'data.frame')
  output_dfr
}


#' Summary of difference in onsets
#'
#' @param onset_obj
#' @param instruments
#' @param splicing_dfr
#'
#' @return
#' @export
#'
#' @examples
#' r1 <- get_recording("NIR_ABh_Puriya", fps = 25)
#' o1 <- get_onsets_selected_data(r1)
#' head(summary_onsets(o1, instruments = c('Inst', 'Tabla')))
summary_onsets <- function(onset_obj, instruments, splicing_dfr = NULL) {

  dfr <- difference_onsets(onset_obj, instruments = instruments, splicing_dfr = splicing_dfr)
  beat_time_pos <- match("Ref_Beat_Time", colnames(dfr), nomatch = 0)
  segment_pos <- match("Segment", colnames(dfr), nomatch = 0)
  tala_pos <- match("Tala", colnames(dfr), nomatch = 0)
  split_list <- split(dfr[-c(segment_pos, beat_time_pos, tala_pos)], list(dfr$Segment, dfr$Tala))

  segment_list <- list()
  for (segment in names(split_list)) {
    dfr <- split_list[[segment]]
    segment_list[[segment]] <- cbind.data.frame(
      Instrument_Pair = colnames(dfr),
      Mean_Difference = apply(dfr, 2, mean, na.rm=TRUE),
      Mean_Absolute_Difference = apply(abs(dfr), 2, mean, na.rm=TRUE),
      SD_Difference = apply(dfr, 2, sd, na.rm=TRUE),
      SD_Absolute_Difference = apply(abs(dfr), 2, sd, na.rm=TRUE)
    )
    rownames(segment_list[[segment]]) <- NULL
  }

  if (length(segment_list) == 1) segment_list <- segment_list[[1]]

  segment_list
}
