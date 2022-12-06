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


#' Compare average power distribution using a splicing table
#'
#' @param jv
#' @param splicing_df
#' @param column
#' @param splice_name
#' @param num_segment_samples
#' @param num_splice_samples
#' @param rejection_list
#' @param show_plot
#' @param sampling_type
#'
#' @export
#'
#' @examples
compare_ave_power1 <- function(jv, splicing_df, splice_name, num_segment_samples,
                              num_splice_samples,
                              column, sampling_type = 'offset', rejection_list = list(),
                              show_plot = TRUE) {
  stopifnot(class(jv)[1] == "JoinedView",
            sampling_type %in% c('offset', 'gap'),
            is.list(rejection_list))

  if (sampling_type == 'offset') {
    splicing_list <- sample_offset_splice(splicing_df, jv, num_samples = num_splice_samples,
                                          rejection_list = rejection_list)
  } else if (sampling_type == 'gap') {
    splicing_list <- sample_gap_splice(splicing_df, jv, num_samples = num_splice_samples,
                                       rejection_list = rejection_list)
  }

  # Original splice
  sv_orig <- get_spliced_view(jv, splicing_df = splicing_df)
  av_power_orig <- ave_power_spliceview(sv_orig, column = column)
  segment_samples <- sample(2:ncol(av_power_orig), num_segment_samples, replace = TRUE)
  period_samples <- sample(nrow(av_power_orig), num_segment_samples, replace = TRUE)
  original_dfr <- cbind.data.frame(
    Period = av_power_orig[period_samples, 'Period'],
    Average_Power = av_power_orig[cbind(period_samples, segment_samples)]
  )

  # Random splices
  sv_list <- lapply(splicing_list, function(x) get_spliced_view(jv, splicing_df = x))
  df_list <- lapply(sv_list, ave_power_spliceview, column = column)
  ave_power_df <- dplyr::bind_rows(df_list, .id = 'Sample')
  ave_power_data <- ave_power_df[-c(1:2)]
  segment_samples <- sample(ncol(ave_power_df) - 2, num_segment_samples, replace = TRUE)
  period_samples <- sample(nrow(ave_power_df), num_segment_samples, replace = TRUE)
  random_splice_dfr <- cbind.data.frame(
    Period = ave_power_df[period_samples, 'Period'],
    Average_Power = ave_power_data[cbind(period_samples, segment_samples)]
  )

  l <- list(original_dfr, random_splice_dfr)
  names(l) <- c(splice_name, 'Sampled Splices')

  if (show_plot) {
    long_dfr <- dplyr::bind_rows(l, .id = 'Sampled_From')

    subtitle <- c(jv$recording$stem, jv$vid, jv$direct, jv$inst)
    subtitle <- paste(subtitle[subtitle != ""], collapse="_")

    g <- ggplot2::ggplot(long_dfr, ggplot2::aes(x = Period, colour = Sampled_From)) +
      ggplot2::xlab("Period / sec") +
      ggplot2::labs(title = "Comparison of Average Power on Sampled Segments",
                    subtitle = paste(subtitle, ':', column)) +
      scale_x_continuous(trans='log2') +
      ggplot2::geom_line(aes(y = Average_Power)) +
      ggplot2::facet_wrap(~Sampled_From)
    print(g)
  }

  l
}


#' Calculate average power over samples and segments using a splicing table
#'
#' @param jv
#' @param splicing_df
#' @param num_samples
#' @param column
#' @param sampling_type
#' @param include_original
#'
#' @export
#'
#' @examples
ave_power_over_samples <- function(jv, splicing_df, num_samples, column, sampling_type = 'offset',
                                   rejection_list = list(), include_original = TRUE) {

  stopifnot(class(jv)[1] == "JoinedView",
            sampling_type %in% c('offset', 'gap'),
            is.list(rejection_list))

  if (sampling_type == 'offset') {
    splicing_list <- sample_offset_splice(splicing_df, jv, num_samples = num_samples,
                                          rejection_list = rejection_list)
  } else if (sampling_type == 'gap') {
    splicing_list <- sample_gap_splice(splicing_df, jv, num_samples = num_samples,
                                          rejection_list = rejection_list)
  }
  if (include_original) splicing_list$Original <- splicing_df

  sv_list <- lapply(splicing_list, function(x) get_spliced_view(jv, splicing_df = x))
  df_list <- lapply(sv_list, ave_power_spliceview, column = column)
  ave_power_df <- dplyr::bind_rows(df_list, .id = 'Sample')

  sample_ave_power <- ave_power_df %>%
    dplyr::filter(Sample != 'Original') %>%
    dplyr::group_by(Period) %>%
    dplyr::summarise(dplyr::across(!Sample, mean, na.rm = TRUE))

  original_ave_power <- ave_power_df %>%
    dplyr::filter(Sample == 'Original') %>%
    dplyr::group_by(Period) %>%
    dplyr::summarise(dplyr::across(!Sample, mean, na.rm = TRUE))
  ave_power_df <- dplyr::bind_rows(Sample_Mean = sample_ave_power,
                                   Original = original_ave_power, .id = 'Sample')

  long_ave_power_df <- tidyr::pivot_longer(ave_power_df, cols = !c(Sample, Period),
                                           names_to = 'Segment', values_to = 'Average_Power')
  long_ave_power_df
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

  obj <- wavelet_list$output[[1]]
  period.tick.label <- 2^(obj$axis.2) / sv$recording$fps

  cbind.data.frame(Period = period.tick.label, output_mat)
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
sample_ave_cross_power_spliceview <- function(sv, num_samples, replace = TRUE, ...) {
  cross_wavelet_list <- apply_segment_spliceview(sv, FUN = analyze_coherency, ...)
  output_mat <- sapply(cross_wavelet_list$output, function(x) x$Power.xy.avg)

  # Each element in cross_wavelet_list$output has the same frequency scale so take first
  axis_2_mat  <- sapply(cross_wavelet_list$output, function(x) x$axis.2)
  period_value <- 2^(axis_2_mat[,1]) / sv$recording$fps

  period_sample <- sample(nrow(output_mat), num_samples, replace = replace)
  segment_sample <- sample(ncol(output_mat), num_samples, replace = replace)
  sampled_cross_power <- cbind.data.frame(
    Period = period_value[period_sample],
    Average_Cross_Power = output_mat[cbind(period_sample, segment_sample)]
  )

  sampled_cross_power
}


#' @export
#'
#' @examples
sample_ave_power_spliceview <- function(sv, num_samples, replace = TRUE, ...) {
  wavelet_list <- apply_segment_spliceview(sv, FUN = analyze_wavelet, ...)
  output_mat <- sapply(wavelet_list$output, function(x) x$Power.avg)

  # Each element in wavelet_list$output has the same frequency scale so take first
  axis_2_mat  <- sapply(wavelet_list$output, function(x) x$axis.2)
  period_value <- 2^(axis_2_mat[,1]) / sv$recording$fps

  period_sample <- sample(nrow(output_mat), num_samples, replace = replace)
  segment_sample <- sample(ncol(output_mat), num_samples, replace = replace)
  sampled_power <- cbind.data.frame(
    Period = period_value[period_sample],
    Average_Power = output_mat[cbind(period_sample, segment_sample)]
  )

  sampled_power
}


#' Compare the average power distribution of two SplicedViews using sampling on
#' each segment
#'
#' @param sv1
#' @param sv2
#' @param name1
#' @param name2
#' @param num_samples
#' @param column
#' @param show_plot
#'
#' @export
#'
#' @examples
compare_avg_power2 <- function(sv1, sv2, name1, name2, num_samples,
                                       column, show_plot = TRUE) {
  stopifnot(class(sv1)[1] == 'SplicedView', class(sv2)[1] == 'SplicedView',
            sv1$recording$stem == sv2$recording$stem, length(column) == 1)

  sampled1_dfr <- sample_ave_power_spliceview(sv1, num_samples = num_samples, column = column)
  sampled2_dfr <- sample_ave_power_spliceview(sv2, num_samples = num_samples, column = column)

  l <- list(sampled1_dfr, sampled2_dfr)
  names(l) <- c(name1, name2)

  if (show_plot) {

    long_dfr <- dplyr::bind_rows(l, .id = 'Sampled_From')

    subtitle <- c(sv1$recording$stem, sv1$vid, sv1$direct, sv1$inst)
    subtitle <- paste(subtitle[subtitle != ""], collapse="_")

    g <- ggplot2::ggplot(long_dfr, ggplot2::aes(x = Period, colour = Sampled_From)) +
      ggplot2::xlab("Period / sec") +
      ggplot2::labs(title = "Comparison of Average Power on Sampled Segments",
                    subtitle = paste(subtitle, ':', column)) +
      scale_x_continuous(trans='log2') +
      ggplot2::geom_line(aes(y = Average_Power)) +
      ggplot2::facet_wrap(~Sampled_From)
    print(g)
  }

  invisible(l)
}


#' Compare the average cross power distribution of two SplicedViews using
#' sampling on each segment
#'
#' @param sv1
#' @param sv2
#' @param name1
#' @param name2
#' @param num_samples
#' @param columns
#' @param show_plot
#'
#' @export
#'
#' @examples
compare_avg_cross_power2 <- function(sv1, sv2, name1, name2, num_samples,
                                       columns, show_plot = TRUE) {
  stopifnot(class(sv1)[1] == 'SplicedView', class(sv2)[1] == 'SplicedView',
            sv1$recording$stem == sv2$recording$stem, length(columns) == 2)

  sampled1_dfr <- sample_ave_cross_power_spliceview(sv1, num_samples = num_samples,
                                                    columns = columns)
  sampled2_dfr <- sample_ave_cross_power_spliceview(sv2, num_samples = num_samples,
                                                    columns = columns)
  l <- list(sampled1_dfr, sampled2_dfr)
  names(l) <- c(name1, name2)

  if (show_plot) {

    long_dfr <- dplyr::bind_rows(l, .id = 'Sampled_From')

    subtitle <- c(sv1$recording$stem, sv1$vid, sv1$direct, sv1$inst)
    subtitle <- paste(subtitle[subtitle != ""], collapse="_")

    g <- ggplot2::ggplot(long_dfr, ggplot2::aes(x = Period, colour = Sampled_From)) +
      ggplot2::xlab("Period / sec") +
      ggplot2::labs(title = "Comparison of Average Cross Power on Sampled Segments",
                    subtitle = paste(subtitle, ':', columns, collapse = " ")) +
      scale_x_continuous(trans='log2') +
      ggplot2::geom_line(aes(y = Average_Cross_Power)) +
      ggplot2::facet_wrap(~Sampled_From)
    print(g)
  }

  invisible(l)
}


#' Randomly create matching segments from a splicing table without overlaps
#'
#' Works by adding a random offset to each start time in the splice. Uses rejection
#' sampling to avoid overlaps with the input segments and a additional segments
#' from a list of splices.
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
#' x <- sample_offset_splice(splicing_df, rv1, num_samples = 1000)
sample_offset_splice <- function(splicing_dfr, v, num_samples, rejection_list = list()) {
  stopifnot(is.data.frame(splicing_dfr), "View" %in% class(v),
            num_samples > 0, is.list(rejection_list))

  # Discard random splices that appear in the rejection list - includes the original splice
  rejection_splices <- c(list(splicing_dfr), rejection_list)

  # Find max possible offset based on recording length
  max_time <- max(v$df[['Time']], na.rm = TRUE)

  # Span of the splice
  max_splice <- max(splicing_dfr[['End']], na.rm = TRUE)
  min_splice <- min(splicing_dfr[['Start']], na.rm = TRUE)
  stopifnot(min_splice >= 0, max_splice <= max_time)

  splicing_list <- list()
  current_num_samples <- 0

  # Repeat until we get the desired number of samples
  while(current_num_samples < num_samples) {

    # Random start times
    start_times <- runif(num_samples, min = -min_splice, max = max_time - max_splice)

    # Generate a list of new sampling data.frames
    new_splicing_list <- lapply(start_times, function(x) {
      new_splicing_dfr <- splicing_dfr
      new_splicing_dfr[c('Start', 'End')] <- x + new_splicing_dfr[c('Start', 'End')]
      if (new_splicing_dfr[nrow(new_splicing_dfr), 'End'] > max_time) browser()
      new_splicing_dfr
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
    message("Accepted samples: ", current_num_samples)
  }

  splicing_list <- splicing_list[seq_len(num_samples)]
  names(splicing_list) <- paste('Sample splice', seq_along(splicing_list))
  splicing_list
}


#' Randomly create matching segments from a splicing table without overlaps
#'
#' Works by randomly varying the gaps between segments assuming that the gap number
#' follow a Poisson process with rate given by the average sample gap length in
#' the input splice. Durations of segments remain the same.
#'
#' Uses rejection sampling to avoid overlaps with the input segments and a
#' additional segments from a list of splices.
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
#' x <- sample_gap_splice(splicing_df, rv1, num_samples = 1000)
sample_gap_splice <- function(splicing_dfr, v, num_samples, rejection_list = list()) {

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

  # Number of gaps follows Poisson process
  # gap_dfr <- dplyr::mutate(splicing_dfr, Next_Start = dplyr::lead(Start))
  # gap_dfr <- dplyr::mutate(gap_dfr, Duration = End - Start)
  # gap_dfr <- dplyr::mutate(gap_dfr, Gap = Next_Start - Start)
  # gap_dfr <- dplyr::mutate(gap_dfr, Prev_Gap = dplyr::lag(Gap, default = Start[1]))
  duration <- splicing_dfr$End - splicing_dfr$Start
  lagged_duration <- dplyr::lag(duration, default = 0)
  gaps <- dplyr::lead(splicing_dfr$Start, default = max_time) - splicing_dfr$Start
  gaps <- c(splicing_dfr$Start[1], gaps)
  ave_gap_splice <- mean(gaps, na.rm = TRUE) # seconds

  # Repeat until we get the desired number of samples
  while(current_num_samples < num_samples) {

    # Generate a list of new sampling data.frames
    new_splicing_list <- lapply(seq_len(num_samples), function(x) {
      # gap is interarrival time and exponentially distributed
      new_gaps <- rexp(nrow(splicing_dfr), rate = 1 / ave_gap_splice)
      new_splicing_dfr <- splicing_dfr
      new_splicing_dfr$Start <- cumsum(new_gaps + lagged_duration)
      new_splicing_dfr$End <- new_splicing_dfr$Start + duration
      new_splicing_dfr
    })

    # Which ones overlap the original splicing or go beyond recording?
    is_rejected <- rep(FALSE, length(new_splicing_list))
    for (rsp in rejection_splices) {
      is_rejected <- is_rejected |
        sapply(new_splicing_list, function(x) is_splice_overlapping(x, rsp)) |
        sapply(new_splicing_list, function(x) max(x[nrow(x), 'End'], na.rm = TRUE) > max_time)
    }

    # Remove the overlapping ones
    splicing_list <- c(splicing_list, new_splicing_list[!is_rejected])
    current_num_samples <- length(splicing_list)
    message("Accepted samples: ", current_num_samples)
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
difference_onsets <- function(onset_obj, instruments, splicing_dfr = NULL, segment_tala = FALSE) {

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
    output_dfr$Segment <- if (segment_tala) output_dfr$Tala else 'All'
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


#' Visualise random splices
#'
#' @param splicing_list
#' @param jv
#' @param overlay
#'
#' @return
#' @export
#'
#' @examples
visualise_sample_splices <- function(splicing_list, jv, overlay = TRUE) {
  stopifnot(is.list(splicing_list), 'View' %in% class(jv))

  subtitle <- c(jv$recording$stem, jv$vid, jv$direct, jv$inst)
  subtitle <- paste(subtitle[subtitle != ""], collapse="_")

  df <- dplyr::bind_rows(splicing_list)
  ggplot(df, aes(y = Segment)) +
    ggplot2::labs(title = "Visualisation of Random Splices", subtitle = subtitle) +
    geom_linerange(aes(xmin = Start, xmax = End)) +
    geom_rect(data = splicing_tabla_solo_df,
              aes(xmin = Start, xmax = End, ymin = 0, ymax = Inf, fill = Segment), alpha = 0.5) +
    ggplot2::xlab("Time / min:sec") +
    ggplot2::scale_x_time(labels = function(l) strftime(l, '%M:%S'))


}
