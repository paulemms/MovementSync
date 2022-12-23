# Asynchrony analysis functions

#' Get onset differences
#'
#' Calculates the difference in onset times for each instrument pair in
#' milli-seconds.
#' @param onset_obj `OnsetsSelected` object.
#' @param instruments character vector of instrument names.
#' @param expr R expression to subset onsets (Default is NULL).
#' @param splicing_dfr `Splice` object (Default is NULL).
#'
#' @return `OnsetsDifference` object.
#' @export
#' @family asynchrony analysis functions
#'
#' @examples
#' r1 <- get_sample_recording()
#' o1 <- get_onsets_selected_data(r1)
#' head(difference_onsets(o1, instruments = c('Inst', 'Tabla')))
#' head(difference_onsets(o1, instruments = c('Inst', 'Tabla'), expr = Matra == 3))
difference_onsets <- function(onset_obj, instruments, expr = NULL, splicing_dfr = NULL) {
  e <- substitute(expr)
  difference_onsets_q(onset_obj, instruments, expr = e, splicing_dfr)
}

# version of difference_onsets that requires a quoted expression
difference_onsets_q <- function(onset_obj, instruments, expr = NULL, splicing_dfr = NULL) {
  stopifnot("OnsetsSelected" %in% class(onset_obj),
            is.null(splicing_dfr) || "Splice" %in% class(splicing_dfr))

  dfr_list <- onset_obj[sapply(onset_obj, is.data.frame)]
  dfr <- dplyr::bind_rows(dfr_list, .id = 'Metre')

  if (!is.null(expr)) {
    dfr <- dplyr::filter(dfr, !!expr)
  }

  dfr <- dfr[, c('Metre', instruments), drop=FALSE]

  # Calculate onset differences for each instrument pair
  instrument_combn <- utils::combn(instruments, 2)
  output_dfr <- data.frame(Metre = dfr['Metre'],
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
      a <- splicing_dfr$Start[r]
      b <- splicing_dfr$End[r]

      segment <-  if (is.null(expr)) splicing_dfr$Segment[r] else paste(deparse(expr), splicing_dfr$Segment[r], sep = " & ")
      segment_list[[segment]] <- output_dfr[
        !is.na(output_dfr$Ref_Beat_Time) & output_dfr$Ref_Beat_Time >= a &
          output_dfr$Ref_Beat_Time <= b, ,drop=FALSE]
    }
    output_dfr <- dplyr::bind_rows(segment_list, .id = 'Segment')
  } else {
    output_dfr$Segment <- if (!is.null(expr)) deparse(expr) else 'All'
  }

  # Convert differences to ms
  pair_cols <- apply(instrument_combn, 2, paste, collapse = "-")
  output_dfr[pair_cols] <- output_dfr[pair_cols] * 1000

  class(output_dfr) <- c('OnsetsDifference', 'data.frame')
  output_dfr
}


#' Summary of difference in onsets
#'
#' @param onset_obj `OnsetsSelected` object.
#' @param splicing_dfr `Splice` object
#' @param instruments character vector of instrument names.
#' @param expr R expression to subset onsetsSelected
#' @param recording `Recording` object.
#' @param show_plot show a plot? (Default is FALSE).
#' @param filter_pair regular expression to filter instrument pair names.
#' @param na_omit omit NAs (Default is TRUE).
#' @param time_breaks suggests the number of major time tick marks (Default is NULL).
#'
#' @return a summary data frame of onset difference statistics.
#' @export
#' @family asynchrony analysis functions
#'
#' @examples
#' r1 <- get_sample_recording()
#' o1 <- get_onsets_selected_data(r1)
#' d1 <- get_duration_annotation_data(r1)
#' splice_dfr <- splice_time(d1, tier = 'FORM')
#' summary_onsets(o1, r1, instruments = c('Inst', 'Tabla'),
#'   splicing_dfr = splice_dfr, show_plot = TRUE)
summary_onsets <- function(onset_obj, recording, instruments, splicing_dfr = NULL, expr = NULL,
                           show_plot = FALSE, filter_pair = NULL, na_omit = TRUE, time_breaks = NULL) {

  stopifnot("OnsetsSelected" %in% class(onset_obj),
            "Recording" %in% class(recording),
            is.null(splicing_dfr) || "Splice" %in% class(splicing_dfr))

  breaks <- if (is.null(time_breaks)) ggplot2::waiver() else scales::pretty_breaks(time_breaks)

  e <- substitute(expr)
  dfr <- difference_onsets_q(onset_obj, instruments = instruments, splicing_dfr = splicing_dfr, expr = e)
  if (nrow(dfr) == 0) stop('No data from splice to summarise')

  long_dfr <- tidyr::pivot_longer(dfr, cols = -c(.data$Metre, .data$Ref_Beat_Time, .data$Segment),
                                  names_to = 'Instrument_Pair', values_to = 'Value')

  if (!is.null(filter_pair)) {
    long_dfr <- dplyr::filter(long_dfr, grepl(filter_pair, .data$Instrument_Pair))
  }

  long_dfr$Segment <- factor(long_dfr$Segment, unique(long_dfr$Segment))
  summary_dfr <- dplyr::select(long_dfr, -c(.data$Metre, .data$Ref_Beat_Time))
  summary_dfr <- dplyr::group_by(summary_dfr, .data$Instrument_Pair, .data$Segment)
  summary_dfr <- dplyr::summarise(
    summary_dfr,
    'N' = sum(!is.na(.data$Value)),
    'Mean Difference' = mean(.data$Value, na.rm = TRUE),
    'Mean Absolute Difference' = mean(abs(.data$Value), na.rm = TRUE),
    'SD Difference' = stats::sd(.data$Value, na.rm = TRUE)
  )

  if (na_omit) {
    summary_dfr <- dplyr::filter(summary_dfr, .data$N > 0)
  }


  if (show_plot) {

    long_dfr <- tidyr::pivot_longer(summary_dfr, cols = -c('Segment', 'Instrument_Pair'),
                                    names_to = 'Statistic', values_to = 'Value')
    long_dfr$Statistic_f <- factor(long_dfr$Statistic, unique(long_dfr$Statistic))

    g <- ggplot2::ggplot(long_dfr) +
      ggplot2::geom_col(ggplot2::aes(x = .data$Value, y = .data$Instrument_Pair, fill = .data$Statistic)) +
      ggplot2::xlab('Value / number or ms') +
      ggplot2::scale_x_continuous(breaks = breaks) +
      ggplot2::theme(legend.position = "none") +
      ggplot2::facet_grid(Segment ~ Statistic_f,
                          labeller = ggplot2::labeller(Statistic_f = ggplot2::label_wrap_gen(12),
                                                       Segment = ggplot2::label_wrap_gen(12)), scales = 'free_x') +
      ggplot2::ggtitle("Summary of Onset Difference Statistics for Instrument Pairs",
                       subtitle = recording$stem)
    print(g)
  }

  invisible(as.data.frame(summary_dfr))
}


#' Helper for ggpairs plots
#'
#' Finds the column numbers of a subset of instrument pairs in a
#' `OnsetsDifference` object using partial matching.
#'
#' @param obj 'OnsetsDifference' object.
#' @param instrument_pairs character vector of instrument pairs or
#' a single instrument pair for partial matching.
#' @param instrument_names character vector of instrument names
#' @param add_ref_beat_time add a Reference_Beat_Time column? (Default is FALSE).
#' @param add_segment add a Segment column? (Default is FALSE).
#' @param add_metre add a Metre column? (Default is FALSE).
#' @param partial should matching be partial? (Default is FALSE)
#'
#' @return numeric vector of column positions in the `OnsetsDifference` object
#' @export
#' @family asynchrony analysis functions
#'
#' @examples
#' \dontrun{
#' library(GGally)
#' instruments <- c("Shoko_L", "Shoko_R", "Taiko", "Kakko", "Kakko_1", "So", "Biwa",
#' "Ryuteki", "Hichiriki", "Sho", "Biwa_RW", "Shoko_RW", "Taiko_LW", "Taiko_RW")
#' r5 <- get_recording("Gagaku_5_Juha", fps = 60)
#' o5 <- get_onsets_selected_data(r5)
#' po5 <- difference_onsets(o5, instruments = instruments)
#' pair_cols <- get_pair_columns(po5, c("Shoko_L-Kakko", "So-Biwa"), instruments)
#' ggpairs(po5, columns = pair_cols)
#' }
get_pair_columns <- function(obj, instrument_pairs, instrument_names,
                             add_ref_beat_time = FALSE, add_segment = FALSE,
                             add_metre = FALSE, partial = FALSE) {
  stopifnot(class(obj)[1] == 'OnsetsDifference')

  to_match <- instrument_pairs
  if (add_ref_beat_time) to_match <- c("Ref_Beat_Time", to_match)
  if (add_segment) to_match <- c(to_match, "Segment")
  if (add_metre) to_match <- c(to_match, "Metre")
  if (partial) {
    if (length(to_match) > 1) stop('Only one instrument_pairs is allowed for partial matching')
    ind <- grep(to_match, colnames(obj))
    if (length(ind) == 1) stop('No matches found')
  } else {
    ind <- match(to_match, colnames(obj), nomatch = NA)
    if (anyNA(ind)) stop('Cannot find all the instrument pairs')
  }

  # Check for data
  is_empty_col <- colSums(is.na(obj[ind])) == nrow(obj)
  if (sum(is_empty_col) > 0) {
    warning('Ignoring instrument pair columns with no data')
    ind <- ind[!is_empty_col]
  }

  ind
}

#' Helper to add title to pairs plot
#'
#' @param recording `Recording` object.

#' @export
pairs_title <- function(recording) {
  ggplot2::ggtitle("Instrument Pairs Onset Differences / ms",
                   subtitle = recording$stem)
}
