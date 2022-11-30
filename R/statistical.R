#


#' Apply summary function to SpliceView object
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
apply_summary_spliceview <- function(sv, FUN, simplify = FALSE, USE.NAMES = FALSE, ...) {
  v_list <- split(sv)
  sapply(v_list, function(x) {
    keys <- match(c('Tier', 'Frame', 'Time'), colnames(x$df), nomatch = 0)
    dfr <- x$df[-keys]
    apply(dfr, 2, function(y) FUN(y, ...))
  }, simplify = simplify, USE.NAMES = USE.NAMES)
}


#' Apply summary function to SpliceView object
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
sapply_summary_spliceview <- function(sv, FUN, simplify = TRUE, USE.NAMES = TRUE, ...) {
  apply_summary_spliceview(sv, FUN, simplify = simplify, USE.NAMES = USE.NAMES, ...)
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
    tier_list <- list()
    for (r in seq_len(nrow(splicing_dfr))) {
      a <- splicing_dfr[r, 'Start']
      b <- splicing_dfr[r, 'End']
      tier <-  splicing_dfr[r, 'Tier']
      tier_list[[tier]] <- output_dfr[
        !is.na(output_dfr$Ref_Beat_Time) & output_dfr$Ref_Beat_Time >= a &
          output_dfr$Ref_Beat_Time <= b, ,drop=FALSE]
    }
    output_dfr <- dplyr::bind_rows(tier_list, .id = 'Segment')
  } else {
    output_dfr$Segment <- 'All'
  }

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
