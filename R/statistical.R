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
#'
#' @return
#' @export
#'
#' @examples
#' r1 <- get_recording("NIR_ABh_Puriya", fps = 25)
#' o1 <- get_onsets_selected_data(r1)
#' dfr <- get_processed_onsets(o1, instruments = c('Inst', 'Tabla'))
get_processed_onsets <- function(onset_obj, instruments) {

  instrument_list <- lapply(onset_obj, function(x) x[,instruments,drop=FALSE])
  dfr <- dplyr::bind_rows(instrument_list, .id = 'Tala')

  # Calculate onset differences for each instrument pair
  instrument_combn <- combn(instruments, 2)
  output_dfr <- data.frame(Ref_Beat_Time = rowMeans(dfr[instruments], na.rm = TRUE))
  for (j in seq_len(ncol(instrument_combn))) {
    inst1 <- instrument_combn[1, j]
    inst2 <- instrument_combn[2, j]
    col_name <- paste(inst1, inst2, sep = "-")
    output_dfr[col_name] <- dfr[inst1] - dfr[inst2]
  }

  output_dfr
}
