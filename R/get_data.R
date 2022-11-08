#' Get a meta-data recording object
#'
#' @param stem
#' @param folder_in
#' @param path
#' @param fps
#'
#' @return
#' @export
#'
#' @examples
#' r <- get_recording("NIR_ABh_Puriya", fps = 25)
get_recording <- function(stem, fps, folder_in = "Original", path = "C:/data/movementsync") {
  data_path <- file.path(path, folder_in)
  data_files <- list.files(data_path, pattern = paste0("^", stem, ".*\\.csv") )
  l <- list(data_root = path, data_path = data_path, data_files = data_files, stem = stem,
            fps = fps)
  class(l) <- "Recording"
  l
}


#' Get onsets selected files
#'
#' @param recording
#'
#' @return
#' @export
#'
#' @examples
#' r <- get_recording("NIR_ABh_Puriya", fps = 25)
#' o <- get_onsets_selected_data(r)
get_onsets_selected_data <- function(recording) {

  # Identify onset files
  is_onsets_selected_file <- grepl(
    paste0("^", recording$stem, ".*_Onsets_Selected_.*\\.csv"),
    recording$data_files)
  onsets_selected_files <- file.path(recording$data_path, recording$data_files[is_onsets_selected_file])
  message("Loading ", paste(basename(onsets_selected_files), collapse = ", "))

  output_onsets_selected <- list()
  for (fil in onsets_selected_files) {
    output_onsets_selected[[basename(fil)]] <- read.csv(fil)
  }
  class(output_onsets_selected) <- "OnsetsSelected"

  invisible(output_onsets_selected)
}


#' Get metre files
#'
#' @param recording
#'
#' @return
#' @export
#'
#' @examples
#' r <- get_recording("NIR_ABh_Puriya", fps = 25)
#' o <- get_metre_data(r)
#' r2 <- get_recording("NIR_DBh_Malhar_2Gats", fps = 25)
#' m2 <- get_metre_data(r2)
get_metre_data <- function(recording) {

  # Identify metre files
  is_metre_file <- grepl(
    paste0("^", recording$stem, ".*_Metre_.*\\.csv"),
    recording$data_files)
  metre_files <- file.path(recording$data_path, recording$data_files[is_metre_file])

  message("Loading ", paste(basename(metre_files), collapse = ", "))

  output_metre <- list()
  for (fil in metre_files) {
    output_metre[[basename(fil)]] <- read.csv(fil)
  }
  names(output_metre) <- sub(".*_Metre_(.*)\\.csv", "\\1", names(output_metre))

  # Order on time
  min_time <- sapply(output_metre, function(x) min(x$Time, na.rm = TRUE))
  output_metre <- output_metre[order(min_time)]

  class(output_metre) <- "Metre"
  invisible(output_metre)
}


#' Get duration annotation data
#'
#' @param recording
#'
#' @return
#' @export
#'
#' @examples
#' r <- get_recording("NIR_ABh_Puriya", fps = 25)
#' df <- get_duration_annotation_data(r)
get_duration_annotation_data <- function(recording) {

  # Identify duration files
  is_duration_file <- grepl(
    paste0("^", recording$stem, ".*_(Annotation|MD)\\.csv"),
    recording$data_files)
  duration_files <- file.path(recording$data_path, recording$data_files[is_duration_file])

  message("Loading ", paste(basename(duration_files), collapse = ", "))

  output_list <- list()
  for (fil in duration_files) {
    output_list[[basename(fil)]] <- read.csv(fil, header = FALSE)
  }

  output_dfr <- do.call(rbind.data.frame, c(output_list, make.row.names = FALSE))
  colnames(output_dfr) <- c("Tier", "In", "Out", "Duration", "Comments")
  class(output_dfr) <- c("Duration", class(output_dfr))

  output_dfr
}


#' Get view from NS video data
#'
#' Creates time reference and displacement from raw data
#' @param recording
#' @param vid
#' @param inst
#' @param direct
#' @param save_output
#' @param folder_out
#'
#' @return
#' @export
#'
#' @examples
#' r <- get_recording("NIR_ABh_Puriya", fps = 25)
#' v <- get_raw_view(r, "Central", NULL, "Sitar")
get_raw_view <- function(recording, vid, direct, inst,
                        folder_out = "Raw", save_output = TRUE) {
  data_file_name <- file.path(recording$data_path,
                              paste0(recording$stem, "_", vid, "_", direct ,'NS_', inst, '.csv'))
  message("Loading ", data_file_name)
  stopifnot(file.exists(data_file_name))

  df <- read.csv(data_file_name, colClasses = "numeric")
  first_col <- colnames(df)[1]
  data_points <- unique(sapply(strsplit(colnames(df), "_"), function(x) x[1]))[-1]
  x_colnames <- paste0(data_points, "_x")
  y_colnames <- paste0(data_points, "_y")
  selected_cols <- c(first_col, rbind(x_colnames, y_colnames))
  if (!all(selected_cols %in% colnames(df))) stop("Cannot find data points in data file")
  df <- df[selected_cols]

  # Add a time column
  df <- cbind(df[1], Time = df$X / recording$fps, df[-1])

  # Add a displacement column
  dx <- as.data.frame(lapply(df[x_colnames], function(x) c(NA, diff(x))))
  dy <- as.data.frame(lapply(df[y_colnames], function(x) c(NA, diff(x))))
  disp <- sqrt(dx^2 + dy^2)
  colnames(disp) <- paste0(data_points, "_d")
  df <- cbind(df, disp)
  selected_cols <- c(first_col, "Time", rbind(x_colnames, y_colnames, colnames(disp)))
  df <- df[selected_cols]

  if (save_output) {
    out_folder <- file.path(recording$data_root, folder_out)
    if (!dir.exists(out_folder)) dir.create(out_folder)
    out_file_name <- file.path(out_folder, paste0(recording$stem, "_", vid , "_", inst, '_RAW.csv'))
    write.csv(df, out_file_name, row.names=FALSE)
  }
  l <- list(df = df, vid = vid, direct = direct,
       inst = inst, recording = recording)
  class(l) <- "RawView"

  invisible(l)
}


#' Get processed view from NS video data
#'
#' Does normalisation and interpolation of missing data in the view
#'
#' @param rv
#' @param save_output
#' @param folder_out
#'
#' @return
#' @export
#'
#' @examples
#' r <- get_recording("NIR_ABh_Puriya", fps = 25)
#' rv <- get_raw_view(r, "Central", "", "Sitar")
#' pv <- get_processed_view(rv)
get_processed_view <- function(rv, folder_out = "Normalized", save_output = TRUE) {

  df <- rv$df
  first_cols <- colnames(df)[1:2]
  data_points <- unique(sapply(strsplit(colnames(df), "_"), function(x) x[1]))[-(1:2)]
  x_colnames <- paste0(data_points, "_x")
  y_colnames <- paste0(data_points, "_y")
  d_colnames <- paste0(data_points, "_d")
  selected_cols <- c(first_cols, x_colnames, y_colnames, d_colnames)
  df <- df[selected_cols]

  # Split dataframe into x- and y- columns and determine which dimension has the larger extent
  dfx <- df[x_colnames]
  dfy <- df[y_colnames]

  max_x <- max(dfx, na.rm = TRUE)
  min_x <- min(dfx, na.rm = TRUE)
  max_y <- max(dfy, na.rm = TRUE)
  min_y <- min(dfy, na.rm = TRUE)

  size <- max((max_x - min_x),(max_y - min_y))

  # Normalise position data of all columns to the larger dimension (=0:1) and move midpoint to 0.5
  # Y-dimension inverted to make (0,0) bottom left of plots
  dfx_norm <- (dfx - min_x)/size
  dfx_norm <- dfx_norm + 0.5 - (max(dfx_norm, na.rm=TRUE) + min(dfx_norm, na.rm=TRUE))/2

  dfy_norm <- 1 - (dfy - min_y)/size
  dfy_norm <- dfy_norm + 0.5 - (max(dfy_norm, na.rm=TRUE) + min(dfy_norm, na.rm=TRUE))/2

  # Recombine dataframe
  X <- df[first_cols]
  df_norm <- cbind(X, dfx_norm, dfy_norm)
  cn <- c(colnames(X), rbind(colnames(dfx_norm), colnames(dfy_norm)))
  df_norm <- df_norm[cn]

  # Interpolate missing data
  df_norm <- replace(zoo::na.spline(df_norm), is.na(zoo::na.approx(df_norm, na.rm=FALSE)), NA)
  df_norm <- as.data.frame(df_norm)

  if (save_output) {
    out_folder <- file.path(rv$recording$data_root, folder_out)
    if (!dir.exists(out_folder)) dir.create(out_folder)
    out_file_name <- file.path(out_folder, paste0(rv$recording$stem, "_", rv$vid , "_", rv$inst, '_NORM.csv'))
    write.csv(df_norm, out_file_name, row.names=FALSE)
  }
  l <- list(df_norm = df_norm, vid = rv$vid, direct = rv$direct,
            inst = rv$inst, recording = rv$recording)
  class(l) <- "ProcessedView"

  invisible(l)
}


#' Apply a filter
#'
#' @param view
#' @param data_points
#' @param window_size
#' @param poly_order
#' @param folder_out
#' @param save_output
#'
#' @return
#' @export
#'
#' @examples
#' r <- get_recording("NIR_ABh_Puriya", fps = 25)
#' rv <- get_raw_view(r, "Central", "", "Sitar")
#' pv <- get_processed_view(rv)
#'
#' filt1 <- apply_filter(pv, c("Nose", "RWrist", "LWrist"), window_size=19, poly_order=4)
#' filt2 <- apply_filter(pv, c("Nose", "RWrist", "LWrist"), window_size=41, poly_order=3)
apply_filter <- function(view, data_points, window_size, poly_order, folder_out = "Filtered", save_output=TRUE) {

  df_norm <- view$df_norm
  first_cols <- colnames(df_norm[1:2])
  x_colnames <- paste0(data_points, "_x")
  y_colnames <- paste0(data_points, "_y")
  df_filt <- df_norm[c(first_cols, rbind(x_colnames, y_colnames))]

  # Apply Savitsky-Golay filter
  for (cn in c(x_colnames, y_colnames)) {
    df_filt[[cn]] <- signal::sgolayfilt(df_filt[[cn]], p = poly_order, n = window_size)
  }

  # Save version
  if (save_output) {
    out_folder <- file.path(view$recording$data_root, folder_out)
    if (!dir.exists(out_folder)) dir.create(out_folder)
    out_file_name <- file.path(out_folder,
      paste0(view$recording$stem, "_", view$vid, "_", view$inst, '_SEL_', window_size, '_', poly_order,'.csv')
    )
    write.csv(df_filt, out_file_name, row.names=FALSE)
  }
  l <- list(df_filt = df_filt, view = view)
  class(l) <- "FilteredView"

  invisible(l)
}



