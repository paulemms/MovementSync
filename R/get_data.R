#' Get NS video data
#'
#' @param stem
#' @param vid
#' @param direc
#' @param inst
#' @param folder_in
#' @param folder_out
#' @param path
#'
#' @return
#' @export
#'
#' @examples
#' ns_data <- get_NS_data("NIR_ABh_Puriya", "Central", "", "Sitar", path="C:/data/movementsync")
get_NS_data <- function(stem, vid, diry, inst, folder_in = "Original",
                        folder_out = "Normalized", path = ".", save_output = TRUE) {
  data_file_name <- file.path(path, folder_in, paste0(stem, "_", vid, "_", diry ,'NS_', inst, '.csv'))
  message("Loading ", data_file_name)
  stopifnot(file.exists(data_file_name))

  df <- read.csv(data_file_name, colClasses = "numeric")
  first_col <- colnames(df)[1]
  data_points <- unique(sapply(strsplit(colnames(df), "_"), function(x) x[1]))[-1]
  x_colnames <- paste0(data_points, "_x")
  y_colnames <- paste0(data_points, "_y")
  selected_cols <- c(first_col, x_colnames, y_colnames)
  if (!all(selected_cols %in% colnames(df))) stop("Cannot find data points in data file")
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
  X <- df[[first_col]]
  df_norm <- cbind(X, dfx_norm, dfy_norm)

  # Interpolate missing data
  df_norm <- replace(zoo::na.spline(df_norm), is.na(zoo::na.approx(df_norm, na.rm=FALSE)), NA)
  df_norm <- as.data.frame(df_norm)

  if (save_output) {
    out_folder <- file.path(path, folder_out)
    if (!dir.exists(out_folder)) dir.create(out_folder)
    out_file_name <- file.path(path, folder_out, paste0(stem, "_", vid , "_", inst, '_NORM.csv'))
    write.csv(df_norm, out_file_name, row.names=FALSE)
  }

  invisible(list(df_norm = df_norm, stem = stem, vid = vid, diry = diry,
                 inst = inst, path = path))
}


#' Apply a filter
#'
#' @param ns_data
#' @param data_points
#' @param window_size
#' @param poly_order
#'
#' @return
#' @export
#'
#' @examples
#' ns_data <- get_NS_data("NIR_ABh_Puriya_", "Central_", "", "Sitar")
#' ns_filt1 <- apply_filter(ns_data, c("Nose", "RWrist", "LWrist"), window_size=41, poly_order=3)
#' ns_filt2 <- apply_filter(ns_data, c("Nose", "RWrist", "LWrist"), window_size=41, poly_order=3)
apply_filter <- function(ns_data, data_points, window_size, poly_order, folder_out = "Filtered", save_output=TRUE) {

  df_norm <- ns_data$df_norm
  first_col <- colnames(df_norm[1])
  x_colnames <- paste0(data_points, "_x")
  y_colnames <- paste0(data_points, "_y")
  df_filt <- df_norm[c(first_col, x_colnames, y_colnames)]

  # Apply Savitsky-Golay filter
  for (cn in c(x_colnames, y_colnames)) {
    df_filt[[cn]] <- signal::sgolayfilt(df_filt[[cn]], p = poly_order, n = window_size)
  }

  # Save version
  if (save_output) {
    out_folder <- file.path(ns_data$path, folder_out)
    if (!dir.exists(out_folder)) dir.create(out_folder)
    out_file_name <- file.path(out_folder,
      paste0(ns_data$stem, "_", ns_data$vid, "_", ns_data$inst, '_SEL_', window_size, '_', poly_order,'.csv')
    )
    write.csv(df_filt, out_file_name, row.names=FALSE)
  }

  invisible(list(df_filt = df_filt, stem = ns_data$stem, vid = ns_data$vid,
                 diry = ns_data$diry, inst = ns_data$inst, path = ns_data$path))
}



#' Get onsets selected files
#'
#' @param stem
#' @param folder_in
#' @param path
#'
#' @return
#' @export
#'
#' @examples
#' o <- get_onsets_selected_data("NIR_ABh_Puriya")
get_onsets_selected_data <- function(stem, folder_in = "Original", path = "C:/data/movementsync") {

  # Get onset selected files
  onsets_selected_files <- list.files(file.path(path, folder_in),
                                      pattern = paste0("^", stem, ".*_Onsets_Selected_.*\\.csv"),
                                      full.names = TRUE)
  message("Loading ", paste(onsets_selected_files, collapse = ", "))

  output_onsets_selected <- list()
  for (fil in onsets_selected_files) {
    output_onsets_selected[[basename(fil)]] <- read.csv(fil)
  }

  invisible(output_onsets_selected)
}


#' Get metre files
#'
#' @param stem
#' @param folder_in
#' @param path
#'
#' @return
#' @export
#'
#' @examples
#' o <- get_metre_data("NIR_ABh_Puriya")
get_metre_data <- function(stem, folder_in = "Original", path = "C:/data/movementsync") {

  # Get metre files
  metre_files <- list.files(file.path(path, folder_in),
                            pattern = paste0("^", stem, ".*_Metre_.*\\.csv"),
                            full.names = TRUE)
  message("Loading ", paste(metre_files, collapse = ", "))

  output_metre_selected <- list()
  for (fil in metre_files) {
    output_metre_selected[[basename(fil)]] <- read.csv(fil)
  }
  class(output_metre_selected) <- "Metre"

  invisible(output_metre_selected)
}


#' Get duration annotation data
#'
#' @param stem
#' @param path
#'
#' @return
#' @export
#'
#' @examples
#' df <- get_duration_annotation_data("NIR_ABh_Puriya")
get_duration_annotation_data <- function(stem, folder_in = "Original", path = "C:/data/movementsync") {

  duration_files <- list.files(
    file.path(path, folder_in),
    pattern = paste0("^", stem, ".*_(Annotation|MD)\\.csv"),
    full.names = TRUE)

  output_list <- list()
  for (fil in duration_files) {
    output_list[[basename(fil)]] <- read.csv(fil, header = FALSE)
  }

  output_dfr <- do.call(rbind.data.frame, c(output_list, make.row.names = FALSE))
  colnames(output_dfr) <- c("Tier", "In", "Out", "Duration", "Comments")

  output_dfr
}

