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
get_recording <- function(stem, fps, folder_in = "Original", path = "~/movementsync") {
  stopifnot(dir.exists(path))

  data_path <- file.path(path, folder_in)
  data_files <- list.files(data_path, pattern = paste0("^", stem, ".*\\.csv") )
  l <- list(data_home = path, data_path = data_path, data_files = data_files, stem = stem,
            fps = fps)
  class(l) <- "Recording"
  l
}


#' Get onsets selected files
#'
#' @param recording
#' @param instrument_cols
#'
#' @return
#' @export
#'
#' @examples
#' r <- get_recording("NIR_ABh_Puriya", fps = 25)
#' o <- get_onsets_selected_data(r)
get_onsets_selected_data <- function(recording, instrument_cols = NULL) {

  # Identify onset files
  is_onsets_selected_file <- grepl(
    paste0("^", recording$stem, ".*_Onsets_Selected_.*\\.csv"),
    recording$data_files)
  onsets_selected_files <- file.path(recording$data_path, recording$data_files[is_onsets_selected_file])
  message("Loading ", paste(basename(onsets_selected_files), collapse = ", "))

  output_onsets_selected <- list()
  for (fil in onsets_selected_files) {
    df <- read.csv(fil)
    # specify colClasses?
    if ("Matra" %in% colnames(df)) {
      df[["Matra"]] <- suppressWarnings(as.integer(df[["Matra"]]))
    }
    if (!is.null(instrument_cols)) {
      df <- tidyr::pivot_longer(df, cols = instrument_cols, names_to = "Inst.Name",
                          values_to = "Inst")
      df[["Inst.Peak"]] <- 0
    }
    output_onsets_selected[[basename(fil)]] <- df
  }
  names(output_onsets_selected) <- sub(".*_Onsets_Selected_(.*)\\.csv", "\\1", names(output_onsets_selected))

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
    paste0("^", recording$stem, ".*_Metre(_|).*\\.csv"),
    recording$data_files)
  metre_files <- file.path(recording$data_path, recording$data_files[is_metre_file])
  message("Loading ", paste(basename(metre_files), collapse = ", "))

  output_metre <- list()
  for (fil in metre_files) {
    output_metre[[basename(fil)]] <- read.csv(fil)
  }
  names(output_metre) <- sub(".*_Metre(_|)(.*)\\.csv", "\\2", names(output_metre))

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
#' Creates time reference and displacement from raw csv data
#'
#' Adds OptFlow data for head if present and a camera is specified in filename.
#'
#' @param recording object
#' @param vid video camera
#' @param direct direction
#' @param inst instrument
#' @param lead_diff Value to start diff calc
#' @param save_output save the output?
#' @param folder_out output folder relative to recording home
#'
#' @return
#' @export
#'
#' @examples
#' r <- get_recording("NIR_ABh_Puriya", fps = 25)
#' v <- get_raw_view(r, "Central", "", "Sitar")
get_raw_view <- function(recording, vid, direct, inst, lead_diff = 0,
                         folder_out = "Raw", save_output = FALSE) {
  fn_cpts <- c(recording$stem, vid, direct, 'NS', inst)
  fn <- paste0(paste(fn_cpts[fn_cpts != ""], collapse = "_"), ".csv")
  data_file_name <- file.path(recording$data_path, fn)
  message("Loading ", data_file_name)
  stopifnot(file.exists(data_file_name))

  df <- read.csv(data_file_name, colClasses = "numeric")
  first_col <- "Frame"
  colnames(df)[1] <- first_col
  data_points <- unique(sapply(strsplit(colnames(df), "_"), function(x) x[1]))[-1]
  x_colnames <- paste0(data_points, "_x")
  y_colnames <- paste0(data_points, "_y")

  # Add a time column
  df <- cbind(df[1], Time = df[[1]] / recording$fps, df[-1])

  # Add a displacement column
  dx <- as.data.frame(lapply(df[x_colnames], function(x) c(lead_diff, diff(x))))
  dy <- as.data.frame(lapply(df[y_colnames], function(x) c(lead_diff, diff(x))))
  disp <- sqrt(dx^2 + dy^2)
  colnames(disp) <- paste0(data_points, "_d")
  df <- cbind(df, disp)
  selected_cols <- c(first_col, "Time", rbind(x_colnames, y_colnames, colnames(disp)))
  df <- df[selected_cols]

  if (save_output) {
    out_folder <- file.path(recording$data_home, folder_out)
    if (!dir.exists(out_folder)) dir.create(out_folder)
    out_file_name <- file.path(out_folder, paste0(recording$stem, "_", vid , "_", inst, '_RAW.csv'))
    write.csv(df, out_file_name, row.names=FALSE)
  }

  l <- list(df = df, vid = vid, direct = direct,
       inst = inst, recording = recording)
  class(l) <- c("RawView", "View")

  invisible(l)
}


#' Creates time reference and displacement from raw csv optflow data
#'
#' Used to loads OptFlow data when no video camera specified in filename.
#'
#' @param recording object
#' @param vid camera
#' @param direct direction
#' @param inst instrument
#' @param save_output save the output?
#' @param folder_out output folder relative to recording home
#'
#' @return
#' @export
#'
#' @examples
#' r <- get_recording("NIR_DBh_Malhar_2Gats", fps = 25)
#' rov <- get_raw_optflow_view(r, "" ,"", "Guitar")
#' pov <- get_processed_view(rov)
#' fv1 <- apply_filter_sgolay(pov, c("Head"), n=19, p=4)
#'
get_raw_optflow_view <- function(recording, vid, direct, inst,
                         folder_out = "Raw", save_output = TRUE) {
  fn_cpts <- c(recording$stem, 'OptFlow', vid, direct, inst)
  fn <- paste0(paste(fn_cpts[fn_cpts != ""], collapse = "_"), ".csv")
  data_file_name <- file.path(recording$data_path, fn)
  message("Loading ", data_file_name)
  stopifnot(file.exists(data_file_name))

  df <- read.csv(data_file_name, colClasses = "numeric")
  colnames(df) <- c("Frame", "Time", "Head_x", "Head_y")

  # Add a displacement column
  dx <- c(NA, diff(df[["Head_x"]]))
  dy <- c(NA, diff(df[["Head_y"]]))
  disp <- sqrt(dx^2 + dy^2)
  df <- cbind(df, 'Head_d' = disp)

  if (save_output) {
    out_folder <- file.path(recording$data_home, folder_out)
    if (!dir.exists(out_folder)) dir.create(out_folder)
    out_file_name <- file.path(out_folder, paste0(recording$stem, "_", inst, '_RAW.csv'))
    write.csv(df, out_file_name, row.names=FALSE)
  }

  l <- list(df = df, vid = "", direct = direct,
            inst = inst, recording = recording)
  class(l) <- c("OptFlowView", "RawView", "View")

  invisible(l)
}


#' Get views from a recording
#'
#' @param recording
#'
#' @return named list of views
#' @export
#'
#' @examples
#' r <- get_recording("NIR_ABh_Puriya", fps = 25)
#' v_list <- get_raw_views(r)
get_raw_views <- function(recording) {

  # Identify view files
  is_view_file <- grepl(
    paste0("^", recording$stem, ".*_NS.*\\.csv"),
    recording$data_files)
  view_files <- file.path(recording$data_path, recording$data_files[is_view_file])

  output_views <- list()
  for (fil in view_files) {
    tail_str <- substr(basename(fil), nchar(recording$stem) + 2, nchar(basename(fil)) - 4)
    tail_lead <- sub("^(.*)_NS_.*", "\\1", tail_str)
    inst <- sub("^.*_NS_(.*)", "\\1", tail_str)

    tail_cpts <- strsplit(tail_lead, "_")[[1]]
    if (length(tail_cpts) == 2) {
      vid <- tail_cpts[1]
      direct <- tail_cpts[2]
      id <- paste(vid, direct, inst, sep = "_")
    } else if (length(tail_cpts) == 1) {
      vid <- tail_cpts[1]
      direct <- ""
      id <- paste(vid, inst, sep = "_")
    } else stop("Unrecognised file format")

    output_views[[id]] <- get_raw_view(recording, vid, direct, inst)
  }


  output_views
}


#' Get processed view from NS video data
#'
#' Normalises and interpolates missing data in the view.
#'
#' @param rv RawView object
#' @param save_output
#' @param folder_out
#' @param lead_diff
#'
#' @return ProcessedView object
#' @export
#'
#' @examples
#' r <- get_recording("NIR_ABh_Puriya", fps = 25)
#' rv <- get_raw_view(r, "Central", "", "Sitar")
#' pv <- get_processed_view(rv)
get_processed_view <- function(rv, folder_out = "Normalized", lead_diff = 0,
                               save_output = FALSE) {
  stopifnot("RawView" %in% class(rv))

  first_cols <- colnames(rv$df)[1:2]
  df <- rv$df
  data_points <- unique(sapply(strsplit(colnames(df), "_"), function(x) x[1]))[-(1:2)]
  x_colnames <- paste0(data_points, "_x")
  y_colnames <- paste0(data_points, "_y")
  # selected_cols <- c(first_cols, x_colnames, y_colnames)
  # df <- df[selected_cols]

  # If there is a Head data_point we need to remove linear drift
  if ("Head" %in% data_points) {
    fit_head_x <- lm(Head_x ~ Time, data = df, na.action = na.exclude)
    fit_head_y <- lm(Head_y ~ Time, data = df, na.action = na.exclude)
    df[['Head_x']] <- as.numeric(residuals(fit_head_x))
    df[['Head_y']] <- as.numeric(residuals(fit_head_y))
    message("Removed linear trend in Head data point")
  }

  # Split dataframe into x- and y- columns and determine which dimension has the larger extent
  dfx <- df[x_colnames]
  dfy <- df[y_colnames]

  max_x <- max(dfx, na.rm = TRUE)
  min_x <- min(dfx, na.rm = TRUE)
  max_y <- max(dfy, na.rm = TRUE)
  min_y <- min(dfy, na.rm = TRUE)

  size <- max((max_x - min_x), (max_y - min_y))

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

  # Recompute a displacement column
  dx <- as.data.frame(lapply(df_norm[x_colnames], function(x) c(lead_diff, diff(x))))
  dy <- as.data.frame(lapply(df_norm[y_colnames], function(x) c(lead_diff, diff(x))))
  disp <- sqrt(dx^2 + dy^2)
  colnames(disp) <- paste0(data_points, "_d")
  df_norm <- cbind(df_norm, disp)
  selected_cols <- c(first_cols, rbind(x_colnames, y_colnames, colnames(disp)))
  df_norm <- df_norm[selected_cols]

  if (save_output) {
    out_folder <- file.path(rv$recording$data_home, folder_out)
    if (!dir.exists(out_folder)) dir.create(out_folder)
    out_file_name <- file.path(out_folder, paste0(rv$recording$stem, "_", rv$vid , "_", rv$inst, '_NORM.csv'))
    write.csv(df_norm, out_file_name, row.names=FALSE)
  }
  l <- list(df = df_norm, vid = rv$vid, direct = rv$direct,
            inst = rv$inst, recording = rv$recording)
  class(l) <- c("ProcessedView", "View")

  invisible(l)
}


#' Apply a Savitzky-Golay filter to a view
#'
#' @param view View object
#' @param data_points
#' @param n window size
#' @param p poly order
#' @param save_output
#' @param sig_filter S3 filter object from signals package
#' @param folder_out
#'
#' @export
#' @return
#'
#' @examples
#' r <- get_recording("NIR_ABh_Puriya", fps = 25)
#' rv <- get_raw_view(r, "Central", "", "Sitar")
#' pv <- get_processed_view(rv)
#'
#' set.seed(1)
#' fv1 <- apply_filter_sgolay(pv, c("Nose", "RWrist", "LWrist"), n = 19, p = 4)
#' fv2 <- apply_filter_sgolay(pv, c("Nose", "RWrist", "LWrist"), n = 41, p = 3)
#'
#' set.seed(1) # to reproduce with S3 filter object
#' fv3 <- apply_filter(pv, c("Nose", "RWrist", "LWrist"), signal::sgolay(4, 19))
apply_filter_sgolay <- function(view, data_points, n, p, folder_out = "Filtered", save_output = FALSE) {
  apply_filter(view, data_points, signal::sgolay(p, n), folder_out, save_output)
}

#' @export
apply_filter <- function(view, data_points, sig_filter, folder_out = "Filtered", save_output = FALSE) {
  stopifnot("ProcessedView" %in% class(view))

  df_norm <- view$df
  first_cols <- colnames(df_norm[1:2])
  x_colnames <- paste0(data_points, "_x")
  y_colnames <- paste0(data_points, "_y")
  d_colnames <- paste0(data_points, "_d")
  df_selected <- df_norm[c(first_cols, rbind(x_colnames, y_colnames, d_colnames))]

    # Apply filter
  df_filt <- df_selected
  for (cn in c(x_colnames, y_colnames, d_colnames)) {
    df_filt[[cn]] <- signal::filter(sig_filter, df_selected[[cn]])
  }

  # Save version
  if (save_output) {
    out_folder <- file.path(view$recording$data_home, folder_out)
    if (!dir.exists(out_folder)) dir.create(out_folder)
    out_file_name <- file.path(out_folder,
      paste0(view$recording$stem, "_", view$vid, "_", view$inst, '_SEL_', window_size, '_', poly_order,'.csv')
    )
    write.csv(df_filt, out_file_name, row.names=FALSE)
  }
  l <- list(df = df_filt, vid = view$vid, direct = view$direct,
            inst = view$inst, recording = view$recording)
  class(l) <- c("FilteredView", "View")

  invisible(l)
}


#' Get the data points held in a view
#'
#' @param obj view object
#'
#' @return character vector
#' @export
#'
#' @examples
#' r <- get_recording("NIR_ABh_Puriya", fps = 25)
#' rv <- get_raw_view(r, "Central", "", "Sitar")
#' get_data_points(rv)
get_data_points <- function(obj) {
  stopifnot("View" %in% class(obj))
  unique(sapply(strsplit(colnames(obj$df), "_"), function(x) x[1]))[-(1:2)]
}


#' Get joined view from multiple views from the same recording
#'
#' @param l named list of view objects
#' @param save_output
#' @param folder_out
#' @param lead_diff
#'
#' @return JoinedView object
#' @export
#'
#' @examples
#' r <- get_recording("NIR_ABh_Puriya", fps = 25)
#' rv_list <- get_raw_views(r)
#' jv <- get_joined_view(rv_list)
#' plot(jv, columns = c("LEar_x_Central_Sitar", "LEar_x_Central_Tabla"), yax.flip=T)
get_joined_view <- function(l, folder_out = "Joined", save_output = FALSE) {
  stopifnot(all(sapply(l, function(x) "View" %in% class(x))))

  join_pair <- function(x, y) {
    dplyr::inner_join(l[[x]]$df, l[[y]]$df, by = c("Frame", "Time"),
    suffix = paste0("_", c(x, y)), copy = TRUE)
  }

  joined_df <- Reduce(join_pair, names(l))

  if (save_output) {
    out_folder <- file.path(l[[1]]$recording$data_home, folder_out)
    if (!dir.exists(out_folder)) dir.create(out_folder)
    out_file_name <- file.path(out_folder, paste0(l[[1]]$recording$stem, "_", l[[1]]$vid , "_", l[[1]]$inst, '_JOINED.csv'))
    write.csv(joined_df, out_file_name, row.names=FALSE)
  }
  output_list <- list(df = joined_df, vid = "", direct = "", inst = "",
                      recording = l[[1]]$recording)
  class(output_list) <- c("JoinedView", class(l[[1]]))

  invisible(output_list)
}
