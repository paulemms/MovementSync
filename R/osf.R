# functions to get recordings from OSF

get_osf_node <- function() "https://osf.io/w2s3a"

#' List available recordings for movementsync from OSF
#'
#' @return character vector of stem names
#' @export
#'
#' @examples
#' \dontrun{
#' list_osf_recordings()
#' }
list_osf_recordings <- function() {
  node <- get_osf_node()
  movementsync_project <- osfr::osf_retrieve_node(node)
  movementsync_files <- osfr::osf_ls_files(movementsync_project)
  file_names <- basename(movementsync_files$name)
  zip_files <- file_names[grepl('(.*)\\.zip$', file_names)]
  stem_names <- substr(zip_files, 1, nchar(zip_files) - 4)

  stem_names
}


#' Opens movementsync data home page at OSF
#'
#' @export
open_movementsync_data <- function() {
  node <- get_osf_node()
  movementsync_project <- osfr::osf_retrieve_node(node)
  osfr::osf_open(movementsync_project)
}


#' Get movementsync recording from OSF
#'
#' @param stem zip file stem (default is 'Original' which contains all the
#' recordings for the walk-throughs.
#' @param to_dir directory to copy to (default is "~/movementsync/Original").
#' @param overwrite overwriting existing dataset files?
#'
#' @return invisible vector of CSV file names.
#' @export
#'
#' @examples
#' \dontrun{
#' get_osf_recording()
#' }
get_osf_recording <- function(stem = 'Original', to_dir = "~/movementsync/Original",
                            overwrite = FALSE) {

  # Find the zip to download
  node <- get_osf_node()
  movementsync_project <- osfr::osf_retrieve_node(node)
  movementsync_files <- osfr::osf_ls_files(movementsync_project)
  fn <- paste0(stem, '.zip')
  ms_file <- movementsync_files[fn == movementsync_files$name, ]
  if (nrow(ms_file) == 0) stop(paste('Cannot find recording', stem, 'at OSF'))

  # Download it to a temp directory
  message(paste("Downloading recording", stem, '...'))
  downloaded_files <- osfr::osf_download(ms_file, conflicts = 'overwrite',
                                         path = tempdir())

  # Check the files don't already exist locally
  csv_files <- basename(utils::unzip(downloaded_files$local_path[1], list = TRUE)$Name)
  if (!dir.exists(to_dir)) dir.create(to_dir, recursive = TRUE)
  if (any(file.exists(file.path(to_dir, csv_files))) && !overwrite) {
    stop('Dataset not downloaded because it already exists locally.
         Use overwrite = TRUE to copy over existing local dataset.'
    )
  }

  message(paste("Unzipping recording", stem, '...'))
  unzipped_files <- utils::unzip(downloaded_files$local_path[1], exdir = to_dir,
                          junkpaths = TRUE)
  invisible(unzipped_files)
}
