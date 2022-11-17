# Disgnostic plots of S3 objects

#' Plot a OnsetsSelected S3 object
#'
#' @param obj
#' @param columns
#' @param ... passed to plot.zoo
#'
#' @return
#'
#' @examples
#' r <- get_recording("NIR_ABh_Puriya", fps = 25)
#' o <- get_onsets_selected_data(r)
#' plot(o)
#' @exportS3Method
plot.OnsetsSelected <- function(obj, column="Inst.Peak", ...) {
  zoo_list <- lapply(obj, function(x) zoo::zoo(x[[column]], order.by = as.numeric(rownames(x))))
  z <- do.call(merge, zoo_list)
  plot(z, xlab = "Row Number", main = paste("OnsetsSelected", column), ...)
}


#' Plot a Metre S3 object
#'
#' @param obj
#'
#' @return
#'
#' @examples
#' r <- get_recording("NIR_ABh_Puriya", fps = 25)
#' m <- get_metre_data(r)
#' plot(m)
#' @exportS3Method
plot.Metre <- function(obj, ...) {
  zoo_list <- lapply(obj, function(x) zoo::zoo(diff(x$Time), order.by = x$Time))
  z <- do.call(merge, zoo_list)
  if (is.null(ncol(z))) {
    plot(z, yax.flip = TRUE, xlab = "Time / s", ylab = "", main = "Metre Object - Time Between Cycles", ...)
  } else {
    plot(z, yax.flip = TRUE, xlab = "Time / s", main = "Metre Object - Time Between Cycles", ...)

  }
}


#' Plot a View S3 object
#'
#' @param obj
#' @param columns
#' @param ... passed to plot.zoo
#'
#' @return
#'
#' @examples
#' r <- get_recording("NIR_ABh_Puriya", fps = 25)
#' v <- get_raw_view(r, "Central", "", "Sitar")
#' plot(v, columns = "LEar_x")
#' @exportS3Method
plot.View <- function(obj, columns=NULL, maxpts = 1000, ...) {

  # Restrict points and columns to plot
  columns <- if (is.null(columns)) seq_len(min(ncol(obj$df), 11))[-1] else c("Time", columns)
  sp <- if (nrow(obj$df) > maxpts) sample(nrow(obj$df), maxpts) else seq_len(nrow(obj$df))

  df <- obj$df[sp, columns, drop = FALSE]
  df <- df[, colSums(is.na(df)) < (nrow(df) - 1), drop=FALSE] # more than one point
  zoo_list <- lapply(df[-1], function(x) zoo::zoo(x, order.by = df$Time))
  z <- do.call(merge, zoo_list)

  title <- c(obj$recording$stem, obj$vid, obj$direct, obj$inst)
  title <- paste(title[title != ""], collapse="_")
  if (is.null(ncol(z))) {
    plot(z, xlab = "Time / s", ylab = columns[-1], main = paste(class(obj)[1], "for", title), ...)
  } else {
    plot(z, xlab = "Time / s", main = paste(class(obj)[1], "for", title), ...)
  }
}
