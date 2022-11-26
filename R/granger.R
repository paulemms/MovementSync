# functions to assess Granger causality

#' Granger causality tests applied to a SplicedView
#'
#' @param obj SplicedView object
#' @param var1
#' @param var2
#' @param lag in seconds
#' @param granger_fn
#'
#' @return
#' @export
#'
#' @examples
#'
#' r1 <- get_recording("NIR_ABh_Puriya", fps = 25)
#' rv_list <- get_raw_views(r1)
#' pv_list <- lapply(rv_list, get_processed_view)
#' get_data_points(pv_list$Central_Sitar)
#' fv_list <- lapply(pv_list, apply_filter_sgolay, data_points = "Nose", n = 41, p = 3)
#' jv <- get_joined_view(fv_list)
#' jv_sub <- subset(jv, Time <= 1*60)
#' splicing_df <- splice_time(jv_sub, win_size = 5, step_size = 0.5)
#' sv <- get_spliced_view(jv_sub, splicing_df)
#' granger_test(sv, "Nose_x_Central_Sitar", "Nose_x_Central_Tabla")

granger_test <- function(obj, var1, var2, lag = 1, granger_fn = ms_grangertest2) {
  stopifnot("SplicedView" %in% class(obj))

  df <- obj$df
  order <- lag * obj$recording$fps
  df <- dplyr::select(df, Tier, !!var1, !!var2)
  df <- dplyr::group_by(df, Tier)
  n_df_group <- dplyr::pull(dplyr::summarise(df, n = dplyr::n()), "n")

  if (any(n_df_group <= 1)) {
    stop("There must be more than one data point in all time slices")
  }

  l1 <- dplyr::group_map(df, ~ granger_fn(.[[var1]], .[[var2]], order = order))
  l2 <- dplyr::group_map(df, ~ granger_fn(.[[var2]], .[[var1]], order = order))

  df1 <- as.data.frame(t(sapply(l1, function(x) as.numeric(x[2,]))))
  df2 <- as.data.frame(t(sapply(l2, function(x) as.numeric(x[2,]))))
  colnames(df1) <- c("Res.Df", "Df", "F", "P_Value")
  colnames(df2) <- c("Res.Df", "Df", "F", "P_Value")
  df1_add <- data.frame(Tier = unique(df$Tier), Var1 = var1, Var2 = var2)
  df2_add <- data.frame(Tier = unique(df$Tier), Var1 = var2, Var2 = var1)
  output_df <- dplyr::bind_rows(dplyr::bind_cols(df1, df1_add),
                                dplyr::bind_cols(df2, df2_add))


  l <- list(df = output_df, var1 = var1, var2 = var2,
            recording = obj$recording, order = order)
  class(l) <- "GrangerTime"

  l
}


#' Plot a Granger S3 object
#'
#' @param obj
#' @param columns
#' @param maxpts
#'
#' @return
#' @exportS3Method
#'
#' @examples
#' r1 <- get_recording("NIR_ABh_Puriya", fps = 25)
#' fv_list <- get_filtered_views(r1, data_points = "Nose", n = 41, p = 3)
#' jv <- get_joined_view(fv_list)
#' jv_sub <- subset(jv, Time <= 1*60)
#' splicing_df <- splice_time(jv_sub, win_size = 3, step_size = 0.5)
#' sv <- get_spliced_view(jv_sub, splicing_df)
#' g <- granger_test(sv, "Nose_x_Central_Sitar", "Nose_x_Central_Tabla", lag = 3/25) # small order
#' autoplot(g, splicing_df)
#'
#' # larger window, larger lag
#' splicing_df <- splice_time(jv_sub, win_size = 10, step_size = 1)
#' sv <- get_spliced_view(jv_sub, splicing_df)
#' g <- granger_test(sv, "Nose_x_Central_Sitar", "Nose_x_Central_Tabla", lag = 1)
#' autoplot(g, splicing_df)

autoplot.GrangerTime <- function(obj, splicing_df = splicing_df, lev_sig = 0.05) {

  df <- obj$df

  title <- paste0(class(obj)[1], ": Lagged at ", obj$order / obj$recording$fps, "s")
  splicing_df$Centre <- (splicing_df$Start + splicing_df$End) / 2
  splicing_df$Width <- splicing_df$End - splicing_df$Start
  df <- dplyr::inner_join(df, splicing_df[c('Tier', 'Centre', 'Width')], by = 'Tier')
  df$Test <- paste(df$Var1, df$Var2, sep = ' <- \n')

  ggplot2::ggplot(df) +
    ggplot2::geom_col(ggplot2::aes(x = Centre, y = P_Value), width = max(df$Width)/10, fill = 'black') +
    ggplot2::geom_hline(yintercept = lev_sig, colour = 'blue') +
    ggplot2::labs(title = title, subtitle = obj$recording$stem) +
    ggplot2::xlab("Time / min:sec") +
    ggplot2::scale_x_time(labels = function(l) strftime(l, '%M:%S')) +
    ggplot2::facet_grid(rows = ggplot2::vars(Test))
}


#' Plot influence diagram from a GrangerTest object
#'
#' Shows arrows showing causality direction.
#' @param obj GrangerTest object
#' @param splicing_df
#' @param lev_sig
#'
#' @return ggplot object
#' @export
#'
#' @examples
#' r1 <- get_recording("NIR_ABh_Puriya", fps = 25)
#' fv_list <- get_filtered_views(r1, data_points = "Nose", n = 41, p = 3)
#' jv <- get_joined_view(fv_list)
#' jv_sub <- subset(jv, Time <= 1*60)
#' splicing_df <- splice_time(jv_sub, win_size = 3, step_size = 0.5)
#' sv <- get_spliced_view(jv_sub, splicing_df)
#' g <- granger_test(sv, "Nose_x_Central_Sitar", "Nose_x_Central_Tabla", lag = 3/25)
#'
#' plot_influence_diagram(g, splicing_df)
#'
#' plot_influence_diagram(g, splicing_df) +
#' autolayer(d1, '(Tier == "Influence S>T" | Tier == "Influence T>S") & Out < 60',
#'           fill_col = "Tier")

plot_influence_diagram <- function(obj, splicing_df = splicing_df, lev_sig = 0.05) {

  df <- obj$df

  splicing_df$Centre <- (splicing_df$Start + splicing_df$End) / 2
  df <- dplyr::inner_join(df, splicing_df[c('Tier', 'Centre')], by = 'Tier')
  x <- df[c("Var1", "Centre", "P_Value")]
  wide_df <- tidyr::pivot_wider(x, names_from = "Var1", values_from = "P_Value")
  wide_df <- dplyr::mutate(wide_df, Value = dplyr::if_else(
      Nose_x_Central_Sitar < lev_sig | Nose_x_Central_Tabla < lev_sig,
      log10(Nose_x_Central_Tabla/Nose_x_Central_Sitar), NA_real_))
  wide_df <- dplyr::select(wide_df, Centre, Value)

  ggplot2::ggplot(wide_df) +
    ggplot2::geom_segment(colour="black", ggplot2::aes(x=Centre, xend=Centre, y=0, yend=Value),
                 arrow = ggplot2::arrow(length = ggplot2::unit(0.3, "cm"), type = "closed")) +
    ggplot2::labs(title = "Influence Diagram", subtitle = obj$recording$stem) +
    ggplot2::xlab("Time / min:sec") +
    ggplot2::ylab("-log10(P_Value) difference if one significant") +
    ggplot2::scale_x_time(labels = function(l) strftime(l, '%M:%S')) +
    ggplot2::annotate("text", label = unique(df$Var1)[1], x=max(df$Centre)/2, y=max(-log10(df$P_Value))) +
    ggplot2::annotate("text", label = unique(df$Var1)[2], x=max(df$Centre)/2, y=min(log10(df$P_Value)))
}


#' Map duration object comments to a Granger Test object
#'
#' @param d DurationObject
#' @param g GrangerTest object
#' @param influence1 Comment X>Y string in the Granger Test of Y~X i.e. X causes Y
#' @param influence2 Comment X>Y string in the Granger Test of Y~X i.e. X causes Y
#'
#' @return modified Duration object
#' @export
#'
#' @examples
#' r <- get_recording("NIR_ABh_Puriya", fps = 25)
#' fv_list <- get_filtered_views(r, data_points = "Nose", n = 41, p = 3)
#' jv <- get_joined_view(fv_list)
#' jv_sub <- subset(jv, Time <= 1*60)
#' splicing_df <- splice_time(jv_sub, win_size = 5, step_size = 0.5)
#' sv <- get_spliced_view(jv_sub, splicing_df)
#' g <- granger_test(sv, "Nose_x_Central_Sitar", "Nose_x_Central_Tabla", lag = 1/25)
#' d <- get_duration_annotation_data(r)
#' map_to_granger_test(d, g, "Influence T>S", "Influence S>T")

map_to_granger_test <- function(d, g, influence1, influence2) {

  d <- dplyr::mutate(d, Test = dplyr::case_when(
        Tier == !!influence1 ~ paste0(!!g$var1, " <- \n", !!g$var2),
        Tier == !!influence2 ~ paste0(!!g$var2, " <- \n", !!g$var1)
      ))

  d
}


#' Get Granger Causality interactions
#'
#' @param sv
#' @param columns
#' @param sig_level
#' @param lag
#' @param granger_fn
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#' r <- get_recording("NIR_ABh_Puriya", fps = 25)
#' fv_list <- get_filtered_views(r, "Nose", n = 41, p = 3)
#' jv <- get_joined_view(fv_list)
#' jv_sub <- subset(jv, Time <= 5*60)
#' l <- list(a = c(0, 300), b = c(300, 600), c = c(600, 900))
#' splicing_df <- splice_time(l)
#' sv <- get_spliced_view(jv_sub, splicing_df)
#' get_granger_interactions(sv, c("Nose_x_Central_Sitar", "Nose_x_Central_Tabla"), lag = 1/25)
get_granger_interactions <- function(sv, columns, sig_level = 0.05, lag = 1,
                                     granger_fn = ms_grangertest2) {
  stopifnot(all(c("SplicedView", "JoinedView") %in% class(sv)))

  # Calculate granger tests for all combinations of columns
  a <- combn(columns, 2)

  gc_list <- list()
  for (j in seq_len(ncol(a))) {
    var1 <- a[1, j]
    var2 <- a[2, j]
    g_test <- paste0(var1, " <--> ", var2)
    message("Calculating Granger Test: ", g_test)
    gc_list[[g_test]] <- granger_test(sv, var1, var2, lag = lag, granger_fn = granger_fn)
  }
  l <- list(gc_list = gc_list, sig_level = sig_level, lag = lag)
  class(l) <- "GrangerInteraction"

  invisible(l)
}


#' Plot network diagram of Granger Causalities
#'
#' @param obj
#' @param mfrow
#' @param mar
#' @param oma
#' @param ... passed through to [plot.igraph()]
#'
#' @return
#' @export
#'
#' @examples
#' r <- get_recording("NIR_ABh_Puriya", fps = 25)
#' fv_list <- get_filtered_views(r, "Nose", n = 41, p = 3)
#' jv <- get_joined_view(fv_list)
#' jv <- subset(jv, Time <= 5*60)
#' l <- list(a = c(0, 100), b = c(100, 200), c = c(200, 300))
#' splicing_df <- splice_time(l)
#' sv <- get_spliced_view(jv, splicing_df)
#' gi <- get_granger_interactions(sv, c("Nose_x_Central_Sitar", "Nose_x_Central_Tabla"), lag = 1/25)
#' plot(gi)
plot.GrangerInteraction <- function(obj, mfrow = NULL, mar = c(1, 1, 1, 1),
                                    oma = c(1, 1, 1, 1), ...) {

  gc_list <- obj$gc_list

  # Single data frame of P-values
  df <- dplyr::bind_rows(lapply(gc_list, function(x) x$df), .id = "Test")
  df$mlog10pv <- ifelse(df$P_Value < obj$sig_level, -log10(df$P_Value), NA_real_)

  if (is.null(mfrow)) {
    num_tiers <- length(unique(df$Tier))
    mfrow <- c(num_tiers %/% 4 + (num_tiers %% 4 > 0), min(num_tiers, 4))
  }

  old_params <- par(mfrow=mfrow, oma=oma, mar=mar)

  # Extract the node names
  nodes <- data.frame(id = sapply(strsplit(unique(df$Var1), "_"), function(x) x[4]))

  # Loop through Tiers
  for (tier in unique(df$Tier)) {

    # Extract the links and removing missings
    splice_df <- dplyr::filter(df, Tier == !!tier)
    splice_df$Var1 <- sapply(strsplit(splice_df$Var1, "_"), function(x) x[4])
    splice_df$Var2 <- sapply(strsplit(splice_df$Var2, "_"), function(x) x[4])
    links <- data.frame(from = splice_df$Var2, to = splice_df$Var1, x = splice_df$mlog10pv)
    links <- links[!is.na(links$x),,drop = FALSE]
    net <- igraph::graph_from_data_frame(d=links, vertices=nodes, directed=T)

    l <- igraph::layout_in_circle(net)
    igraph::V(net)$size <- max(nchar(nodes$id)) * 10
    igraph::V(net)$label.cex <- 1.25
    if (nrow(links) > 0) {
      igraph::E(net)$color <- "grey"
      igraph::E(net)$label <- round(igraph::E(net)$x, 1)
      igraph::E(net)$label.color <- "red"
    }
    plot(net, layout=l, edge.curved=.4, main=tier, ...)
  }

  main_title <- paste0("Recording: ", obj$gc_list[[1]]$recording$stem, ", Lag = ",
                       obj$lag, "s")
  mtext(text = main_title, side = 1, line = -1, outer = TRUE)
  par(old_params)

  invisible(df)
}


#' Test for Granger Causality
#'
#' Faster implementation of the vector version of  [lmtest::grangertest()]
#' which uses a vectorised lag operation.
#'
#' @param x
#' @param y
#' @param order
#' @param na.action
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#' data(ChickEgg, package = "lmtest")
#' ms_grangertest1(ChickEgg, order = 3)

ms_grangertest1 <- function(x, y, order = 1, na.action = na.omit, ...) {
  ## either x is a 2-column time series
  ## or x and y are univariate time series
  if((NCOL(x) == 2) && missing(y)) {
    xnam <- colnames(x)[1]
    ynam <- colnames(x)[2]
    x <- zoo::as.zoo(x)
    y <- x[,2]
    x <- x[,1]
  } else {
    xnam <- deparse(substitute(x))
    ynam <- deparse(substitute(y))
    x <- zoo::as.zoo(x)
    y <- zoo::as.zoo(y)
    stopifnot((NCOL(x) == 1), (NCOL(y) == 1))
  }

  ## compute lagged observations
  #lagX <- do.call("merge", lapply(1:order, function(k) stats::lag(x, -k)))
  #lagY <- do.call("merge", lapply(1:order, function(k) stats::lag(y, -k)))
  lagX <- stats::lag(x, -seq_len(order))
  lagY <- stats::lag(y, -seq_len(order))

  ## collect series, handle NAs and separate results again
  all <- zoo::merge.zoo(x, y, lagX, lagY)
  colnames(all) <- c("x", "y", paste("x", 1:order, sep = "_"), paste("y", 1:order, sep = "_"))
  all <- na.action(all)
  y <- as.vector(all[,2])
  lagX <- as.matrix(all[,(1:order + 2)])
  lagY <- as.matrix(all[,(1:order + 2 + order)])

  ## fit full model
  fm <- lm(y ~ lagY + lagX)

  ## compare models with waldtest
  rval <- lmtest::waldtest(fm, 2, ...)

  ## adapt annotation
  attr(rval, "heading") <- c("Granger causality test\n",
                             paste("Model 1: ", ynam, " ~ ", "Lags(", ynam, ", 1:", order, ") + Lags(", xnam, ", 1:", order,
                                   ")\nModel 2: ", ynam, " ~ ", "Lags(", ynam, ", 1:", order, ")", sep = ""))

  return(rval)
}


#' Test for Granger Causality
#'
#' Faster implementation of the vector version of  [lmtest::grangertest()]. The
#' function assumes time series always have the same start date, which is true
#' for the data in this package.
#'
#' @param x
#' @param y
#' @param order
#' @param na.action
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#' data(ChickEgg, package = "lmtest")
#' ms_grangertest2(ChickEgg, order = 3)

ms_grangertest2 <- function(x, y, order = 1, na.action = na.omit, ...) {
  ## either x is a 2-column time series
  ## or x and y are univariate time series
  if((NCOL(x) == 2) && missing(y)) {
    xnam <- colnames(x)[1]
    ynam <- colnames(x)[2]
    y <- x[,2]
    x <- x[,1]
  } else {
    xnam <- deparse(substitute(x))
    ynam <- deparse(substitute(y))
    stopifnot((NCOL(x) == 1), (NCOL(y) == 1))
  }

  ## compute lagged observations
  x1 <- c(rep(NA, order - 1), x)
  y1 <- c(rep(NA, order - 1), y)
  lagX <- embed(x1, order + 1)[,-1,drop=FALSE]
  lagY <- embed(y1, order + 1)
  y <- lagY[,1]
  lagY <- lagY[,-1,drop=FALSE]

  ## fit full model
  fm <- lm(y ~ lagY + lagX, na.action = na.action)

  ## compare models with waldtest
  rval <- lmtest::waldtest(fm, 2, ...)

  ## adapt annotation
  attr(rval, "heading") <- c("Granger causality test\n",
                             paste("Model 1: ", ynam, " ~ ", "Lags(", ynam, ", 1:", order, ") + Lags(", xnam, ", 1:", order,
                                   ")\nModel 2: ", ynam, " ~ ", "Lags(", ynam, ", 1:", order, ")", sep = ""))

  return(rval)
}

