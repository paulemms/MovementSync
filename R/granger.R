# functions to assess Granger causality

# apply grangertest to each Tier in spliced_df


#' Granger causality tests applied to a SplicedView
#'
#' @param obj SplicedView object
#' @param var1
#' @param var2
#' @param splicing_df
#' @param lag in seconds
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
#'
granger_test <- function(obj, var1, var2, lag = 1) {
  stopifnot("JoinedView" %in% class(obj))

  df <- obj$df

  df <- dplyr::select(df, Tier, !!var1, !!var2)
  df <- dplyr::group_by(df, Tier)

  order <- lag * obj$recording$fps
  l1 <- dplyr::group_map(df, ~ lmtest::grangertest(.[[var1]], .[[var2]], order = order))
  l2 <- dplyr::group_map(df, ~ lmtest::grangertest(.[[var2]], .[[var1]], order = order))

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
  df <- dplyr::inner_join(df, splicing_df[c('Tier', 'Centre')], by = 'Tier')
  df$Test <- paste(df$Var1, df$Var2, sep = ' <- \n')

  ggplot2::ggplot(df) +
    ggplot2::geom_col(ggplot2::aes(x = Centre, y = P_Value), fill = 'black') +
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
  wide_df <- dplyr::mutate(wide_df, Value = if_else(
      Nose_x_Central_Sitar < sig_level | Nose_x_Central_Tabla < sig_level,
      log10(Nose_x_Central_Tabla/Nose_x_Central_Sitar), NA_real_))
  wide_df <- select(wide_df, Centre, Value)

  ggplot2::ggplot(wide_df) +
    ggplot2::geom_segment(colour="black", aes(x=Centre, xend=Centre, y=0, yend=Value),
                 arrow = arrow(length = unit(0.3, "cm"), type = "closed")) +
    ggplot2::labs(title = "Influence Diagram", subtitle = obj$recording$stem) +
    ggplot2::xlab("Time / min:sec") +
    ggplot2::ylab("-log10(P_Value) difference if one significant") +
    ggplot2::scale_x_time(labels = function(l) strftime(l, '%M:%S')) +
    annotate("text", label = unique(df$Var1)[1], x=max(df$Centre)/2, y=max(-log10(df$P_Value))) +
    annotate("text", label = unique(df$Var1)[2], x=max(df$Centre)/2, y=min(log10(df$P_Value)))
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
#' g <- granger_test(sv, "Nose_x_Central_Sitar", "Nose_x_Central_Tabla")
#' d <- get_duration_annotation_data(r)
#' map_to_granger_test(d, g, "Influence T>S", "Influence S>T")
map_to_granger_test <- function(d, g, influence1, influence2) {

  d <- dplyr::mutate(d, Test = dplyr::case_when(
        Tier == !!influence1 ~ paste0(!!g$var1, " <- \n", !!g$var2),
        Tier == !!influence2 ~ paste0(!!g$var2, " <- \n", !!g$var1)
      ))

  d
}
