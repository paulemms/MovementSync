rm(list = ls())
library(movementsync)
library(gganimate)
library(gifski)
library(dplyr)
library(gapminder)
library(transformr)

r1 <- get_recording("NIR_ABh_Puriya", fps = 25)
rv1 <- get_raw_view(r1, "Central", "", "Sitar")
pv1 <- get_processed_view(rv1)
features <- c("LWrist", "RWrist", "LElbow", "RElbow", "LEye", "REye")
fv1 <- apply_filter_sgolay(pv1, data_points = features, n = 41, p = 3)

maxpts <- 250
x_features <- paste(features, "x", sep = "_")
y_features <- paste(features, "y", sep = "_")

df <- fv1$df[seq_len(maxpts), c("Frame","Time", x_features, y_features), drop=FALSE]
df_list <- list()
for (i in seq_along(features)) {
  df_list[[features[i]]] <- df[c("Time", "Frame", x_features[i], y_features[i])]
  names(df_list[[features[i]]]) <- c("Time", "Frame", "x", "y")
}
df1 <- dplyr::bind_rows(df_list, .id = "Feature")

g <- ggplot2::ggplot(df1, ggplot2::aes(x, y)) +
  ggplot2::geom_point(aes(colour = Feature), alpha = 0.5) +
  scale_size(range = c(2,12)) +
  #ggplot2::facet_wrap(~Feature) +
  labs(title = 'Time: {frame_time}') +
  transition_time(Time) +
  shadow_wake(.3)+
  ease_aes('linear')
a <- animate(g, nframes=maxpts, fps=25)
a

