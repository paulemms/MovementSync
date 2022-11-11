rm(list = ls())
devtools::load_all()
library(gganimate)
library(gifski)
library(dplyr)
library(gapminder)
library(transformr)

# gapminder %>%
#   filter(continent=="Oceania") %>%
#   ggplot(aes(gdpPercap, lifeExp, size=pop, colour = country)) +
#   geom_point(alpha = 0.5, show.legend = TRUE) +
#   scale_size(range = c(2,12)) +
#   # Here comes the gganimate specific bits
#   labs(title = 'Year: {frame_time}', x = 'GDP per capita in USD', y = 'Life Expectancy') +
#   transition_time(year) +
#   shadow_wake(.3)+
#   ease_aes('linear')

r1 <- get_recording("NIR_ABh_Puriya", fps = 25)
rv1 <- get_raw_view(r1, "Central", "", "Sitar")


features <- c("RElbow", "LElbow")
maxpts <- 250
x_features <- paste(features, "x", sep = "_")
y_features <- paste(features, "y", sep = "_")

df <- rv1$df[seq_len(maxpts), c("X","Time", x_features, y_features), drop=FALSE]
df_list <- list()
for (i in seq_along(features)) {
  df_list[[features[i]]] <- df[c("Time", "X", x_features[i], y_features[i])]
  names(df_list[[features[i]]]) <- c("Time", "X", "x", "y")
}
df1 <- dplyr::bind_rows(df_list, .id = "Feature")

g <- ggplot2::ggplot(df1, ggplot2::aes(x, y)) +
  ggplot2::geom_point(alpha = 0.5) +
  scale_size(range = c(2,12)) +
  ggplot2::facet_wrap(~Feature) +
  labs(title = 'Time: {frame_time}') +
  transition_time(Time) +
  shadow_wake(.3)+
  ease_aes('linear')
animate(g, nframes=maxpts, fps=25)
