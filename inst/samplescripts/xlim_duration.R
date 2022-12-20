r <- get_recording("NIR_ABh_Puriya", fps = 25)
d <- get_duration_annotation_data(r)
v <- get_raw_view(r, "Central", "", "Sitar")

autoplot(v, columns = c("LEar_x", "LEar_y")) +
  xlim_duration(d, expr = Tier == "FORM" & substr(Comments, 1, 1) == "J") +
  ggplot2::scale_x_time(labels = function(l) strftime(l, '%M:%S'))

# What we want but with x axis in
autoplot(v, columns = c("LEar_x", "LEar_y")) +
  xlim_duration(d, expr = Tier == "FORM" & substr(Comments, 1, 1) == "J") +
  autolayer(d, expr = Tier == "FORM" & substr(Comments, 1, 1) == "J")

autoplot(v, columns = c("LEar_x", "LEar_y"), time_limits = d,
         expr = Tier == "FORM" & substr(Comments, 1, 1) == "J") +
  autolayer(d, expr = Tier == "FORM" & substr(Comments, 1, 1) == "J")

autoplot(v, columns = c("LEar_x", "LEar_y")) +
  ggplot2::scale_x_time(labels = function(l) strftime(l, '%M:%S')) +
  xlim_duration(d, expr = Tier == "FORM" & substr(Comments, 1, 1) == "J") +
  autolayer(d, expr = Tier == "FORM" & substr(Comments, 1, 1) == "J") +
  ggplot2::scale_x_time(labels = function(l) strftime(l, '%M:%S'))
