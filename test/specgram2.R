rm(list=ls())
library(signal)
library(ggplot2)
devtools::load_all()

r2 <- get_recording("NIR_DBh_Malhar_2Gats", fps = 25)
rv_list <- get_raw_views(r2)
pv_list <- lapply(rv_list, get_processed_view)
fv_list <- lapply(pv_list, apply_filter_sgolay, data_points = "RWrist", n = 11, p = 3)

# processed data
pv <- subset(pv_list$SideL_Tabla, column = c("RWrist_x", "RWrist_y"))
specgram_plot(pv)

# filtered data
fv <- subset(fv_list$SideL_Tabla, column = c("RWrist_x", "RWrist_y"))
specgram_plot(fv)

# get duration object
d <- get_duration_annotation_data(r2)

# annotate filtered specgram with In duration data
specgram_plot(fv) + autolayer(d, geom = "vline", nudge_x = -10, size = 3)

# annotate unfiltered specgram with Out duration data
specgram_plot(pv) + autolayer(d, geom = "vline", nudge_x = -10, size = 3, vline_column = "Out")

# bw palette
specgram_plot(pv, window = 200) +
  ggplot2::scale_fill_gradient(low = "white", high = "black") +
  autolayer(d, geom = "vline", nudge_x = -10, size = 3, colour = "cyan")
