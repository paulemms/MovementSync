rm(list=ls())
library(signal)
library(ggplot2)
devtools::load_all()

r2 <- get_recording("NIR_DBh_Malhar_2Gats", fps = 25)
rv_list <- get_raw_views(r2)
pv_list <- lapply(rv_list, get_processed_view)
fv_list <- lapply(pv_list, apply_filter_sgolay, data_points = "RWrist", n = 11, p = 3)

# Transient at end
plot(fv_list$SideL_Tabla)

# Subset to remove end transient
SideL_Tabla <- subset(fv_list$SideL_Tabla, Time < 1700)
plot(SideL_Tabla)

a <- SideL_Tabla$df$RWrist_x

jet <- grDevices::colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F",
                                     "yellow", "#FF7F00", "red", "#7F0000"))

sp <- specgram(a, Fs = 25)

plot(sp, col = jet(20), main = "Tabla_134_L RWrist_xz")

image(sp$t, sp$f, 20 * log10(t(abs(sp$S))), col=jet(20))

# ggplot reproduction

df <- expand.grid(X = sp$t, Y = sp$f)
df$Z <- as.numeric(20 * log10(t(abs(sp$S))))

subtitle <- c(SideL_Tabla$recording$stem, SideL_Tabla$vid, SideL_Tabla$direct, SideL_Tabla$inst)
subtitle <- paste(subtitle[subtitle != ""], collapse="_")

ggplot(df, aes(X, Y, fill= Z)) +
  geom_tile() + scale_fill_gradientn(colours = jet(20)) +
  ggplot2::labs(title = "Specgram", subtitle = subtitle) +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0)) +
  theme(legend.position = "none")

# multiple nodes
df <- SideL_Tabla$df
data_point <- c("RWrist_x", "RWrist_y")
sp_list <- lapply(df[data_point], specgram, Fs = 25)
df_list <- list()
for (i in seq_along(data_point)) {
  df_list[[data_point[i]]] <- expand.grid(X = sp_list[[i]]$t, Y = sp_list[[i]]$f)
  df_list[[data_point[i]]]$Z <- as.numeric(20 * log10(t(abs(sp_list[[i]]$S))))
}
long_df <- dplyr::bind_rows(df_list, .id = "DataPoint")
ggplot(long_df, aes(X, Y, fill= Z)) +
  geom_tile() + scale_fill_gradientn(colours = jet(20)) +
  ggplot2::labs(title = "Specgram", subtitle = subtitle) +
  theme(legend.position = "none") +
  ggplot2::xlab("Time / min:sec") +
  ggplot2::ylab("Frequency") +
  ggplot2::scale_x_time(expand = c(0,0), labels = function(l) strftime(l, '%M:%S')) +
  scale_y_continuous(expand = c(0,0)) +
  facet_grid(rows = vars(DataPoint))
