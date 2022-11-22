rm(list=ls())
library(WaveletComp)
library(ggplot)
devtools::load_all()

r1 <- get_recording("NIR_ABh_Puriya", fps = 25)
rv1 <- get_raw_view(r1, "Central", "", "Sitar")
pv1 <- get_processed_view(rv1)
features <- c("Nose")
fv1 <- apply_filter_sgolay(pv1, data_points = features, n = 41, p = 3)

plot(fv1)

pv2 <- subset(pv1, Time >= 0*60 & Time <= 2*60)
fv2 <- subset(fv1, Time >= 0*60 & Time <= 2*60)

w <- analyze_wavelet(pv2, "Nose_x", n.sim = 10, lowerPeriod = 1, upperPeriod = 32,
                     dj = 1/20)
plot_power_spectrum(w, pv2)
abline(v = 10, col = "gray", lty = 1)


time <- w$axis.1
p <- w$axis.2
power <- as.vector(w$Power)
long_df <- expand.grid(p = p, time = time)
long_df$power <- power

cols <- c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F",
  "yellow", "#FF7F00", "red", "#7F0000")
jet <- grDevices::colorRampPalette(cols)

ggplot2::ggplot(long_df) +
  ggplot2::geom_tile(ggplot2::aes(time, p, fill= power)) +
  ggplot2::scale_fill_gradientn(colours = jet(10), trans = 'log2')






