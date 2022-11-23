rm(list=ls())
library(WaveletComp)
devtools::load_all()

r1 <- get_recording("NIR_ABh_Puriya", fps = 25)
rv1 <- get_raw_view(r1, "Central", "", "Sitar")
pv1 <- get_processed_view(rv1)
features <- c("Nose")
fv1 <- apply_filter_sgolay(pv1, data_points = features, n = 41, p = 3)

plot(fv1)

# Subset processed and filtered views to first five minutes
pv2 <- subset(pv1, Time >= 0*60 & Time <= 5*60)
fv2 <- subset(fv1, Time >= 0*60 & Time <= 5*60)

### Power spectrum - single time series

# Unsmoothed
w <- analyze_wavelet(pv2, "Nose_x")
plot_power_spectrum(w, pv2)
summary(w, pv2)

# Smoothed view
w1 <- analyze_wavelet(fv2, "Nose_x")
plot_power_spectrum(w1, fv2)

# Compare to power spectrum for Nose_y
w2 <- analyze_wavelet(fv2, "Nose_y")
plot_power_spectrum(w2, fv2)

### Average power for two series: Nose_x, Nose_y
maximum.level <- 1.001*max(w1$Power.avg, w2$Power.avg)
plot_average_power(w1, fv2, maximum.level = maximum.level)
plot_average_power(w2, fv2, maximum.level = maximum.level, show.siglvl=FALSE)

### Cross Power Spectrum: Nose_x, Nose_y

# Cross wavelet power spectrum of Nose_x and Nose_y
co <- analyze_coherency(fv2, columns = c("Nose_x", "Nose_y"))
plot_cross_spectrum(co, fv2)

# Cross-wavelet average power
plot_average_coherency(co, fv2)

# Cross wavelet coherence
plot_coherence(co, fv2)

# Phase differences at period 0.64s. range [0.5, 0.7]
plot_sel_phases(co, fv2, sel.period = 0.64)
plot_sel_phases(co, fv2, sel.period = NULL, sel.lower = 0.5, sel.upper = 0.7)

# Global image of phase differences
plot_phase_difference(co, fv2)
