# Tests Wavelet functionality in the package

rm(list=ls())
gc()
if (dev.cur() > 1) dev.off()
library(movementsync)

r1 <- get_recording("NIR_ABh_Puriya", fps = 25)
rv1 <- get_raw_view(r1, "Central", "", "Sitar")
pv1 <- get_processed_view(rv1)
fv1 <- apply_filter_sgolay(pv1, data_points = c("Nose"), n = 41, p = 3)

autoplot(fv1)

# Subset processed and filtered views to first five minutes
pv2 <- subset(pv1, Time >= 0*60 & Time <= 5*60)
fv2 <- subset(fv1, Time >= 0*60 & Time <= 5*60)

### Power spectrum - single time series

# Base R graphical parameters for the plots (see ?par)
old_par <- par(cex.main = 0.95) # Titles smaller for plots

# Unsmoothed
w <- analyze_wavelet(pv2, "Nose_x")
plot_power_spectrum(w, pv2)
summary(w, pv2)

# Smoothed view
w1 <- analyze_wavelet(fv2, "Nose_x")
plot_power_spectrum(w1, fv2)

# Fixed frequency range
w2 <- analyze_wavelet(fv2, "Nose_x", lowerPeriod = 0.16, upperPeriod = 2.56)
plot_power_spectrum(w2, fv2)

# Compare to power spectrum for Nose_y
w2 <- analyze_wavelet(fv2, "Nose_y")
plot_power_spectrum(w2, fv2)

# Plot normalised wavelet energy as a function of time
plot_wt_energy(w1, fv2)
plot_wt_energy(w2, fv2)

# Average power for two series: Nose_x, Nose_y
maximum.level <- 1.001*max(w1$Power.avg, w2$Power.avg)
plot_average_power(w1, fv2, maximum.level = maximum.level)
plot_average_power(w2, fv2, maximum.level = maximum.level, show.siglvl=FALSE)

# Get the locally maximal average power
get_local_max_average_power(w1, fv2)

### Cross Power Spectrum: Nose_x, Nose_y

# Cross wavelet power spectrum of Nose_x and Nose_y
co <- analyze_coherency(fv2, columns = c("Nose_x", "Nose_y"))
plot_cross_spectrum(co, fv2)

# Plot normalised cross wavelet energy as a function of time
plot_cwt_energy(co, fv2)

# Cross-wavelet average power
plot_average_coherency(co, fv2)

# Cross wavelet coherence
plot_coherence(co, fv2)

# Global image of phase differences
plot_phase_difference(co, fv2)

# Phase differences at period 0.64s. range [0.5, 0.7]
sp1 <- plot_sel_phases(co, fv2, sel.period = 0.64)
sp2 <- plot_sel_phases(co, fv2, sel.lower = 0.5, sel.upper = 0.7)

# Summaries of relative phase
summary(sp1)
summary(sp2)
plot_roll_resultant_length(sp2, window_duration = 20, by = 5)

# Add annotation layer
l <- list(a = c(60, 120), b = c(180, 240))
splicing_dfr <- splice_time(l)
plot_roll_resultant_length(sp2, window_duration = 20, by = 5) +
  autolayer(splicing_dfr)
plot_roll_resultant_length(sp2, window_duration = 20, by = 5) +
  autolayer(splicing_dfr, geom = 'vline', nudge_x = -3, size = 3) +
  autolayer(splicing_dfr, geom = 'vline', nudge_x = -3, size = 3, vline_column = 'End')

d1 <- get_duration_annotation_data(r1)
plot_roll_resultant_length(sp2, window_duration = 20, by = 5) +
  autolayer(d1, expr = Tier == "Influence S>T" & Comments == "appreciation" & Out <= 300) +
  ggplot2::scale_fill_discrete(type = 'grey')

# Get the cross spectrum for two musicians
fv_list <- get_filtered_views(r1, data_points = 'Nose', n = 41, p = 3)
jv <- get_joined_view(fv_list)
jv_sub <- subset(jv, Time >= 0*60 & Time <= 5*60)
co2 <- analyze_coherency(jv_sub, columns = c("Nose_x_Central_Sitar", "Nose_x_Central_Tabla"),
                         n.sim = 2)
plot_cross_spectrum(co2, jv_sub)
plot_cwt_energy(co2, jv_sub) +
  autolayer(d1, expr = Tier == "Influence S>T" & Comments == "appreciation" & Out <= 300) +
  ggplot2::scale_fill_discrete(type = 'cyan')

# Reset graphical parameters
par(old_par)

