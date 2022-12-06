# Test the statistical analysis functions
library(ggplot2)
library(GGally)
library(zoo)
rm(list=ls())
devtools::load_all()

r1 <- get_recording("NIR_ABh_Puriya", fps = 25)
fv1_list <- get_filtered_views(r1, data_points = "Nose", n = 41, p = 3)
jv1 <- get_joined_view(fv1_list)
autoplot(jv1)

# Splices based on Duration object
d1 <- get_duration_annotation_data(r1)
autoplot(d1)

# Mutual look and smile
splicing_duration1_df <- splice_time(
  d1, tier ='INTERACTION', comments = 'Mutual look and smile'
)
splicing_duration1_df
sv_duration_smile <- get_spliced_view(jv1, splicing_df = splicing_duration1_df)
autoplot(sv_duration_smile)

# Mutual head and body movement
splicing_duration2_df <- splice_time(
  d1, tier = 'INTERACTION', comments = 'Mutual head and body movement'
)
splicing_duration2_df
sv_duration_body <- get_spliced_view(jv1, splicing_df = splicing_duration2_df)
autoplot(sv_duration_body)

################################################################################
#
# 1. Compare two sets of segments by generating random samples from each then
#    comparing distribution
#
################################################################################

# Draw 1000 samples from the average power on random segments of a SplicedView
# and compare with another
sample_list <- compare_avg_power2(
  sv_duration_smile, sv_duration_body, 'Smile', 'Body', num_samples = 1000,
  column = "Nose_x_Central_Sitar")

# Summary stats
max(sample_list$Smile$Average_Power, na.rm = TRUE)
max(sample_list$Body$Average_Power, na.rm = TRUE)

# Draw 1000 samples from the average cross power on random segments of a SplicedView
# and compare with another
sample2_list <- compare_avg_cross_power2(
  sv_duration_smile, sv_duration_body, 'Smile', 'Body', num_samples = 1000,
  columns = c("Nose_x_Central_Sitar", "Nose_y_Central_Sitar"))

# Summary stats
max(sample2_list$Smile$Average_Cross_Power, na.rm = TRUE)
max(sample2_list$Body$Average_Cross_Power, na.rm = TRUE)

# Clip splice so segments of fixed lengths in each SplicedView
splicing_clipped1_df <- clip_splice(splicing_duration1_df, duration = 1, location = 'middle')
splicing_clipped2_df <- clip_splice(splicing_duration2_df, duration = 1, location = 'middle')
splicing_clipped1_df
splicing_clipped2_df

# Draw 1000 samples from the average power on random clipped segments
sv_clipped_smile <- get_spliced_view(jv1, splicing_df = splicing_clipped1_df)
sv_clipped_body <- get_spliced_view(jv1, splicing_df = splicing_clipped2_df)
sample3_list <- compare_avg_power2(
  sv_clipped_smile, sv_clipped_body, 'Smile', 'Body', num_samples = 1000,
  column = "Nose_x_Central_Sitar")
max(sample3_list$Smile$Average_Power, na.rm = TRUE)
max(sample3_list$Body$Average_Power, na.rm = TRUE)

# Digging down on avg_power_segments ...

# Convert SplicedViews to lists to get individual segment data
view_smile_list <- split(sv_duration_smile)
view_body_list <- split(sv_duration_body)

# Plot a single segment in SplicedView
segment_10_view <- view_smile_list$`Mutual look and smile.10`
autoplot(segment_10_view)

# Calculate power spectrum for this segment
w <- analyze_wavelet(segment_10_view, column = "Nose_x_Central_Sitar")
plot_power_spectrum(w, segment_10_view)
plot_average_power(w, segment_10_view)

# Number of rows on each segment in a list of Segments using base R
sapply(view_smile_list, function(x) nrow(x$df))

# Simple stats on each view data column in each segment - sapply gives named matrices
mean_mat <- sapply_column_spliceview(sv_duration_smile, mean, na.rm=TRUE)
View(mean_mat)
sd_mat <- sapply_column_spliceview(sv_duration_body, sd, na.rm=TRUE)
View(sd_mat)

# More complex functions - apply fun to each Segment and fixed column in a SplicedView
wavelet_smile_list <- apply_segment_spliceview(sv_duration_smile, analyze_wavelet,
                                               column = "Nose_x_Central_Sitar")
wavelet_body_list <- apply_segment_spliceview(sv_duration_body, analyze_wavelet,
                                              column = "Nose_x_Central_Sitar")

# Compare power spectrum on two segments from two different splices
plot_power_spectrum(wavelet_smile_list$output$`Mutual look and smile.10`,
                    wavelet_smile_list$input$`Mutual look and smile.10`)
plot_power_spectrum(wavelet_body_list$output$`Mutual head and body movement.10`,
                    wavelet_body_list$input$`Mutual head and body movement.10`)

# Go straight to the average Power contained in a wavelet object on each segment
ave_power_smile <- ave_power_spliceview(sv_duration_smile, column = "Nose_x_Central_Sitar")
ave_power_body <- ave_power_spliceview(sv_duration_body, column = "Nose_x_Central_Sitar")
plot.ts(ave_power_smile[, 2:11], ann = FALSE) # Specialised plots?
plot.ts(ave_power_body[, 2:11], ann = FALSE)

# Digging down on ave_power_segments for cross wavelet power ...

# Cross wave power object lists
cross_power_smile_list <- apply_segment_spliceview(
  sv_duration_smile, analyze_coherency, columns = c("Nose_x_Central_Sitar", "Nose_y_Central_Sitar"))
cross_power_body_list <- apply_segment_spliceview(
  sv_duration_body, analyze_coherency, columns = c("Nose_x_Central_Sitar", "Nose_y_Central_Sitar"))
plot_cross_spectrum(cross_power_smile_list$output$`Mutual look and smile.1`,
                    cross_power_smile_list$input$`Mutual look and smile.1`)
plot_cross_spectrum(cross_power_body_list$output$`Mutual head and body movement.1`,
                    cross_power_body_list$input$`Mutual head and body movement.1`)

# Go straight to average cross power ...
ave_cross_power_smile <- ave_cross_power_spliceview(
  sv_duration_smile, columns = c("Nose_x_Central_Sitar", "Nose_y_Central_Sitar"))
ave_cross_power_body <- ave_cross_power_spliceview(
  sv_duration_body, columns = c("Nose_x_Central_Sitar", "Nose_y_Central_Sitar"))
plot.ts(ave_cross_power_smile[, 1:10], ann = FALSE)
plot.ts(ave_cross_power_body[, 1:10], ann = FALSE)

# Supply parameter to pull out information from each wavelet/cross wavelet object
# see ?analyze.wavelet
power_list <- pull_segment_spliceview(sv_duration_body, FUN = analyze_wavelet,
                        column = "Nose_x_Central_Sitar", element = 'Power')
# get the wavelet power in time-frequency domain on each segment (matrix - rows frequency, columns time)
View(power_list$output$`Mutual head and body movement`)

##################################################################################
#
# 2. Set of segments to compare against matching segments elsewhere in performance
#
##################################################################################
splicing_tabla_solo_df <- splice_time(d1, tier = 'Event', comments = 'tabla solo')
splicing_tabla_solo_df

# Make the segments the same length - offset sampling has better coverage with short clipped segments
splicing_tabla_solo_df <- clip_splice(splicing_tabla_solo_df, duration = 40)

# Apply a function over segments for an original splice and 'equivalent' randomly generated splices
# Then sample from original segments and compare against random selected segments
# from equivalent splices
sample4_list <- compare_ave_power1(
  jv1, splicing_tabla_solo_df, 'Harmonium Solos', num_segment_samples = 100,
  num_splice_samples = 10, sampling_type = 'offset', column = 'Nose_x_Central_Sitar')
ks.test(sample4_list$`Harmonium Solos`$Average_Power,
        sample4_list$`Sampled Splices`$Average_Power)

# Maybe only needs to calculate power spectrum on the unique segments sampled...

# Drilling down into calculation ...

# Randomly create matching segments - add a random offset to start times
# and use rejection sampling to avoid tabla solo segments
splicing_list <- sample_offset_splice(splicing_tabla_solo_df, jv1, num_samples = 100)

# Add in the original for comparison
splicing_list$Original <- splicing_tabla_solo_df
# Check distribution of samples - plot superimposed sample Start and End segments
visualise_sample_splices(splicing_list, jv1)

# Apply each sample splice to JoinedView to get a list of SplicedViews
sv_list <- lapply(splicing_list, function(x) get_spliced_view(jv1, splicing_df = x))

# Extract a named segment from each sample
segment_list <- lapply(sv_list, function(x) {
  view_list <- split(x)
  view_list[[which(names(view_list) == 'tabla solo.2')]]
})

# Power spectrum of tabla solo.2 segment from each sample
wavelet_tabla_list <- lapply(segment_list, analyze_wavelet, column = "Nose_x_Central_Sitar")
plot_power_spectrum(wavelet_tabla_list$Original, segment_list$Original)
plot_average_power(wavelet_tabla_list$Original, segment_list$Original)
plot_power_spectrum(wavelet_tabla_list[[1]], segment_list[[1]])
plot_average_power(wavelet_tabla_list[[1]], segment_list[[1]])

# Filtering of output segments - process list - for fixed time - maybe an option on the
# splice for fixed windows from the provided segments?

# Compare original ave power with sampled average power on tabla solo.1 segments
# max_ave_power_original <- max(ave_power_tabla[, 'Original'])
# max_ave_power_dist <- apply(ave_power_tabla, 2, max, na.rm = TRUE)
# plot(max_ave_power_dist)
# abline(h=max_ave_power_original)

# Apply a function across samples AND segments
long_ave_power_df <- ave_power_over_samples(jv1, splicing_tabla_solo_df, num_samples = 10,
                                       column = 'Nose_x_Central_Sitar')

ggplot(long_ave_power_df) +
  geom_line(aes(x = Period, y = Average_Power, colour = Sample)) +
  scale_x_continuous(trans='log2') +
  facet_wrap(~Segment) # dedicated function to do

# Avoid some sections with a condition
avoid_list <- list(avoid_segment1 = c(10, 100), avoid_segment2 = c(2500, 2600))
avoid_splice_dfr <- splice_time(avoid_list)

# Randomly create matching segments - add a random offset and use rejection sampling
splicing2_list <- sample_offset_splice(splicing_tabla_solo_df, jv1, num_samples = 100,
                                rejection_list = list(avoid_splice_dfr))
df <- dplyr::bind_rows(splicing2_list, .id = 'Sample')
avoid_splice_dfr$Segment <- NA
ggplot(df, aes(y = Segment)) +
  geom_linerange(aes(xmin = Start, xmax = End)) +
  geom_rect(data = splicing_tabla_solo_df,
            aes(xmin = Start, xmax = End, ymin = 0, ymax = Inf, fill = Segment), alpha = 0.5) +
  geom_rect(data = avoid_splice_dfr,
            aes(xmin = Start, xmax = End, ymin = 0, ymax = Inf), alpha = 0.5)

# Distribution of new segments faceted by original segment
ggplot(df) +
  geom_linerange(aes(y = Sample, xmin = Start, xmax = End, colour = Segment)) +
  theme(axis.ticks.y=element_blank(), axis.text.y=element_blank(), panel.background = element_blank()) +
  facet_wrap(~Segment) +
  geom_rect(data = splicing_tabla_solo_df,
          aes(xmin = Start, xmax = End, ymin = 0, ymax = Inf, fill = Segment), alpha = 0.5) +
  geom_rect(data = avoid_splice_dfr[-1],
            aes(xmin = Start, xmax = End, ymin = 0, ymax = Inf), alpha = 0.5)

# Sample using a Poisson process to count gaps but maintain segment durations
splicing3_list <- sample_gap_splice(splicing_tabla_solo_df, jv1, num_samples = 100,
                                    rejection_list = list(avoid_splice_dfr))
df <- dplyr::bind_rows(splicing3_list, .id = 'Sample')
ggplot(df, aes(y = Segment)) +
  geom_linerange(aes(xmin = Start, xmax = End)) +
  geom_rect(data = splicing_tabla_solo_df,
            aes(xmin = Start, xmax = End, ymin = 0, ymax = Inf, fill = Segment), alpha = 0.5) +
  geom_rect(data = avoid_splice_dfr,
            aes(xmin = Start, xmax = End, ymin = 0, ymax = Inf), alpha = 0.5)

# Distribution of new segments faceted by original segment
ggplot(df) +
  geom_linerange(aes(y = Sample, xmin = Start, xmax = End, colour = Segment)) +
  theme(axis.ticks.y=element_blank(), axis.text.y=element_blank(), panel.background = element_blank()) +
  facet_wrap(~Segment) +
  geom_rect(data = splicing_tabla_solo_df,
            aes(xmin = Start, xmax = End, ymin = 0, ymax = Inf, fill = Segment), alpha = 0.5) +
  geom_rect(data = avoid_splice_dfr[-1],
            aes(xmin = Start, xmax = End, ymin = 0, ymax = Inf), alpha = 0.5)

# Vary the durations of the segments and keep the gaps the same??? durations exponential dist - NO
# apply power to samples vs original  - plot

# Splices based on Metre object
m1 <- get_metre_data(r1)
autoplot(m1)
head(m1[[1]])

splicing_metre_df <- splice_time(m1, window_duration = 10)
head(splicing_metre_df)

sv_metre <- get_spliced_view(jv1, splicing_df = splicing_metre_df)
autoplot(sv_metre)

# Splices based on OnsetSelected object
r1 <- get_recording("NIR_ABh_Puriya", fps = 25)
o1 <- get_onsets_selected_data(r1)
plot(o1)
autoplot(o1)
r2 <- get_recording("NIR_DBh_Malhar_2Gats", fps = 25)
o2 <- get_onsets_selected_data(r2)
plot(o2)
autoplot(o2)
r3 <- get_recording("NIRP1_MAK_Jaun", fps = 25)
o3 <- get_onsets_selected_data(r3)
plot(o3, instrument = 'Onset')
autoplot(o3, instrument = 'Onset')
r4 <- get_recording("NIRP1_VS_Hams", fps = 25)
o4 <- get_onsets_selected_data(r4)
plot(o4, instrument = 'Onset')
autoplot(o4, instrument = 'Onset')
r5 <- get_recording("Gagaku_5_Juha", fps = 60)
o5 <- get_onsets_selected_data(r5)
plot(o5, instrument = 'Hichiriki', matra = 'SD_T') # ok
autoplot(o5, instrument = 'Hichiriki', matra = 'SD_T')

instruments <- c("Shoko_L", "Shoko_R", "Taiko", "Kakko", "Kakko_1", "So", "Biwa",
                 "Ryuteki", "Hichiriki", "Sho", "Biwa_RW", "Shoko_RW", "Taiko_LW",
                 "Taiko_RW")

# Difference in onsets for each instrument pair
po1 <- difference_onsets(o1, instruments = c('Inst', 'Tabla'))
ggpairs(po1, columns = 2:4, aes(colour = Tala))
po2 <- difference_onsets(o2, instruments = c('Inst', 'Tabla'))
ggpairs(po2, columns = 2:4, aes(colour = Tala))
po5 <- difference_onsets(o5, instruments = instruments)
ggpairs(po5, columns = 2:5, aes(colour = Tala)) # only one Tala in plot

# Summary of difference in onsets (allows segmentation via splicing_dfr argument)
summary_dfr <- summary_onsets(o5, instruments = instruments) # Add number of rows
View(summary_dfr)
ggplot(summary_dfr) + # plot_summary_onsets
  geom_col(aes(x = Mean_Absolute_Difference, y = Instrument_Pair)) +
  ggtitle("Mean Absolute Onset Differences of Instrument Pairs")

# Splice the processed onsets
d5 <- get_duration_annotation_data(r5)
splicing_dfr <- splice_time(d5, tier = 'Section')
segmented_po <- difference_onsets(o5, instruments = instruments, splicing_dfr = splicing_dfr)
View(segmented_po)
ggpairs(segmented_po, columns = 3:6, aes(colour = Segment))
ggpairs(segmented_po, columns = c(3, 7:9), aes(colour = Segment))

# Do splicing via a separate function get_spliced_onsets like views
#spliced_onsets <- get_spliced_onsets(po5, splicing_dfr)

# Calculate summary statistics on the segments
summary_segmented_list <- summary_onsets(o5, instruments = instruments, splicing_dfr)
summary_segmented_list$C.v2[1:3, ]

# Visualise stats on segment A
barplot(Mean_Absolute_Difference  ~ Instrument_Pair, ylab = "", cex.names = 0.5,
        las = 2, horiz = TRUE, main = "Mean Absolute Onset Differences of Instrument Pairs",
        data = summary_segmented_list$A.v2)

# DONE
# difference based Inst, + others and add differences in time as columns
# mean sd, mean of absolute  from second object
# subset onset and summary on sections in annotation

# Windows around reference points of processed onset object
splicing_dfr <- splice_time(po1, window_duration = 1) # filter using columns in onset data, expr for filter
head(splicing_dfr)

# TODO:
# subset onset data with a condition based on duration annotation data
# subsetting between objects and sampling
# change_splice - take in splice return new one with criteria
# two plots - originals vs samples or A vs B -
# p number from some statistical test for average power or stats on onsets
