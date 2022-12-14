# Test the statistical analysis functions

rm(list=ls())
library(movementsync)
library(ggplot2)
library(GGally)
library(zoo)

r1 <- get_recording("NIR_ABh_Puriya", fps = 25)
fv1_list <- get_filtered_views(r1, data_points = "Nose", n = 41, p = 3)
jv1 <- get_joined_view(fv1_list)
autoplot(jv1)

# Splices based on Duration object
d1 <- get_duration_annotation_data(r1)
autoplot(d1)
autoplot(d1) + theme(axis.text.x = element_text(size = 8)) # resize x-labels

# Mutual look and smile
splicing_smile_df <- splice_time(
  d1, tier ='INTERACTION', comments = 'Mutual look and smile'
)
splicing_smile_df
sv_duration_smile <- get_spliced_view(jv1, splicing_df = splicing_smile_df)
autoplot(sv_duration_smile)

# Mutual head and body movement
splicing_body_df <- splice_time(
  d1, tier = 'INTERACTION', comments = 'Mutual head and body movement'
)
splicing_body_df
sv_duration_body <- get_spliced_view(jv1, splicing_df = splicing_body_df)
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
ks.test(sample_list$Smile$Average_Power, sample_list$Body$Average_Power)

# Draw 1000 samples from the average cross power on random segments of a SplicedView
# and compare with another
sample2_list <- compare_avg_cross_power2(
  sv_duration_smile, sv_duration_body, 'Smile', 'Body', num_samples = 1000,
  columns = c("Nose_x_Central_Sitar", "Nose_y_Central_Sitar"))

# Summary stats
max(sample2_list$Smile$Average_Cross_Power, na.rm = TRUE)
max(sample2_list$Body$Average_Cross_Power, na.rm = TRUE)
ks.test(sample2_list$Smile$Average_Cross_Power, sample2_list$Body$Average_Cross_Power)

# Clip splice so segments of fixed lengths in each SplicedView
splicing_clipped1_df <- clip_splice(splicing_smile_df, duration = 1, location = 'middle')
splicing_clipped2_df <- clip_splice(splicing_body_df, duration = 1, location = 'middle')
splicing_clipped1_df
splicing_clipped2_df

# Draw 1000 samples from the average power on random clipped segments
sv_clipped_smile <- get_spliced_view(jv1, splicing_df = splicing_clipped1_df)
sv_clipped_body <- get_spliced_view(jv1, splicing_df = splicing_clipped2_df)
sample3_list <- compare_avg_power2(
  sv_clipped_smile, sv_clipped_body, 'Smile', 'Body', num_samples = 1000,
  column = "Nose_x_Central_Sitar")

# Summary stats
max(sample3_list$Smile$Average_Power, na.rm = TRUE)
max(sample3_list$Body$Average_Power, na.rm = TRUE)
ks.test(sample3_list$Smile$Average_Power, sample3_list$Body$Average_Power)

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
head(mean_mat)
sd_mat <- sapply_column_spliceview(sv_duration_body, sd, na.rm=TRUE)
head(sd_mat)

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
head(power_list$output$`Mutual head and body movement`)

angle_list <- pull_segment_spliceview(
  sv_duration_smile, FUN = analyze_coherency,
  columns = c("Nose_x_Central_Sitar", "Nose_x_Central_Sitar"), element = 'Angle')
head(angle_list$output$`Mutual head and body movement.10`) # 0s - need a longer time segmentation?

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

# Summary stats/tests
max(sample4_list$`Harmonium Solos`$Average_Power, na.rm = TRUE)
max(sample4_list$`Sampled Splices`$Average_Power, na.rm = TRUE)
ks.test(sample4_list$`Harmonium Solos`$Average_Power,
        sample4_list$`Sampled Splices`$Average_Power)

# Drilling down into calculation ...

# Randomly create matching segments - add a random offset to start times
# and use rejection sampling to avoid tabla solo segments
splicing_list <- sample_offset_splice(splicing_tabla_solo_df, jv1, num_splices = 100)

# Add in the original for comparison
splicing_list$Original <- splicing_tabla_solo_df
# Check distribution of samples - plot superimposed sample Start and End segments
visualise_sample_splices(splicing_tabla_solo_df, splicing_list, jv1)

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

# Apply average power across splices AND segments then calculate mean over splices per segment
mean_ave_power_df <- ave_power_over_splices(jv1, splicing_tabla_solo_df, num_splices = 10,
                                       column = 'Nose_x_Central_Sitar', show_plot = TRUE)
head(mean_ave_power_df)

# Avoid some sections with a condition
avoid_list <- list(avoid_segment1 = c(10, 100), avoid_segment2 = c(2500, 2600))
avoid_splice_dfr <- splice_time(avoid_list)

# Randomly create matching segments - add a random offset and use rejection sampling
splicing2_list <- sample_offset_splice(splicing_tabla_solo_df, jv1, num_splices = 100,
                                rejection_list = list(avoid_splice_dfr))

# Distribution of new segments faceted by original segment
visualise_sample_splices(splicing_tabla_solo_df, splicing2_list, jv1, avoid_splice_dfr = avoid_splice_dfr)

# Sample segments faceted by original segment
visualise_sample_splices(splicing_tabla_solo_df, splicing2_list, jv1, avoid_splice_dfr = avoid_splice_dfr,
                         unstack = TRUE)

# Sample using a Poisson process to count gaps but maintain segment durations
splicing3_list <- sample_gap_splice(splicing_tabla_solo_df, jv1, num_splices = 100,
                                    rejection_list = list(avoid_splice_dfr))

# Visualise samples
visualise_sample_splices(splicing_tabla_solo_df, splicing3_list, jv1, avoid_splice_dfr = avoid_splice_dfr)
visualise_sample_splices(splicing_tabla_solo_df, splicing3_list, jv1, avoid_splice_dfr = avoid_splice_dfr,
                         unstack = TRUE)

################################################################################
#
# 3. Onsets difference and summary statistics
#
################################################################################

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
autoplot(o5, instrument = 'Hichiriki', tactus = 'SD_T')

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
summary_dfr <- summary_onsets(o5, r5, instruments = instruments,
                              show_plot = TRUE, filter_pair = 'T')

# Splice the processed onsets for summarisation on segments
d5 <- get_duration_annotation_data(r5)
splicing_section_dfr <- splice_time(d5, tier = 'Section')
segmented_po <- difference_onsets(o5, instruments = instruments, splicing_dfr = splicing_section_dfr)
head(segmented_po)
ggpairs(segmented_po, columns = 3:6, aes(colour = Segment))
ggpairs(segmented_po, columns = c(3, 7:9), aes(colour = Segment))

# Calculate summary statistics on the segments from splice
summary_segmented_dfr <- summary_onsets(o5, r5, instruments = instruments, splicing_section_dfr,
                                        show_plot = TRUE, filter_pair = 'K.*-S')
summary_segmented_dfr

# Summary for r1 of onset differences using splice from annotation data
splicing_tabla_solo_df
summary_onsets(o1, r1, instruments = c('Inst', 'Tabla'),
               splicing_dfr = splicing_tabla_solo_df, show_plot = TRUE)

# Summary for r5 of onset differences using splice from annotation data
summary_onsets(o5, r5, instruments = instruments, splicing_section_dfr, filter_pair = "Taiko-",
               show_plot = TRUE)

# Summary for r5 of difference in onsets on third beat
summary_onsets(o5, r5, instruments = instruments, splicing_section_dfr,
               expr = 'SD_T == 3', show_plot = TRUE)

################################################################################
#
# 4. Generating splices, subsetting between splices and sampling
#
################################################################################

# Windows around reference points of processed onset object using an expression filter
po1_beat3 <- difference_onsets(o1, instruments = c('Inst', 'Tabla'), expr = 'Matra == 3')
splicing_po1_beat3_dfr <- splice_time(po1_beat3, window_duration = 0.4)
is_splice_overlapping(splicing_po1_beat3_dfr)
head(splicing_po1_beat3_dfr)

# Splice based on Metre object
m1 <- get_metre_data(r1)
splicing_metre_dfr <- splice_time(m1, window_duration = 1)
is_splice_overlapping(splicing_metre_dfr)
head(splicing_metre_dfr)

# Splice based on Duration object
splicing_form_dfr <- splice_time(
  d1, tier = 'FORM', comments = "Vilambit teental (16 beats)"
)
splicing_form_dfr

# Merged splice: tabla solo in Vilambit teental (16 beats)
splicing_merged_dfr <- merge_splice(Vilambit = splicing_form_dfr,
                                  tabla_solo = splicing_tabla_solo_df,
                                  operation = 'intersection')
splicing_merged_dfr
splicing_tabla_solo_df

# Compare tabla solo vs tabla solo in Vilambit teental (16 beats)
sv_tabla_solo <- get_spliced_view(jv1, splicing_df = splicing_tabla_solo_df)
sv_merged <- get_spliced_view(jv1, splicing_df = splicing_merged_dfr)
sample5_list <- compare_avg_power2(
  sv_tabla_solo, sv_merged, 'Tabla Solo', 'Tabla Solo & Vilambit', num_samples = 1000,
  column = "Nose_x_Central_Sitar")

ave_power_tabla_solo <- ave_power_spliceview(sv_tabla_solo, column = "Nose_x_Central_Sitar",
                                             show_plot = TRUE)





