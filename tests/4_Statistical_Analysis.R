# Test the statistical analysis functions
library(ggplot2)
library(GGally)
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
plot(w$Power.avg) # raw data from wavelet object using base R plot

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
plot.ts(ave_power_smile[, 1:10], ann = FALSE) # Specialised plots?
plot.ts(ave_power_body[, 1:10], ann = FALSE)

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
plot.ts(ave_cross_power_smile[, 1:10], ann = FALSE) # Specialised plots?
plot.ts(ave_cross_power_body[, 1:10], ann = FALSE)

# Maybe supply parameter to pull out information from each wavelet/cross wavelet object???
# see ?analyze.wavelet
ampl_list <- pull_segment_spliceview(sv_duration_body, FUN = analyze_wavelet,
                        column = "Nose_x_Central_Sitar", element = 'Ampl')
View(ampl_list$output$`Mutual head and body movement`)

# Draw 1000 samples from random segments with replacement
samp_ave_power_smile <- sample_ave_power_spliceview(
  sv_duration_smile, num_samples = 1000, column = "Nose_x_Central_Sitar")
samp_ave_power_body <- sample_ave_power_spliceview(
  sv_duration_body, num_samples = 1000, column = "Nose_x_Central_Sitar")
plot(samp_ave_power_smile)
plot(samp_ave_power_body)

# Tabla solos
splicing_tabla_solo_df <- splice_time(d1, tier = 'Event', comments = 'tabla solo')
splicing_tabla_solo_df

# Randomly create matching segments - add a random offset and use rejection sampling
splicing_list <- sample_splice(splicing_tabla_solo_df, jv1, num_samples = 100)

# Check distribution of start times
start_times <- unlist(lapply(splicing_list, function(x) x$Start))
plot(start_times)
abline(h=splicing_tabla_solo_df$Start)
abline(h=splicing_tabla_solo_df$End)

# Plot Start and End times
df <- dplyr::bind_rows(splicing_list)
ggplot(df, aes(y = Segment)) +
  geom_linerange(aes(xmin = Start, xmax = End)) +
  geom_rect(data = splicing_tabla_solo_df,
            aes(xmin = Start, xmax = End, ymin = 0, ymax = Inf, fill = Segment), alpha = 0.5)

# Add in the original for comparison
splicing_list$Original <- splicing_tabla_solo_df

# Add see the original segment covering bands
df <- dplyr::bind_rows(splicing_list)
ggplot(df, aes(y = Segment)) +
  geom_linerange(aes(xmin = Start, xmax = End)) +
  geom_rect(data = splicing_tabla_solo_df,
            aes(xmin = Start, xmax = End, ymin = 0, ymax = Inf, fill = Segment), alpha = 0.5)


# Apply each sample splice to JoinedView to get a list of SplicedViews
sv_list <- lapply(splicing_list, function(x) get_spliced_view(jv1, splicing_df = x))

autoplot(sv_list$Original)
autoplot(sv_list[[1]])

# Extract a named segment from each sample
segment_list <- lapply(sv_list, function(x) {
  view_list <- split(x)
  view_list[[which(names(view_list) == 'tabla solo.1')]]
})
autoplot(segment_list[[1]])

# Power spectrum of named segment from each sample
wavelet_tabla_list <- lapply(segment_list, analyze_wavelet, column = "Nose_x_Central_Sitar")
plot_power_spectrum(wavelet_tabla_list$Original, segment_list$Original)
plot_power_spectrum(wavelet_tabla_list[[1]], segment_list[[1]])
ave_power_tabla <- sapply(wavelet_tabla_list, function(x) x$Power.avg)
View(ave_power_tabla)
plot.ts(ave_power_tabla[, 1:10], ann = FALSE)

# Filtering of output segments - process list - for fixed time - maybe an option on the
# splice for fixed windows from the provided segments?

# Compare original with samples - how? max ave power?
max_ave_power_original <- max(ave_power_tabla[, 'Original'])
max_ave_power_dist <- apply(ave_power_tabla, 2, max, na.rm = TRUE)
plot(max_ave_power_dist)
abline(h=max_ave_power_original)

# Avoid some sections with a condition
avoid_list <- list(avoid_segment1 = c(3000, 3100), avoid_segment2 = c(4000, 4100))
avoid_splice_dfr <- splice_time(avoid_list)

# Randomly create matching segments - add a random offset and use rejection sampling
splicing2_list <- sample_splice(splicing_tabla_solo_df, jv1, num_samples = 100,
                                rejection_list = list(avoid_splice_dfr))
df <- dplyr::bind_rows(splicing2_list, .id = 'Sample')
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

# Same durations but not necessarily same gaps? number of gaps follows Poisson process?
gap_dfr <- dplyr::mutate(splicing_duration1_df, Next_Start = dplyr::lead(Start))
gap_dfr <- dplyr::mutate(gap_dfr, Duration = End - Start)
gap_dfr <- dplyr::mutate(gap_dfr, Gap = Next_Start - Start)
gap_dfr <- dplyr::mutate(gap_dfr, Prev_Gap = dplyr::lag(Gap, default = Start[1]))
ave_gap_splice <- mean(gap_dfr$Prev_Gap, na.rm = TRUE) # seconds
# interarrival time gap occuring is exponentially distributed
gap_dfr <- dplyr::mutate(gap_dfr, New_Gap = rexp(nrow(splicing_duration1_df), rate = 1/ave_gap_splice))
gap_dfr <- dplyr::mutate(gap_dfr, New_Start = cumsum(New_Gap + dplyr::lag(Duration, default = 0)),
                         New_End = New_Start + Duration)
gap_dfr

# First Gap gives start of initial segment
# Need to reject samples going beyond recording length

# plot sampling data frame as horizontal line and stack to visualise areas of avoidance
# https://stackoverflow.com/questions/64334320/r-horizontal-bar-chart-simple-gantt-chart

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
plot(o5, instrument = 'Hichiriki', matra = 'SD_T')
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

# Summary of difference in onsets - currently ignoring Tala ???? but ok for o5 because there is one
summary_dfr <- summary_onsets(o5, instruments = instruments)
View(summary_dfr)
old_params <- par(mar=c(5,10,2,1))
barplot(Mean_Absolute_Difference  ~ Instrument_Pair, ylab = "", cex.names = 0.5,
        las = 2, horiz = TRUE, main = "Mean Absolute Onset Differences of Instrument Pairs",
        data = summary_dfr)
par(old_params)
ggplot(summary_dfr) +
  geom_col(aes(x = Mean_Absolute_Difference, y = Instrument_Pair))

# Splice the difference
d5 <- get_duration_annotation_data(r5)
splicing_dfr <- splice_time(d5, tier = 'Section')
segmented_differences_dfr <- difference_onsets(o5, instruments = instruments, splicing_dfr = splicing_dfr)
View(segmented_differences_dfr)
ggpairs(segmented_differences_dfr, columns = 3:6, aes(colour = Segment))
ggpairs(segmented_differences_dfr, columns = c(3, 7:9), aes(colour = Segment))

# Calculate summary statistics on the segments
summary_segmented_list <- summary_onsets(o5, instruments = instruments, splicing_dfr)
summary_segmented_list$C.v2[1:3, ]

# Visualise stats on segment A
barplot(Mean_Absolute_Difference  ~ Instrument_Pair, ylab = "", cex.names = 0.5,
        las = 2, horiz = TRUE, main = "Mean Absolute Onset Differences of Instrument Pairs",
        data = summary_segmented_list$A.v2)

# difference based Inst, + others and add differences in time as columns
# mean sd, mean of absolute  from second object
# subset onset and summary on sections in annotation

# Windows around reference points of processed onset object
splicing_dfr <- splice_time(po1, window_duration = 1)
head(splicing_dfr)

# TODO:
# subset onset data with a condition based on duration annotation data
# subsetting between objects and sampling
