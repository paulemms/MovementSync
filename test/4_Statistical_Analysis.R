# Test the statistical analysis functions

rm(list=ls())
devtools::load_all()

r1 <- get_recording("NIR_ABh_Puriya", fps = 25)
fv1_list <- get_filtered_views(r1, data_points = "Nose", n = 41, p = 3)
jv1 <- get_joined_view(fv1_list)
autoplot(jv1)

# Splices based on Duration object
d1 <- get_duration_annotation_data(r1)
autoplot(d1)

splicing_duration_df <- splice_time(d1, tier = 'FORM')
head(splicing_duration_df)

sv_duration <- get_spliced_view(jv1, splicing_df = splicing_duration_df)
autoplot(sv_duration)

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

# Number of rows on each segment in a list of Tiers using base R
sapply(view_smile_list, function(x) nrow(x$df))

# Apply summary function to each data point column in a SplicedView and return list of output data
apply_summary_spliceview <- function(sv, FUN, simplify = FALSE, USE.NAMES = FALSE, ...) {
  v_list <- split(sv)
  sapply(v_list, function(x) {
    keys <- match(c('Tier', 'Frame', 'Time'), colnames(x$df), nomatch = 0)
    dfr <- x$df[-keys]
    apply(dfr, 2, function(y) FUN(y, ...))
  }, simplify = simplify, USE.NAMES = USE.NAMES)
}
# Simplify list to matrix
sapply_summary_spliceview <- function(sv, FUN, simplify = TRUE, USE.NAMES = TRUE, ...) {
  apply_summary_spliceview(sv, FUN, simplify = simplify, USE.NAMES = USE.NAMES, ...)
}

# Simple stats on each view data column - sapply gives named matrices
View(sapply_summary_spliceview(sv_duration_smile, mean, na.rm=TRUE))
View(sapply_summary_spliceview(sv_duration_body, sd, na.rm=TRUE))

# More complex functions - apply fun to each Tier in a SplicedView
view_smile_list <- split(sv_duration_smile)
view_body_list <- split(sv_duration_body)
wavelet_smile_list <- lapply(view_smile_list, analyze_wavelet, column = "Nose_x_Central_Sitar")
wavelet_body_list <- lapply(view_body_list, analyze_wavelet, column = "Nose_x_Central_Sitar")
plot_power_spectrum(wavelet_smile_list$`Mutual look and smile.10`, view_smile_list$`Mutual look and smile.10`)

# Units on base R plot reflects internal data
plot(wavelet_smile_list$`Mutual look and smile.10`$Power.avg)

# Get the average power for each segment in a named list
ave_power_smile <- sapply(wavelet_smile_list, function(x) x$Power.avg)
ave_power_body <- sapply(wavelet_body_list, function(x) x$Power.avg)
View(ave_power_smile)
plot.ts(ave_power_smile[, 1:10], ann = FALSE)
plot.ts(ave_power_body[, 1:10], ann = FALSE)

# Draw 1000 samples from random segments with replacement
num_samples <- 1000
period_sample <- sample(nrow(ave_power_smile), num_samples, replace = TRUE)
segment_sample <- sample(ncol(ave_power_smile), 1000, replace = TRUE)
sampled_avgpow_smile <- ave_power_smile[cbind(period_sample, segment_sample)]
barplot(sampled_avgpow_smile)

# or convert to vector
total_sample <- sample(nrow(ave_power_smile) * ncol(ave_power_smile), num_samples, replace = TRUE)
sampled_avgpow_smile <- as.vector(ave_power_smile)[total_sample]
barplot(sampled_avgpow_smile)

# Tabla solos
splicing_tabla_solo_df <- splice_time(d1, tier = 'Event', comments = 'tabla solo')
splicing_tabla_solo_df

# randomly create matching segments - add a random offset and use rejection sampling

# find max possible offset based on recording length
max_time <- max(jv1$df$Time, na.rm = TRUE)

# total span of segments
total_span <- max(splicing_tabla_solo_df$Start, na.rm = TRUE) -
  min(splicing_tabla_solo_df$Start, na.rm = TRUE)

# random start times
stopifnot(total_span <= max_time)
num_samples <- 100
start_times <- runif(num_samples, min = 0, max = max_time - total_span)

# Generate a list of new sampling data.frames
splicing_list <- lapply(start_times, function(x) {
  splicing_tabla_solo_df$Start <- splicing_tabla_solo_df$Start + x
  splicing_tabla_solo_df$End <- splicing_tabla_solo_df$End + x
  splicing_tabla_solo_df
})
names(splicing_list) <- paste('Sample splice', seq_along(splicing_list))

# Which ones overlap the original splicing?
is_overlapped <- sapply(splicing_list,
                        function(x) is_splice_overlapping(x, splicing_tabla_solo_df))

# remove the overlapping ones
splicing_list <- splicing_list[!is_overlapped]

# Repeat until we get the desired number of samples - stick with what we have for now

# Add in the original for comparison
splicing_list$Original <- splicing_tabla_solo_df

# Apply each sample splice to JoinedView to get a list of SplicedViews
sv_list <- lapply(splicing_list, function(x) get_spliced_view(jv1, splicing_df = x))

autoplot(sv_list$Original)
autoplot(sv_list$`Sample splice 1`)

# Extract a named segment from each sample
segment_list <- lapply(sv_list, function(x) {
  view_list <- split(x)
  view_list[[which(names(view_list) == 'tabla solo.1')]]
})
autoplot(segment_list$`Sample splice 1`)

# Power spectrum of named segment from each sample
wavelet_tabla_list <- lapply(segment_list, analyze_wavelet, column = "Nose_x_Central_Sitar")
plot_power_spectrum(wavelet_tabla_list$Original, segment_list$Original)
plot_power_spectrum(wavelet_tabla_list$`Sample splice 1`, segment_list$`Sample splice 1`)
ave_power_tabla <- sapply(wavelet_tabla_list, function(x) x$Power.avg)
View(ave_power_tabla)
plot.ts(ave_power_tabla[, 1:10], ann = FALSE)

# Compare original with samples - how? max ave power?
max_ave_power_original <- max(ave_power_tabla)
max_ave_power_dist <- apply(ave_power_tabla, 2, max, na.rm = TRUE)
plot(max_ave_power_dist)
abline(h=max_ave_power_original)

# avoid some sections? with a condition? OK
# Add extra splicing data frames that contain regions of avoidance and reject them as above

# same durations but not necessarily same gaps? distribution of gaps between starts?
gap_dfr <- dplyr::mutate(splicing_duration1_df, Prev_Start = dplyr::lag(Start, default = 0))
gap_dfr <- dplyr::mutate(gap_dfr, Duration = End - Start)
gap_dfr <- dplyr::mutate(gap_dfr, Gap = Start - Prev_Start)
ave_gap_splice <- mean(gap_dfr$Gap, na.rm = TRUE) # seconds
# interarrival time between gaps exponentially distributed - so start time Poisson (rate)
gap_dfr$New_Gap <- rexp(nrow(splicing_duration1_df), rate = 1/ave_gap_splice)
gap_dfr <- dplyr::mutate(gap_dfr, New_Start = cumsum(New_Gap), New_End = New_Start + Duration)
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

# do it for proportion of interval

# Splices based on OnsetSelected object
o1 <- get_onsets_selected_data(r1)

# difference based Inst, + others and add differences in time as columns
# generate a reference beat time point from the mean
# mean sd, mean of absolute  from second object

# Windows around reference points and then set operations to
# subset onset data with a condition based on duration annotation data
# subsetting between objects and sampling
