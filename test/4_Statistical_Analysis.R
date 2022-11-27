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
  d1, tier ='INTERACTION', comments = 'Mutual look and smile', make.unique = TRUE
)
splicing_duration1_df
sv_duration1 <- get_spliced_view(jv1, splicing_df = splicing_duration1_df)
autoplot(sv_duration1)

# Mutual head and body movement
splicing_duration2_df <- splice_time(
  d1, tier = 'INTERACTION', comments = 'Mutual head and body movement'
)
splicing_duration2_df
sv_duration2 <- get_spliced_view(jv1, splicing_df = splicing_duration2_df)
autoplot(sv_duration2)

# downsample\
sv_list1 <- sample_spliced_views(a = sv_duration1, b = sv_duration2, num_samples = 100)
autoplot(sv_list1$a)
autoplot(sv_list1$b)

# upsample 10000 points from each SplicedView and return a SplicedView object list
sv_list2 <- sample_spliced_views(a = sv_duration1, b = sv_duration2, num_samples = 10000,
                        replace = TRUE)
autoplot(sv_list2$a)
autoplot(sv_list2$b)


v_list <- split(sv_list$a)
autoplot(v_list[[1]])
w1 <- analyze_wavelet(fv2, "Nose_x")
plot_power_spectrum(w1, fv2)

# Tabla solos
splicing_duration4_df <- splice_time(d1, tier = 'Event',
                                     comments = 'tabla solo', make.unique = TRUE)
splicing_duration4_df

# randomly create matching segments? how?

# Tier    Start      End
# 1   tabla solo 1168.218 1209.722
# 2 tabla solo.1 1334.148 1374.912
# 3 tabla solo.2 1552.906 1610.111
# 4 tabla solo.3 1752.424 1856.106
# 5 tabla solo.4 1975.688 2025.760
# 6 tabla solo.5 2295.255 2372.199
# 7 tabla solo.6 2791.923 2860.007

# fixed random addition to all start times? (clip at end, avoid existing intervals)?
# avoid some sections? with a condition? OK
# same durations but not necessarily same gaps? distribution of gaps? uniform?

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
