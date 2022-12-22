# Introduces dataset (5 performances) and data types; functions for loading and
# pre-processing movement and annotation data; data visualisation and exploration.

rm(list = ls())
gc()
if (dev.cur() > 1) dev.off()
library(movementsync)

################################################################################
### Recording 1
################################################################################
r1 <- get_recording("NIR_ABh_Puriya", fps = 25)
summary(r1)

o1 <- get_onsets_selected_data(r1)
summary(o1)
autoplot(o1)

m1 <- get_metre_data(r1)
summary(m1)
autoplot(m1)
summary(m1, tempo = TRUE)
autoplot(m1, tempo = TRUE)

d1 <- get_duration_annotation_data(r1)
summary(d1)
autoplot(d1)

rv1 <- get_raw_view(r1, "Central", "", "Sitar")
summary(rv1)
autoplot(rv1)

rv2 <- get_raw_optflow_view(r1, "Central", "", "Sitar")
autoplot(rv2, columns = c("Head_x", "Head_y", "Head_d")) # drift in camera

# Time limit axis
autoplot(rv1, columns = c("LEar_x", "LEar_y"), time_limits = c(15*60, 20*60), maxpts = 2000)

# Autolayering with OnsetSelected, Metre and Duration objects,
autoplot(rv1, columns = c("LEar_x", "LEar_y")) + autolayer(d1)
autoplot(rv1, columns = c("LEar_x", "LEar_y")) + autolayer(o1)
autoplot(rv1, columns = c("LEar_x", "LEar_y")) +
  autolayer(d1, filter_expr = Tier == "FORM" & substr(Comments, 1, 1) == "J")
autoplot(rv1, columns = c("LEar_x", "LEar_y"), time_limits = c(1000, 2000), maxpts=5000) +
  autolayer(m1, time_limits = c(1000, 2000), alpha = 0.5)
# Showing tempo layer on facets
autoplot(rv1, columns = "LEar_d", time_limits = c(1000, 2000), maxpts = Inf) +
  autolayer(m1, tempo = TRUE, time_limits = c(1000, 2000), view = rv1,
            columns = "LEar_d")
autoplot(rv1, columns = c("LEar_d", "Nose_d"), time_limits = c(1000, 2000), maxpts = Inf) +
  autolayer(m1, tempo = TRUE, time_limits = c(1000, 2000), view = rv1,
            columns = c("LEar_d", "Nose_d"), colour = 'orange')

# Set time-scale using Duration object
autoplot(rv1, columns = c("LEar_x", "LEar_y"), time_limits = d1,
         time_expr = Tier == "FORM" & Comments == "Jor")

# Processed data
pv1 <- get_processed_view(rv1)
autoplot(pv1)
autoplot(pv1, columns = c("LEar_x", "LEar_y", "LEar_d"))
pv2 <- get_processed_view(rv2)
autoplot(pv2, columns = c("Head_x", "Head_y", "Head_d")) # Trend in Head removed

# Filtered data
fv1 <- apply_filter_sgolay(pv1, c("Nose", "RWrist", "LWrist"), n = 19, p = 4)
summary(fv1)
autoplot(fv1)
autoplot(fv1) + autolayer(d1)

# Saving data objects to the file system ...

# Save the filtered objected in an RDS file
fv1_file <- file.path(tempdir(), "fv1.rds")
saveRDS(fv1, file = fv1_file)
rm(fv1)
fv1 <- readRDS(fv1_file)
autoplot(fv1)

# Different filter
fv2 <- apply_filter(pv1, c("Nose", "RWrist", "LWrist"), signal::MedianFilter(n = 3))
autoplot(fv2)

# Load feature data (by default it is not made continuous)
fd <- get_feature_data(r1, "Central" ,"", "Sitar")
autoplot(fd)

# Load all raw pose video data, process and filter it for one data point in one go
fv_list <- get_filtered_views(r1, data_points = 'Nose', n = 19, p = 4)

# Add the feature data to the list
fv_list$Feature <- fd

# Join all the video data together
jv <- get_joined_view(fv_list)
get_data_points(jv)

# Shows the feature data as part of the JoinedView ready for analysis
autoplot(jv, columns = 'Pitch_Feature')
autoplot(jv)

# Motiongram

# Look at Left and Right Elbow of Sitar player
fv3 <- apply_filter_sgolay(pv1, data_points = c("LElbow", "RElbow"), n = 41, p = 3)

# Position of Elbows for the first 100s
sub_fv3 <- subset(fv3, Time >= 0 & Time <= 100, by = 10)
plot_history_xy(sub_fv3)

# Use 1 minute of data and plot distribution of body over time
dp <- c("LWrist", "RWrist", "LElbow", "RElbow", "LEye", "REye", "Neck", "MidHip")
fv_body <- apply_filter_sgolay(pv1, data_point = dp, n = 41, p = 4)
sub_1min_fv_body <- subset(fv_body, Time >= 0 & Time <= 60)
distribution_dp(sub_1min_fv_body)

# Plot the absolute velocity of body
velocity_dp(sub_1min_fv_body)

# Plot Sitar motiongram for first minute
motion_gram(sub_1min_fv_body)

################################################################################
### Recording 2
################################################################################
r2 <- get_recording("NIR_DBh_Malhar_2Gats", fps = 25)
o2 <- get_onsets_selected_data(r2)
autoplot(o2)
m2 <- get_metre_data(r2)
d2 <- get_duration_annotation_data(r2)
autoplot(m2)
# 4 views
rv2_SideL_Guitar <- get_raw_view(r2, "SideL", "", "Guitar")
autoplot(rv2_SideL_Guitar, columns = c('LEar_x', 'LEar_y'))
rv2_SideL_Tabla <- get_raw_view(r2, "SideL", "", "Tabla")
autoplot(rv2_SideL_Tabla)
rv2_SideR_Tabla <- get_raw_view(r2, "SideR", "", "Tabla")
autoplot(rv2_SideL_Tabla)
rv2_SideR_Tabla <- get_raw_view(r2, "SideR", "", "Tabla")
autoplot(rv2_SideL_Tabla)

# OptFlow data has no camera in filename so load separately
rv2_OptFlow_Guitar <- get_raw_optflow_view(r2, "", "", "Guitar")
rv2_OptFlow_Tabla <- get_raw_optflow_view(r2, "", "", "Tabla")
pv2_OptFlow_Guitar <- get_processed_view(rv2_OptFlow_Guitar)
pv2_OptFlow_Tabla <- get_processed_view(rv2_OptFlow_Tabla)
fv2_OptFlow_Guitar <- apply_filter_sgolay(pv2_OptFlow_Guitar, "Head", n = 19, p = 4)
fv2_OptFlow_Tabla <- apply_filter_sgolay(pv2_OptFlow_Tabla, "Head", n = 19, p = 4)
autoplot(fv2_OptFlow_Guitar) # linear drift removed
autoplot(fv2_OptFlow_Tabla)

# Spectograms

# Use the Tabla player in recording NIR_DBh_Malhar_2Gats for analysis
pv_list <- get_processed_views(r2)

# Focus on RWrist on 10s window at 20 minutes
sub_pv <- subset(pv_list$SideL_Tabla, Time >= 60*20 & Time <= 60*20+10, columns = "RWrist_x")

# Estimate the spectral density of data points using spectrum in the stats package
spec <- spectral_density(sub_pv, columns = "RWrist_x", spans = c(3, 3))
autoplot(spec, period_range = c(0, 10))

# Specgram on processed data (colour) using signal::specgram function
sub_full_pv <- subset(pv_list$SideL_Tabla, Time <= 1700, columns = c("RWrist_x", "RWrist_y"))
specgram_plot(sub_full_pv)

# Specgram on filtered data (colour)
fv_tabla <- apply_filter_sgolay(pv_list$SideL_Tabla, data_points = "RWrist", n = 11, p = 3)
sub_full_fv <- subset(fv_tabla, Time <= 1700)
specgram_plot(sub_full_fv)

# Specgram on filtered data (bw)
specgram_plot(sub_full_fv, window = 200) +
  ggplot2::scale_fill_gradient(low = "white", high = "black")

# Filtered specgram with In time duration annotation data
specgram_plot(sub_full_fv) + autolayer(d2, geom = "vline", nudge_x = -10, size = 3)

# Unfiltered specgram with Out time duration annotation data
specgram_plot(sub_full_pv) + autolayer(d2, geom = "vline", nudge_x = -10, size = 3, vline_column = "Out")

# BW palette
specgram_plot(sub_full_pv, window = 200) +
  ggplot2::scale_fill_gradient(low = "white", high = "black") +
  autolayer(d2, geom = "vline", nudge_x = -10, size = 3, colour = "cyan")


################################################################################
###  Recording 3
################################################################################
r3 <- get_recording("NIRP1_MAK_Jaun", fps = 25)
o3 <- get_onsets_selected_data(r3)
autoplot(o3, instrument = 'Onset')
m3 <- get_metre_data(r3)
autoplot(m3)
d3 <- get_duration_annotation_data(r3)
autoplot(d3)
o3 <- get_onsets_selected_data(r3)
# 3 views
rv3_Cam1_Harmonium <- get_raw_view(r3, "Cam1", "", "Harmonium")
autoplot(rv3_Cam1_Harmonium)
rv3_Cam2_Singer <- get_raw_view(r3, "Cam2", "", "Singer")
autoplot(rv3_Cam2_Singer)
rv3_Cam2_Tabla <- get_raw_view(r3, "Cam2", "", "Tabla")
autoplot(rv3_Cam2_Tabla) # Ear only has one point on sampling

################################################################################
###  Recording 4
################################################################################
r4 <- get_recording("NIRP1_VS_Hams", fps = 25)
o4 <- get_onsets_selected_data(r4)
autoplot(o4, instrument = 'Onset')
m4 <- get_metre_data(r4)
autoplot(m4)
# 5 views
rv4_Central_Harmonium <- get_raw_view(r4, "Central", "", "Harmonium")
autoplot(rv4_Central_Harmonium)
rv4_Central_Singer <- get_raw_view(r4, "Central", "", "Singer")
autoplot(rv4_Central_Singer)
rv4_Central_Tabla <- get_raw_view(r4, "Central", "", "Tabla")
autoplot(rv4_Central_Tabla)
rv4_Central_TanpuraL <- get_raw_view(r4, "Central", "", "TanpuraL")
autoplot(rv4_Central_TanpuraL)
rv4_Central_TanpuraR <- get_raw_view(r4, "Central", "", "TanpuraR")
autoplot(rv4_Central_TanpuraR)

################################################################################
###  Recording 5
################################################################################
instruments <- c("Shoko_L", "Shoko_R", "Taiko", "Kakko", "Kakko_1", "So", "Biwa",
                 "Ryuteki", "Hichiriki", "Sho", "Biwa_RW", "Shoko_RW", "Taiko_LW",
                 "Taiko_RW")

r5 <- get_recording("Gagaku_5_Juha", fps = 60)
o5 <- get_onsets_selected_data(r5)
autoplot(o5, instrument = 'Hichiriki', tactus = 'SD_T')
m5 <- get_metre_data(r5)
autoplot(m5)
d5 <- get_duration_annotation_data(r5)

# 8 views - automation
rv_view <- get_raw_views(r5)
names(rv_view)
autoplot(rv_view$V1_M_Taiko) # Use maxpts = Inf to prevent sampling of rows
pv_view <- lapply(rv_view, get_processed_view)
str(pv_view, 2) # shows underlying data structure

# 8 filtered views
fv_view <- get_filtered_views(r5, data_points = "Nose", n = 19, p = 4)

# Layers using annotation and Metre objects
autoplot(fv_view$V3_Ryuteki)
autoplot(fv_view$V3_Ryuteki) + autolayer(d5, filter_expr = Tier == "Section")
autoplot(fv_view$V3_Ryuteki, time_limits = d5, time_expr = Tier == "Section" & Comments == "B") +
  autolayer(m5, time_limits = d5, time_expr = Tier == "Section" & Comments == "B")
autoplot(fv_view$V3_Ryuteki) + autolayer(o5, colour = "Inst.Name", fill = "", alpha = 0,
                                         instrument_cols = instruments)

# To compare data from different cameras use a JoinedView
two_cameras <- fv_view[c('V4_2_Biwa', 'V2_M_Taiko')]
jv <- get_joined_view(two_cameras)
autoplot(jv, time_breaks = 3)




