rm(list = ls())
devtools::load_all()

# DONE
# Separate optflow function if no camera - inherits from RawView so processing works using
# existing functions
# for OSF data process to remove camera drift in process view


# TODO
# overlay audio onto video - autolayers - test on last performance
# * periodicity - look at github
# iii, iv - color code position of features
# order onsetselected data
# generalise filter
# Diagnostic duration plot?
# Generalise autolayer to accept parameters or expr
# dedicated zoo methods?
# gganimate? https://rpubs.com/jedoenriquez/animatingchartsintro - on processed
# Generalise interpolation methods

################################################################################
### Recording 1
################################################################################
r1 <- get_recording("NIR_ABh_Puriya", fps = 25)
summary(r1)

o1 <- get_onsets_selected_data(r1)
summary(o1)
plot(o1)
autoplot(o1)

m1 <- get_metre_data(r1)
summary(m1)
plot(m1)
autoplot(m1)

d1 <- get_duration_annotation_data(r1)
summary(d1)
# plot(d1)

rv1 <- get_raw_view(r1, "Central", "", "Sitar")
summary(rv1)
plot(rv1, nc = 3, maxpts = 500)
plot(rv1, columns = c("LEar_x", "LEar_y"))
plot(rv1, columns = c("Head_x", "Head_y", "Head_d")) # drift in camera
plot(rv1, xlim = c(500,1000), nc = 3)

# Autolayering with OnsetSelected, Metre and Duration objects,
autoplot(rv1, columns = c("LEar_x", "LEar_y"), maxpts = 2000)
autoplot(rv1, columns = c("LEar_x", "LEar_y")) + autolayer(d1)
autoplot(rv1, columns = c("LEar_x", "LEar_y")) + autolayer(o1)
autoplot(rv1, columns = c("LEar_x", "LEar_y")) + autolayer(d1, 'Tier == "FORM" & substr(Comments, 1, 1) == "J"')
autoplot(rv1, columns = c("LEar_x", "LEar_y"), maxpts=5000) + ggplot2::xlim(1000, 2000) +
  autolayer(m1)

# Set x-scale using Duration object
autoplot(rv1, columns = c("LEar_x", "LEar_y")) +
  xlim_duration(d1, 'Tier == "FORM" & Comments == "Alap"')

# Processed data
pv1 <- get_processed_view(rv1)
plot(pv1, nc = 3)
plot(pv1, columns = c("Head_x", "Head_y", "Head_d")) # Trend in Head removed
autoplot(pv1, columns = c("LEar_x", "LEar_y", "LEar_d")) # processed displacement added

# Filtered data
fv1 <- apply_filter(pv1, c("Nose", "RWrist", "LWrist"), window_size=19, poly_order=4)
summary(fv1)
plot(fv1, nc = 3)
autoplot(fv1)
autoplot(fv1) + autolayer(d1)

# Save the filtered objected in an RDS file
saveRDS(fv1, file = "C:/temp/fv1.rds")
rm(fv1)
fv1 <- readRDS("C:/temp/fv1.rds")
plot(fv1, nc = 3)

################################################################################
### Recording 2
################################################################################
r2 <- get_recording("NIR_DBh_Malhar_2Gats", fps = 25)
o2 <- get_onsets_selected_data(r2)
m2 <- get_metre_data(r2)
plot(m2)
# 4 views
rv2_SideL_Guitar <- get_raw_view(r2, "SideL", "", "Guitar")
plot(rv2_SideL_Guitar, nc=3)
rv2_SideL_Tabla <- get_raw_view(r2, "SideL", "", "Tabla")
plot(rv2_SideL_Tabla, nc=3)
rv2_SideR_Tabla <- get_raw_view(r2, "SideR", "", "Tabla")
plot(rv2_SideL_Tabla, nc=3)
rv2_SideR_Tabla <- get_raw_view(r2, "SideR", "", "Tabla")
plot(rv2_SideL_Tabla, nc=3)

# OptFlow data has no camera in filename so load separately
rv2_OptFlow_Guitar <- get_raw_optflow_view(r2, "", "Guitar")
rv2_OptFlow_Tabla <- get_raw_optflow_view(r2, "", "Tabla")
pv2_OptFlow_Guitar <- get_processed_view(rv2_OptFlow_Guitar)
pv2_OptFlow_Tabla <- get_processed_view(rv2_OptFlow_Tabla)
fv2_OptFlow_Guitar <- apply_filter(pv2_OptFlow_Guitar, "Head", window_size=19, poly_order=4)
fv2_OptFlow_Tabla <- apply_filter(pv2_OptFlow_Tabla, "Head", window_size=19, poly_order=4)
plot(fv2_OptFlow_Guitar) # linear drift removed
plot(fv2_OptFlow_Tabla)

################################################################################
###  Recording 3
################################################################################
r3 <- get_recording("NIRP1_MAK_Jaun", fps = 25)
o3 <- get_onsets_selected_data(r3)
m3 <- get_metre_data(r3)
plot(m3)
o3 <- get_onsets_selected_data(r3)
# 3 views
rv3_Cam1_Guitar <- get_raw_view(r3, "Cam1", "", "Harmonium")
plot(rv3_Cam1_Guitar, nc=3)
rv3_Cam2_Singer <- get_raw_view(r3, "Cam2", "", "Singer")
plot(rv3_Cam2_Singer, nc=3)
rv3_Cam2_Tabla <- get_raw_view(r3, "Cam2", "", "Tabla")
plot(rv3_Cam2_Tabla, nc=3) # Ear only has one point on sampling

################################################################################
###  Recording 4
################################################################################
r4 <- get_recording("NIRP1_VS_Hams", fps = 25)
o4 <- get_onsets_selected_data(r4)
m4 <- get_metre_data(r4)
plot(m4)
# 5 views
rv4_Central_Harmonium <- get_raw_view(r4, "Central", "", "Harmonium")
plot(rv4_Central_Harmonium, nc=3)
rv4_Central_Singer <- get_raw_view(r4, "Central", "", "Singer")
plot(rv4_Central_Singer, nc=3)
rv4_Central_Tabla <- get_raw_view(r4, "Central", "", "Tabla")
plot(rv4_Central_Tabla, nc=3)
rv4_Central_TanpuraL <- get_raw_view(r4, "Central", "", "TanpuraL")
plot(rv4_Central_TanpuraL, nc=3)
rv4_Central_TanpuraR <- get_raw_view(r4, "Central", "", "TanpuraR")
plot(rv4_Central_TanpuraR, nc=3)

################################################################################
###  Recording 5
################################################################################
r5 <- get_recording("Gagaku_5_Juha", fps = 60)
o5 <- get_onsets_selected_data(r5)
plot(o5, column = "Time") # Data format different
m5 <- get_metre_data(r5)
autoplot(m5)
# 8 views - automation
rv_view <- get_raw_views(r5)
names(rv_view)
plot(rv_view$V1_M_Taiko, nc = 3)
View(rv_view)

pv_view <- lapply(rv_view, get_processed_view)
fv_view <- lapply(pv_view, apply_filter, data_points=c("Nose", "RWrist", "LWrist"), window_size=19, poly_order=4)
