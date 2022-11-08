rm(list = ls())
devtools::load_all()

# add displacement - what happens to the displace on normalisation?
# overlay audio onto video

# Recording 1
r1 <- get_recording("NIR_ABh_Puriya", fps = 25)
summary(r1)

o1 <- get_onsets_selected_data(r1)
summary(o1)

m1 <- get_metre_data(r1)
summary(m1)
plot(m1)

d1 <- get_duration_annotation_data(r1) # Form == Harp
summary(d1)

rv1 <- get_raw_view(r1, "Central", "", "Sitar")
summary(rv1)
plot(rv1)
plot(rv1, cols = c("LEar_x", "LEar_y"))
plot(rv1, xlim = c(500,1000))
# function to extract data from d1 and use it to subset rv1 then plot?

pv1 <- get_processed_view(rv1)
plot(pv1)

fv1 <- apply_filter(pv1, c("Nose", "RWrist", "LWrist"), window_size=19, poly_order=4)
summary(fv1)
plot(fv1)

# Save the filtered objected in an RDS file (maybe convenience filename generator)
saveRDS(fv1, file = "C:/temp/fv1.rds")
rm(fv1)
devtools::load_all()
fv1 <- readRDS("C:/temp/fv1.rds")
plot(fv1)
# or save function for each object to CSVs/JSON - need reads too

# Recording 2
r2 <- get_recording("NIR_DBh_Malhar_2Gats", fps = 25)
o2 <- get_onsets_selected_data(r2)
m2 <- get_metre_data(r2) # 1.1, 1.2 for cycle column ???
plot(m2)

# Recording 3
r3 <- get_recording("NIRP1_MAK_Jaun", fps = 25)
o3 <- get_onsets_selected_data(r3)
m3 <- get_metre_data(r3)
plot(m3)

# Recording 4
r4 <- get_recording("NIRP1_VS_Hams", fps = 25)
o4 <- get_onsets_selected_data(r4)
m4 <- get_metre_data(r4)
plot(m4)

# Recording 5
r5 <- get_recording("Gagaku_5_Juha", fps = 60)
o5 <- get_onsets_selected_data(r5)
m5 <- get_metre_data(r5) # no data

