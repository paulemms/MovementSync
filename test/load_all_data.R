devtools::load_all()

# Recording 1
r1 <- get_recording("NIR_ABh_Puriya", fps = 25)
summary(r1)

o1 <- get_onsets_selected_data(r1) # overlays
summary(o1)

m1 <- get_metre_data(r1) # convert to intervals? order diff
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
# add displacement

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
summary(r2)

o2 <- get_onsets_selected_data(r2)
summary(o2)

m2 <- get_metre_data(r2)
summary(m2)
plot(m2) # order

# Recording 3
r3 <- get_recording("NIRP1_MAK_Jaun", fps = 25)
summary(r3)

# Recording 4
r4 <- get_recording("NIRP1_VS_Hams", fps = 25)
summary(r4)

# Recording 5
r5 <- get_recording("Gagaku_5_Juha", fps = 60)
summary(r5)

