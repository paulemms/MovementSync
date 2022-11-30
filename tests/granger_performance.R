rm(list=ls())
library(dplyr)
devtools::load_all(".")

# A recording with three instruments
r3 <- get_recording("NIRP1_MAK_Jaun", fps = 25)

# Join the views
fv3_list <- get_filtered_views(r3, data_points = "Nose", n = 41, p = 3)
jv3 <- get_joined_view(fv3_list)
autoplot(jv3)

# splice time based on duration object
d3 <- get_duration_annotation_data(r3)
splicing3_df <- splice_time(d3, expr = "Tier == 'Form'")
splicing3_df

# get a spliced view object
sv3 <- get_spliced_view(jv3, splicing_df = splicing3_df)
autoplot(sv3)

# Plot Granger Causality tests on each splice of data
system.time(g1 <- get_granger_interactions(sv3, c("Nose_x_Cam1_Harmonium", "Nose_x_Cam2_Singer",
                                       "Nose_x_Cam2_Tabla"), granger_fn = lmtest::grangertest))
system.time(g2 <- get_granger_interactions(sv3, c("Nose_x_Cam1_Harmonium", "Nose_x_Cam2_Singer",
                                            "Nose_x_Cam2_Tabla"), granger_fn = ms_grangertest1))
system.time(g3 <- get_granger_interactions(sv3, c("Nose_x_Cam1_Harmonium", "Nose_x_Cam2_Singer",
                                                  "Nose_x_Cam2_Tabla"), granger_fn = ms_grangertest2))

# Calls
system.time(granger_test(sv3, "Nose_x_Cam1_Harmonium", "Nose_x_Cam2_Singer"))
system.time(granger_test(sv3, "Nose_x_Cam2_Singer", "Nose_x_Cam2_Tabla"))
system.time(granger_test(sv3, "Nose_x_Cam2_Tabla", "Nose_x_Cam1_Harmonium"))

# Calls
unique(sv3$df$Tier) # second longest
df <- filter(sv3$df, Tier == 'vilambit rupak')
nrow(df)

# lmtest::grangertest taking 6s
system.time(lmtest::grangertest(df[["Nose_x_Cam1_Harmonium"]], df[["Nose_x_Cam2_Singer"]], order = 25))

# Copied implementation with zoo
system.time(ms_grangertest1(df[["Nose_x_Cam1_Harmonium"]], df[["Nose_x_Cam2_Singer"]], order = 25))

# Copied implementation with embed
system.time(ms_grangertest2(df[["Nose_x_Cam1_Harmonium"]], df[["Nose_x_Cam2_Singer"]], order = 25))
