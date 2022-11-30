# Test we can do Granger Tests
rm(list=ls())
devtools::load_all(".")

# Get recording meta data
r1 <- get_recording("NIR_ABh_Puriya", fps = 25)

# Get filtered views
fv_list <- get_filtered_views(r1, data_points = "Nose", n = 41, p =3)

# Put individual ts into one data frame using a JoinedView
jv <- get_joined_view(fv_list)
autoplot(jv)

# Get duration annotation object
d1 <- get_duration_annotation_data(r1)

# Subset the time line to first 10 minutes
jv_sub <- subset(jv, Time <= 10*60)
autoplot(jv_sub)

# Function to take jv_sub and generated a spliced_df
splicing_df <- splice_time(jv_sub, win_size = 30, step_size = 5)
splicing_df

# Apply spliced_df to jv_sub
sv <- get_spliced_view(jv_sub, splicing_df)

# It will have a lot of sub-divisions so autoplot only shows first 10
autoplot(sv)

# Apply granger_test to each Tier in spliced_df using 0.1s lag, then 1s
g <- granger_test(sv, "Nose_x_Central_Sitar", "Nose_x_Central_Tabla", lag = 0.1)
g <- granger_test(sv, "Nose_x_Central_Sitar", "Nose_x_Central_Tabla", lag = 1)
g

# p-value plot both forward and backwards
autoplot(g, splicing_df = splicing_df)

# p-value plot with annotation from duration data
autoplot(g, splicing_df = splicing_df) +
  autolayer(d1, '(Tier == "Influence S>T" | Tier == "Influence T>S") & Out < 600',
            fill_col = "Tier")

autoplot(g, splicing_df = splicing_df) +
  autolayer(d1, '(Tier == "Influence S>T" | Tier == "Influence T>S") & Out < 600',
            fill_col = "Comments")

# Data for arrows showing causality direction
plot_influence_diagram(g, splicing_df = splicing_df)

# Influence diagram with duration autolayer superimposed using Tier column
plot_influence_diagram(g, splicing_df = splicing_df) +
  autolayer(d1, '(Tier == "Influence S>T" | Tier == "Influence T>S") & Out < 600',
            fill_col = "Tier")

# Influence diagram with duration autolayer superimposed using Comment column
plot_influence_diagram(g, splicing_df = splicing_df) +
  autolayer(d1, '(Tier == "Influence S>T" | Tier == "Influence T>S") & Out < 600',
            fill_col = "Comments")

# Splice time using Tier for larger time intervals
splicing_tier_df <- splice_time(d1, tier = 'FORM')
splicing_tier_df

# apply splicing_df to full joined view
sv_tier <- get_spliced_view(jv, splicing_tier_df)
autoplot(sv_tier)

# Granger Causality Test on each Tiered time slice
g_tier <- granger_test(sv_tier, "Nose_x_Central_Sitar", "Nose_x_Central_Tabla", lag = 1)
autoplot(g_tier, splicing_df = splicing_tier_df) +
  autolayer(d1, 'Tier == "FORM"', fill_col = "Comments")

# Data for arrows showing causality direction
plot_influence_diagram(g_tier, splicing_df = splicing_tier_df) +
  autolayer(d1, 'Tier == "FORM"', fill_col = "Comments")

# Use igraph to illustrate causality on time segments
gi <- get_granger_interactions(sv_tier, c("Nose_x_Central_Sitar", "Nose_x_Central_Tabla"))
plot(gi)

# Now use a recording with three instruments
r3 <- get_recording("NIRP1_MAK_Jaun", fps = 25)

# Join the views
fv3_list <- get_filtered_views(r3, data_points = "Nose", n = 41, p = 3)
jv3 <- get_joined_view(fv3_list)
autoplot(jv3)

# splice time based on duration object
d3 <- get_duration_annotation_data(r3)
splicing3_df <- splice_time(d3, tier = 'Form')
splicing3_df

# get a spliced view object
sv3 <- get_spliced_view(jv3, splicing_df = splicing3_df)
autoplot(sv3)

# Plot Granger Causality tests on each splice of data
gi3 <- get_granger_interactions(sv3, c("Nose_x_Cam1_Harmonium", "Nose_x_Cam2_Singer",
                                 "Nose_x_Cam2_Tabla"))
plot(gi3)

# Retrieve first test results from gi3
t1 <- gi3$gc_list$`Nose_x_Cam1_Harmonium <--> Nose_x_Cam2_Singer`
autoplot(t1, splicing_df = splicing3_df) +
  autolayer(d3, 'Tier == "Form"', fill_col = "Comments")

# Focus on Harmonium time slice Events
harmonium_splicing_df <- splice_time(
  d3, expr = "Tier == 'Event' & grepl('harmonium', Comments, fixed=TRUE)")
harmonium_splicing_df

# Plot Granger Causality interactions
harmonium_sv <- get_spliced_view(jv3, splicing_df = harmonium_splicing_df)
autoplot(harmonium_sv)
harmonium_gi <- get_granger_interactions(
  harmonium_sv, c("Nose_x_Cam1_Harmonium", "Nose_x_Cam2_Singer", "Nose_x_Cam2_Tabla"))
plot(harmonium_gi)
plot(harmonium_gi, edge.arrow.size = 0.3, vertex.color = "yellow")
