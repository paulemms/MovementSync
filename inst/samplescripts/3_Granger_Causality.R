# Introduces functions for granger causality and conditional granger causality
# analysis, including windowed analysis with p-values and arrows
# (influence diagrams), and summary network diagrams.

rm(list=ls())
gc()
if (dev.cur() > 1) dev.off()
library(movementsync)

# Get recording meta data
r1 <- get_recording("NIR_ABh_Puriya", fps = 25)

# Get filtered views
fv_list <- get_filtered_views(r1, data_points = "Nose", n = 41, p =3)

# Put individual ts into one data frame using a JoinedView
jv <- get_joined_view(fv_list)
autoplot(jv)

# Function to take jv and generated a spliced_df
splicing_df <- splice_time(jv, win_size = 30, step_size = 5)
head(splicing_df)

# Apply spliced_df to jv_sub
sv <- get_spliced_view(jv, splicing_df)

# It will have a lot of sub-divisions so autoplot only shows first 10 segments
autoplot(sv)

# Apply granger_test to each Segment in spliced_df using 1s lag
# (full recording so will take a while it do all the linear regressions on each window)
# NULL hypothesis is Tabla doesn't influence Sitar, small p-value rules this out
g <- granger_test(sv, "Nose_x_Central_Sitar", "Nose_x_Central_Tabla", lag = 1)
str(g, 2)

# p-value plot both forward and backwards
autoplot(g, splicing_df = splicing_df)

# overlay with annotation data
d1 <- get_duration_annotation_data(r1)
autoplot(g, splicing_df = splicing_df) +
  autolayer(d1, filter_expr = Tier == "FORM", fill_col = "Comments")

# Now subset the time line to first 10 minutes and redo Granger Tests to speed things up
jv_sub <- subset(jv, Time <= 10*60)
autoplot(jv_sub)
splicing_df <- splice_time(jv_sub, win_size = 30, step_size = 5)
sv_sub <- get_spliced_view(jv_sub, splicing_df)
g <- granger_test(sv_sub, "Nose_x_Central_Sitar", "Nose_x_Central_Tabla", lag = 1)

# p-value plot with influence colouring from annotation from duration data
autoplot(g, splicing_df = splicing_df) +
  autolayer(d1, filter_expr = (Tier == "Influence S>T" | Tier == "Influence T>S") & Out < 600,
            fill_col = "Tier")

# p-value plot with colours highlighting comment group
autoplot(g, splicing_df = splicing_df) +
  autolayer(d1, filter_expr = (Tier == "Influence S>T" | Tier == "Influence T>S") & Out < 600,
            fill_col = "Comments")

# p-value plot with influence colouring mapping to relevant facet
d1_mapped <- map_to_granger_test(d1, g, "Influence T>S", "Influence S>T")
autoplot(g, splicing_df = splicing_df) +
  autolayer(d1_mapped, filter_expr = (Tier == "Influence S>T" | Tier == "Influence T>S") & Out < 600,
            fill_col = "Tier")

# Data for arrows showing causality direction

# Difference in log10(p_values) if one significant
plot_influence_diagram(g, splicing_df = splicing_df)

# -log10(p-Value) if significant
plot_influence_diagram(g, splicing_df = splicing_df, two_arrows = TRUE)

# Influence diagram with duration autolayer superimposed using Tier column
plot_influence_diagram(g, splicing_df = splicing_df) +
  autolayer(d1, filter_expr = (Tier == "Influence S>T" | Tier == "Influence T>S") & Out < 600,
            fill_col = "Tier")

# Influence diagram with duration autolayer superimposed using Comment column
plot_influence_diagram(g, splicing_df = splicing_df) +
  autolayer(d1, filter_expr = (Tier == "Influence S>T" | Tier == "Influence T>S") & Out < 600,
            fill_col = "Comments")

# Splice time using Tier for larger time intervals
splicing_tier_df <- splice_time(d1, tier = 'FORM')
splicing_tier_df

# apply splicing_df to full joined view
sv_tier <- get_spliced_view(jv, splicing_tier_df)
autoplot(sv_tier, time_breaks = 3)

# Granger Causality Test on each Tiered time slice
g_tier <- granger_test(sv_tier, "Nose_x_Central_Sitar", "Nose_x_Central_Tabla", lag = 1)
autoplot(g_tier, splicing_df = splicing_tier_df) +
  autolayer(d1, fill_col = "Comments")

# Data for arrows showing causality direction
plot_influence_diagram(g_tier, splicing_df = splicing_tier_df) +
  autolayer(d1, fill_col = "Comments")

# Use igraph to illustrate causality on time segments
gi <- get_granger_interactions(sv_tier, c("Nose_x_Central_Sitar", "Nose_x_Central_Tabla"))
plot(gi, mfrow = c(2, 3)) # overloaded plot command

# further arguments are passed through to igraph - see ?igraph.plotting
plot(gi, edge.width = 1, edge.label.cex = 3)

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
autoplot(sv3, time_breaks = 3)

# Plot Granger Causality tests on each splice of data
gi3 <- get_granger_interactions(sv3, c("Nose_x_Cam1_Harmonium", "Nose_x_Cam2_Singer",
                                 "Nose_x_Cam2_Tabla"))
plot(gi3)

# Retrieve first test results from gi3
t1 <- gi3$gc_list$`Nose_x_Cam1_Harmonium <--> Nose_x_Cam2_Singer`
autoplot(t1, splicing_df = splicing3_df) +
  autolayer(d3, filter_expr = Tier == "Form", fill_col = "Comments")

# Focus on Harmonium time slice Events
harmonium_splicing_df <- splice_time(
  d3, expr = "Tier == 'Event' & grepl('harmonium', Comments, fixed=TRUE)")
harmonium_splicing_df

# Plot Granger Causality interactions
harmonium_sv <- get_spliced_view(jv3, splicing_df = harmonium_splicing_df)
autoplot(harmonium_sv, time_breaks = 2)
harmonium_gi <- get_granger_interactions(
  harmonium_sv, c("Nose_x_Cam1_Harmonium", "Nose_x_Cam2_Singer", "Nose_x_Cam2_Tabla"))
plot(harmonium_gi)
plot(harmonium_gi, edge.arrow.size = 0.3, vertex.color = "yellow")

# Conditional Granger Causality

# Granger causality tests for Harmonium and Singer conditional on Tabla player
# on intervals where Harmonium is solo
g_cond <- granger_test(harmonium_sv, "Nose_x_Cam1_Harmonium", "Nose_x_Cam2_Singer", "Nose_x_Cam2_Tabla",
                      lag = 1)
g_cond$df # P_Value table

# P-value plot with a layer
autoplot(g_cond, splicing_df = harmonium_splicing_df) +
  autolayer(d3, time_limit = c(8*60, Inf), filter_expr = Tier == "Form")

# Conditional influence diagram
plot_influence_diagram(g_cond, splicing_df = harmonium_splicing_df) +
  autolayer(d3, time_limit = c(8*60, Inf), filter_expr = Tier == "Form")

# Interactions between Harmonium and Singer
harmonium_singer <- get_granger_interactions(
  harmonium_sv, c("Nose_x_Cam1_Harmonium", "Nose_x_Cam2_Singer"))
plot(harmonium_singer)

# Now conditional on Tabla player
harmonium_gci <- get_granger_interactions(
  harmonium_sv, c("Nose_x_Cam1_Harmonium", "Nose_x_Cam2_Singer"),
  cond_column = "Nose_x_Cam2_Tabla")
plot(harmonium_gci)




