rm(list=ls())
library(lmtest)
library(dplyr)
devtools::load_all(".")

# Get recording meta data
r1 <- get_recording("NIR_ABh_Puriya", fps = 25)

# Get raw views
rv_list <- get_raw_views(r1)
pv_list <- lapply(rv_list, get_processed_view)
get_data_points(pv_list$Central_Sitar)
fv_list <- lapply(pv_list, apply_filter_sgolay, data_points = "Nose", n = 41, p = 3)

# Put individual ts into one data frame using a JoinedView
jv <- get_joined_view(fv_list)
plot(jv, yax.flip=TRUE)

# Subset the time line
jv_sub <- subset(jv, Time <= 10*60)
plot(jv_sub, yax.flip=TRUE)

# Do Grangers - time domain
grangertest(Nose_x_Central_Sitar ~ Nose_x_Central_Tabla, order = 3, data = jv_sub$df)
grangertest(Nose_x_Central_Tabla ~ Nose_x_Central_Sitar, order = 3, data = jv_sub$df)

# function to take jv_sub and generated a spliced_df
splicing_df <- splice_time(jv_sub, win_size = 3, step_size = 0.5)

# apply spliced_df to jv_sub
sv <- get_spliced_view(jv_sub, splicing_df)
nrow(sv$df) # because overlapping

# it will have a lot of sub-divisions so to view each piece need to subset
sv_sub <- subset(sv, Tier %in% paste0('w', 1:5))
colnames(sv$df)
colnames(sv_sub$df)
autoplot(sv_sub)

# apply grangertest to each Tier in spliced_df
l1 <- sv_sub$df %>%
  select(Tier, Nose_x_Central_Sitar, Nose_x_Central_Tabla) %>%
  group_by(Tier) %>%
  group_map(~ grangertest(.['Nose_x_Central_Sitar'], .$Nose_x_Central_Tabla, order = 3))
l2 <- sv_sub$df %>%
  select(Tier, Nose_x_Central_Sitar, Nose_x_Central_Tabla) %>%
  group_by(Tier) %>%
  group_map(~ grangertest(Nose_x_Central_Tabla ~ Nose_x_Central_Sitar, order = 3, data = .))

sapply(l1, function(x) x[['Pr(>F)']][2])
sapply(l2, function(x) x[['Pr(>F)']][2])

# visualise p values from each Tier both forward and backwards - horizontal line for critical p
# to reject null hypothesis
