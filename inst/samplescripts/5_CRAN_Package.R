# Test CRAN packaging

rm(list=ls())
library(movementsync)

# Load sample data from package: only 1 min of video/feature data
r1 <- get_sample_recording() # defaults to NIR_ABh_Puriya
summary(r1)

# Load feature data (by default it is not made continuous)
fd <- get_feature_data(r1, "Central" ,"", "Sitar")
pv_list <- get_processed_views(r1)
pv_list$Feature <- fd
jv <- get_joined_view(pv_list)
get_data_points(jv)
jv_sub <- subset(jv, data_points = c('LEar', 'Pitch', 'Smooth'))
autoplot(jv_sub)

# Applying SG filter can re-introduce interior NAs if the series starts with NAs
# So filters are only applied on interval where time series starts and ends
rv <- get_raw_view(r1, "Central" ,"", "Sitar")
autoplot(rv, columns = 'LEar_x')
pv <- get_processed_view(rv)
autoplot(pv, columns = 'LEar_x')
fv <- apply_filter_sgolay(pv, data_points = 'LEar', n = 41, p = 3)
autoplot(fv, columns = 'LEar_x')

# Load sample data from package: Test data contains z-cordinates in view data
r2 <- get_sample_recording("Test")
summary(r2)

# Load video data with z-cordinates on some data points
rvz <- get_raw_view(r2, "Central", "", "z")
get_data_points(rvz)
pvz <- get_processed_view(rvz)
head(pvz$df)
fvz <- apply_filter_sgolay(pvz, data_points = c('LEar', 'LElbow', 'LEye'), n = 19, p = 4)
head(fvz$df)

# Check the package for CRAN
# devtools::check()


