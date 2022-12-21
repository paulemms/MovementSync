# Load sample data (inc. 3D test version)

rm(list=ls())
gc()
if (dev.cur() > 1) dev.off()
library(movementsync)

# Load sample data from package: only 1 min of video data
r1 <- get_sample_recording() # defaults to NIR_ABh_Puriya
summary(r1)
rv <- get_raw_view(r1, "Central", "", "Sitar")
autoplot(rv)

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


