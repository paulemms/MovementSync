# Test CRAN packaging

rm(list=ls())
devtools::load_all()

# Load sample data from package: Test data contains z-cordinates in view data
r1 <- get_sample_recording("Test")
summary(r1)

# Load video data with z-cordinates on some data points
rvz <- get_raw_view(r1, "Central", "", "z")
get_data_points(rvz)
pvz <- get_processed_view(rvz)
head(pvz$df)
fvz <- apply_filter_sgolay(pvz, data_points = c('LEar', 'LElbow', 'LEye'), n = 19, p = 4)
head(fvz$df)

# Run the examples in the package
devtools::run_examples()


