# Get the data from OSF

rm(list=ls())
library(movementsync)

# List the recordings available at OSF
list_osf_recordings()

# Open the data home page at OSF
open_movementsync_data()

# Download the five Original recordings for the numbered walk-throughs
# get_osf_recordings()

# Download a particular recording
# get_osf_recordings("NIR_ABh_Puriya")

# Download a particular recording and force local overwrite
# get_osf_recordings("NIR_ABh_Puriya", overwrite = TRUE)

# Open the locally available downloaded recordings
open_local_recordings()

# Load the meta data for one recording from the local data
r <- get_recording("NIR_ABh_Puriya", fps = 25)
summary(r)
