# Get the dataset of five recordings from from OSF and install them
# into a folder on your local machine (by default ~/movementsync/Original)

rm(list=ls())
library(movementsync)

osf_node <- "https://osf.io/w2s3a"

# List the recordings available at OSF
list_osf_recordings(node = osf_node)

# Open the data home page at OSF
open_movementsync_data(node = osf_node)

# Download the five recordings for the numbered walk-throughs
# get_osf_recordings(c("NIR_ABh_Puriya", "NIRP1_VS_Hams", "NIRP1_MAK_Jaun", "Gagaku_5_Juha", "NIR_DBh_Malhar"), node = osf_node)

# Download a particular recording
# get_osf_recordings("NIR_ABh_Puriya", node = osf_node)

# Download a particular recording and force local overwrite
# get_osf_recordings("NIR_ABh_Puriya", node = osf_node, overwrite = TRUE)

# Open the locally available downloaded recordings
open_local_recordings()

# Load the meta data for one recording from the local data
r <- get_recording("NIR_ABh_Puriya", fps = 25)
summary(r)

# The Recording object is used by other functions in movementsync to load data.
