# QUERIES

# DONE
# Changed in naming on data files - NS to Pose - uploaded new data to Google drive
# Updated default onsetselected graphs, added autoplots
# Generated a reference beat time point from the mean
# Tabular summary functions: added simple summary function to analyze.wavelet - maybe output table as CSV?
# Rename Tier column as Segment in splicing tables
# difference based Inst, + others and add differences in time as columns
# mean sd, mean of absolute  from second object
# subset onset and summary on sections in annotation
# Added number of non NA rows in onset summaries
# Added clip_splice function to take in splice return a new windowed splice
# For sampling use two plots - originals vs samples or A vs B
# Added default data set to package
# Added a clip_slice function to generate splicing table from annotation data such that segments of equal length
# Added set operations on Splicing tables
# Lags in seconds in Granger code are rounded to nearerst frame
# Other signal filters can be applied to processed data now


# TODO
# displacement if there are three columns - add _z cordinate then calculate d
# adhoc feature data - check for displacement elsewhere? - email on that - turn into filteredView
# Infer wavelet the relative phase of two ts

# Splicing for proportion of interval on metre objects
# Option for arrows both ways on influencing plots
# Longer time spans for network diagram, - Filter on harmonium in network diagrams
# Look at some possible conditional granger implementations - modify lmtest?
# Limit colour to panels for GrangerTime - currently in script - improve function?
# wrist (faster) and nose focus as points -lag over a second, 2 or 3 seconds nose
# motiongram - displacement gives velocity
# * Added periodicity FFT plot - yet to do windowing
# grangers - extension - stats ?, vars, lmtest - tables - p number against time
# periodicity - restrict time domain, windowed 10s version
# overlay audio onto video - autolayers - test on last performance - stack the rects for inst?
# gganimate? https://rpubs.com/jedoenriquez/animatingchartsintro - on processed
# order onsetselected data
# Generalise autolayer to accept parameters or expr - done for Duration - others?

