# QUERIES

# DONE
# Updated default onsetselected graphs, added autoplots
# Generated a reference beat time point from the mean
# Tabular summary functions: added simple summary function to analyze.wavelet - maybe output table as CSV?
# Rename Tier column as Segment in splicing tables
# difference based Inst, + others and add differences in time as columns
# mean sd, mean of absolute  from second object
# subset onset and summary on sections in annotation
# Added number of non NA rows in onset summaries
# Added clip_splice function to take in splice return a new windowed splice
# two plots - originals vs samples or A vs B -

# TODO
# displacement if there are three columns - add _z cordinate then calculate d
# adhoc feature data - check for displacement elsewhere? - email on that - turn into filteredView

# Splicing for proportion of interval on metre objects
# Generate splicing table from annotation data such that segments of equal length
# Add default data set
# ensure we treat granger test data as time series so lags computed correctly
# apply type functions to SplicedView - return list of objects from function output OK
# set operations on the SplicedViews? - see generics package for setops
# steal sync_sample_paired from onsetsync - see email
# find out what it does on the lag with regard to frames
# option for arrows both ways
# Longer time spans for network diagram, - Filter on harmonium in network diagrams
# Look at some possible conditional granger implementations
# Added in duration annotation CSV for NIRP1_MAK_Jaun - add autoplot - improve?
# Limit colour to panels for GrangerTime - currently in script - improve function?
# conditional granger time in R packages ? - modify lmtest
# infer wavelet the relative phase of two ts
# maybe some other filters?
# wrist (faster) and nose focus as points -lag over a second, 2 or 3 seconds nose
# motiongram - displacement gives velocity
# * Added periodicity FFT plot - yet to do windowing
# grangers - extension - stats ?, vars, lmtest - tables - p number against time
# periodicity - restrict time domain, windowed 10s version
# overlay audio onto video - autolayers - test on last performance - stack the rects for inst?
# gganimate? https://rpubs.com/jedoenriquez/animatingchartsintro - on processed
# order onsetselected data
# Generalise autolayer to accept parameters or expr
# Generalise interpolation methods
# https://stackoverflow.com/questions/68022639/combining-time-trend-plot-with-timeline
# dedicated zoo methods?

