# QUERY

# how to handle replacement with movement sampling? regression then draw random times?
#  - lots of ways to do bootstrapping - package?
# Can the spliced time intervals overlap - if so then multi-value time series might not be desirable


# DONE
# Added simple summary function to analyze.wavelet
# Sample 100 rows from a number of split views with/without replacement
# - returned List of sample in each Tier
# Segements need to be unique for Granger Causality Tests

# TODO
# add default data set
# ensure we treat granger test data as time series so lags computed correctly
# apply type functions to SplicedView - return list of objects from function output OK
# set operations on the SplicedViews? - see generics package for setops
# steal sync_sample_paired from onsetsync - see email
# find out what it does on the lag with regard to frames
# option for arrows both ways
# Longer time spans for network diagram, - Filter on harmonium in network diagrams
# Look at some possible conditional granger implementations
# Added in duration annotation CSV for NIRP1_MAK_Jaun - add autoplot
# summary functions for data objects - add to summary.R
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
# Make the scripts in test/ reflect sections of the spec

