# General setup
# Relative ranking stuff
library(looptimer)
library(maxgrowthcomparison)
library(parallel)
source("saver-loader.R") # call from within subdir

fpref <- "marinoi_series_examples_for_chapter-2021-04-22"

###################
# small number, shouldn't need but in case log(0)
epsilon <- 0.01
#
# multicore
if(!exists("cores")) cores <- detectCores() - 1
