# General setup
# Relative ranking stuff
library(looptimer)
library(maxgrowthcomparison)
library(parallel)
source("../saver-loader.R") # call from within subdir



###################
# small number, shouldn't need but in case log(0)
epsilon <- 0.01
#
# multicore
if(!exists("cores")) cores <- detectCores() - 1

# If not otherwise stated , do this many simulations per problem set
batch_size <- 100
# use regularly sampled observation times
TMAX <- 14
# 
makeid <- function(k, pref = "")  paste0("id_", pref, "_", substr(10000000+k, 2, 9))

##########################################################
# Parameters for the simulations.
set <- "run01"
source( sprintf("../setup-%s.R", set))


## Generic sampler of parameters
draw_parameters <- function(n = 1) {
  cbind(rate = runif(n, rates_limits[1], rates_limits[2]),
        lag = runif(n, lag_limits[1], lag_limits[2]),
        A = runif(n, A_limits[1], A_limits[2]),
        y0 = runif(n, nugget_limits[1], nugget_limits[2]))
}

######################################################
#
# Problems or challenges
#
# Setup for noise: log scale
sigmas <- c(.01, .15, .3)
# Simple independent noise
add_noise1 <- function(...) add_noise(..., log = TRUE, log1 = TRUE) 
#add_AR1_noise(..., rho = .5, log = TRUE, log1=TRUE)

# sampling frequency
freq_by_vector <- c(0.5, 1, 2)

# censoring
upto_vector <- c(TMAX, round(TMAX/2) )

# crash
crash_after_vector <- c(TMAX+1, round(TMAX/2))

## eof