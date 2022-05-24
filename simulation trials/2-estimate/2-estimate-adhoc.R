# Estimate  relative growth rate  non-parametrically
library(looptimer)
source("0-setup.R")
source("adhoc_methods.R")

# Assuming lu and curves loaded
# need fpref!
fn1 <- paste0(fpref, "_fits_adhoc.rds")

if(length(dirrer(fn1))== 0){

  # Fine grid for max rate estimation
  xg <- seq(0, TMAX, l = 100)
  
  ests <- NULL
  
  # Fit all
  models <- tolower( names(adhoc_rrates) )
  
  t0 <- looptimer(n = nrow(lu), pref = "[adhoc]", printevery = 200)
  for(i in 1:nrow(lu)){
    id <- lu$id[i]
    xy <- curves[[id]]
    # go for all non-smooths
    mrs <- sapply(models, function(mod)adhoc_rrates[[mod]](xy[,1], xy[,2]))
    df1 <- data.frame(id=id, estimator = models, rate = mrs,
                      row.names = NULL, stringsAsFactors = FALSE)
    ests <- rbind(ests, df1)
  
    # smoothed curve
    #w <- smooth.spline(xy[,1], xy[,2], spar=.35)$y
    #w <- smooth.spline(xy[,1], xy[,2], control.spar = list(low = 0.3, high = 0.8))$y
    #w <- smooth(xy[,2])
    w <- runmed(xy[,2], 3)
    #w[w<0] <- xy[,2][w<0]
  
    # go for all smoothed
    mrs <- sapply(models, function(mod)adhoc_rrates[[mod]](xy[,1], w))
    df1 <- data.frame(id=id, estimator = paste0(models, "_med3"), rate = mrs,
                      row.names = NULL, stringsAsFactors = FALSE)
    ests <- rbind(ests, df1)
    #
    print ( t0 <- looptimer(t0) )
  }
  summary(t0)
  saver(list(ests=ests, lookup = lu), fn1)
}
