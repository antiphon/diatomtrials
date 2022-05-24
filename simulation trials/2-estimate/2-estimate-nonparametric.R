# Estimate  growth rate  non-parametrically
# Relative rate version!!!

library(fANCOVA)
library(parallel)
source("0-setup.R")

fn1 <- paste0(fpref, "_fits_np.rds")

if(length(dirrer(fn1))== 0){
  # Fine grid for max rate estimation
  ests <- NULL
  epsilon <- 0.01
  blocks <- split_block(lu$id, cores)
  t0 <- looptimer(n = length(blocks), pref = "[non-parameric]", printevery = 200)
  
  for(bl in blocks){
    estl <- mclapply(bl, function(id) {
      xy <- curves[[id]]
      xg <- seq(0, TMAX, l = 500)
      # dont extrapolate
      xg <- xg[ xg <= max(xy[,1]) & xg >= min(xy[,1])]
      dx <- diff(xg[1:2])
      ### log scale. something for the negatives, dont drop since sparse data (need n>3 for methods)
      zap <- (xy[,2] <= 0)
      if(any(zap)) xy[zap, 2] <- epsilon
      xy[,2] <- log(xy[,2])
      # cubic splines
      fit <- smooth.spline(xy[,1], xy[,2], cv = FALSE)
      yg <- predict(fit, xg)$y
      # Estimate max relative
      mr1 <- max( diff(yg)/dx, na.rm=TRUE)
      # loess CV
      fit <- loess.as(xy[,1], xy[,2], criterion = "gcv")
      yg <- predict(fit, xg)
      mr2 <- max( diff(yg)/dx, na.rm=TRUE)
      # loess AIC
      fit <- loess.as(xy[,1], xy[,2], criterion = "aicc")
      yg <- predict(fit, xg)
      mr3 <- max( diff(yg)/dx, na.rm=TRUE)
      c(mr1, mr2, mr3)
    })
    mods <- c("spline gcv", "loess gcv", "loess AIC")
    ests <- rbind(ests, data.frame(id = rep(bl, each=length(mods)),
                                   estimator = mods, 
                                   rate = unlist(estl))  )
    print( t0 <- looptimer(t0) )
  }
  summary(t0)
  saver(list(ests=ests, lookup = lu), fn1)
}
