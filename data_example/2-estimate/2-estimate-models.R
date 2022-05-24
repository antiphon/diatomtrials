# Estimate parameters
library(looptimer)
source("0-setup.R")
source("adhoc_methods.R") # for max diff

# Assuming lu and curves loaded
# need fpref!
fn1 <- paste0(fpref, "_fits_models.rds")


models <- c("logistic", "gompertz", "peleg", "baranyi") # tolower( names(curve_models) )

## Optimisation boundaries
l0 <- c(nugget = 0, rate = 0.01, lag = 0, A = 1)
u0 <- c(nugget = 10, rate = 5,   lag = 14, A = 9)
lower <- list(logistic = l0, 
              gompertz = l0,
              peleg    = c(l0, nrate=-5, nlag=0),
              baranyi  = c(l0[-1], y0 = 0.01))
upper <- list(logistic = u0, 
              gompertz = u0,
              peleg    = c(u0, 0.0001, 25),
              baranyi  = c(u0[-1], y0 = u0[1]))
# start in the middle
start <- lapply(names(lower), function(v) rowMeans(cbind(lower[[v]], upper[[v]])))
names(start) <- names(upper)
###


if(length(dirrer(fn1))== 0){
  ests <- NULL
  # Fit these modelsi
  t0 <- looptimer(n = nrow(lu), pref = sprintf("[%s models]", set), printevery = 200)
  
  for(i in 1:nrow(lu)){
    id <- lu$id[i]
    xy <- curves[[id]]
    TMAX <- max(xy[,1])
    xdense <- seq(0, TMAX, l = 200)
    
    
    mrs <- sapply(models, function(m){
      est <- curve_fit(xy[,1], xy[,2], 
                       model = m, 
                       nugget = TRUE, 
                       lower = lower[[m]], 
                       upper = upper[[m]], 
                       start = start[[m]],
                       logscale = TRUE)
      # use dense prediction to estimate rrate
      ydense <- curve_predict(xdense, est)
      adhoc_max_diff_rel(xdense, ydense)
    })
    ests <- rbind(ests, data.frame(id = id, 
                                   estimator = models, 
                                   rate = mrs,
                                   row.names = NULL, stringsAsFactors = FALSE))
    print ( t0 <- looptimer(t0) )
  }
  summary(t0)
  saver(list(ests=ests, lookup = lu), fn1)
}