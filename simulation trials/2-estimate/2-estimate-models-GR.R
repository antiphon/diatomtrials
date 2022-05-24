# Estimate parameters using growthrates-package
# Estimate parameters
library(looptimer)
library(growthrates)
source("0-setup.R")

source("adhoc_methods.R") # for max diff

# Assuming lu and curves loaded
# need fpref!
fn1 <- paste0(fpref, "_fits_modelsGR.rds")


models <- c("baranyi") # tolower( names(curve_models) )
xdense <- seq(0, TMAX, l = 200)
xdense_df <- data.frame(time = xdense)


l0 <- c(nugget = 0, rate = 0.01, lag = 0, A = 50)
u0 <- c(nugget = 20, rate = 10, lag = 14, A = 150)
lower <- list(gompertz = c(),
              baranyi  = c(y0 = 0.01, mumax = 0.01, K = 50, h0 = 0))
upper <- list(gompertz = u0,
              baranyi  = c(y0 = 20, mumax = 10, K = 150, h0 = 10*14))

start <- lapply(names(lower), function(v) rowMeans(cbind(lower[[v]], upper[[v]])))
names(start) <- names(upper)


if(length(dirrer(fn1))== 0){
  ests <- NULL
  # Fit these models
  t0 <- looptimer(n = nrow(lu), pref = sprintf("[%s models GR]", set), printevery = 200)
  for(i in 1:nrow(lu)){
    id <- lu$id[i]
    xy <- curves[[id]]
    mrs <- sapply(models, function(m){
      fu <- get(sprintf("grow_%s", m))
      est <- fit_growthmodel(fu, p = start[[m]], time = xy[,1], y = xy[,2], 
                       lower = lower[[m]], upper = upper[[m]])
      # use dense prediction to estimate rrate
      ydense <- predict(est, xdense_df)#curve_predict(xdense, est)
      adhoc_max_diff_rel(xdense, ydense[,2])
    })
    ests <- rbind(ests, data.frame(id = id, 
                                   estimator = paste0(models, "GR"), 
                                   rate = mrs,
                                   row.names = NULL, stringsAsFactors = FALSE))
    print ( t0 <- looptimer(t0) )
  }
  summary(t0)
  saver(list(ests=ests, lookup = lu), fn1)
}