
relative_rate <- function(rates, 
                          params_other, 
                          model_fun, 
                          nugget = 0,
                          tmax = TMAX){ # just eval 
  x <- seq(0, tmax, l = n <- 2000)
  dx <- diff(x[1:2])
  sapply(rates, function(rate){
    f <- model_fun(x, c(rate,params_other)) + nugget
    lf <- log(f)
    max( (diff(lf)/dx)[f>0], na.rm=TRUE)
  })
}
