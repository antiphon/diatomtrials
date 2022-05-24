# Ad-hoc methods

adhoc_max_diff <- function(x, y) {
  d2 <- diff(y)/diff(x)
  mr <- max(d2)
  mr
}

adhoc_max_diff_rel <- function(x, y) {
  y[y<= 0] <- epsilon
  y <- log(y)
  d2 <- diff(y)/diff(x)
  #n <- length(x)
  #d2 <- d2/((y[-1]+y[-n])/2)
  mr <- max(d2)
  mr
}

adhoc_rate_robust <- function(x, y, m =2:3){
  d <- diff(y) / diff(x)
  d[is.infinite(d)] <- NA
  mr <- mean(sort(d, decreasing = TRUE)[m])
  mr
}

adhoc_rate_robust_rel <- function(x, y, m =2:3){
  y[y<= 0] <- epsilon
  y <- log(y)
  d <- diff(y) / diff(x)
  d[is.infinite(d)] <- NA
  #n <- length(x)
  #d <- d /((y[-1]+y[-n])/2)  # relative
  mr <- mean(sort(d, decreasing = TRUE)[m])
  mr
}


adhoc_rate_robust4 <- function(x, y){
  adhoc_rate_robust(x, y, m = 3:4)
}

adhoc_rate_robust4_rel <- function(x, y){
  adhoc_rate_robust_rel(x, y, m = 3:4)
}







adhoc_rates <- list(max_diff = adhoc_max_diff, 
                    trunc_ave2_3 = adhoc_rate_robust, 
                    trunc_ave3_4 = adhoc_rate_robust4)
adhoc_rrates <- list(max_diff = adhoc_max_diff_rel, 
                     trunc_ave2_3 = adhoc_rate_robust_rel,
                     trunc_ave3_4 = adhoc_rate_robust4_rel)
