# Various methods from growthrates-package

library(growthrates)

adhoc_rrate_easylinearGR <- function(x, y, h = 3) {
  te <- try( f <- fit_easylinear(x, y, h = h) , silent = TRUE)
  if(inherits(te, "try-error")) NA else coef(f)[3]
}

adhoc_rrates <- list(easylinearGR = adhoc_rrate_easylinearGR)
