# Set run 3 setup:
####
model_fun <- model_baranyi
model_name <- "baranyi"
# Example
params <- c(rate=1, lag=3, A=100, y0 = 0.1)
# Perfect curve from uniformly the following hyperinterval:
rates_limits <- c(.5, 3) # r1: fixed, r2:release 
lag_limits   <- c(0, 6) 
nugget_limits <- c(0.01, 5)
A_limits <- c(50, 150) # r3: release
