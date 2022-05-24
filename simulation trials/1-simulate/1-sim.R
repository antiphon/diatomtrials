# Simulate growth curves! Mega looop. Will simulate 
# draws from the parameter hypercube.

source("0-setup.R")
source("relative_rate_calculator.R")

################################################
#
# Lets go
curves <- list()
lu     <- NULL
# unique identifier, start with something useful

npars <- length(sigmas) * length(freq_by_vector) * length(upto_vector)*length(crash_after_vector)

run <- length(dirrer(paste0(set, "_run[0-9]*.rds"))) + 1
k <- (run-1) * npars * batch_size + 1

set.seed(k)

# one batch
t0 <- looptimer(n = npars, fg = 256, bg=33, pref = sprintf("[%s rep %i]", set, run))

for(sigma in sigmas) {
  for(sample_by in freq_by_vector) {
    for(upto in upto_vector) {
      x <- seq(0, upto, by = sample_by)
      for(crash_after in crash_after_vector) {
        # trouble chosen.
        trouble <- function(xy) {
          if(crash_after < upto) xy <- add_crash(xy, at = crash_after, rate = .25)
          add_noise1(xy, sigma = sigma)
        }
        # then sample
        pars <- rbind( draw_parameters(batch_size) )
        rrate <- apply(pars, 1, function(pa) 
          # baranyi has inbuild nugget 
          relative_rate(pa[1], pa[-1], model_fun, nugget = 0))
        #
        obs <- lapply(1:batch_size, function(i) 
          trouble(cbind(x=x, model_fun(x, pars[i,]))  ))
        # then compile
        id <- makeid(1:batch_size + k - 1)
        k <- k + batch_size
        lu1 <- data.frame(id = id, 
                          pars, 
                          rrate = rrate, 
                          sigma = sigma,
                          sample_by = sample_by,
                          upto = upto,
                          crash_after = crash_after,
                          nobs = length(x),
                          model = model_name)
        curves[id] <- obs
        lu <- rbind(lu, lu1)
        print(t0 <- looptimer(t0))
      }
    }
  }
}
summary(t0)
# ok done
fn <- simfile(set, run)
saver(list(curves=curves, lookup=lu) , fn)
print(fn)

