# Gather all results

source("0-setup.R")


### all fits
slist <- dirrer(paste0(set, "_run[0-9]*_fits.*"))

t00 <- looptimer(n=length(slist), fg = 2, bg=222, pref="[gather]")

df <- tibble()

for(s in slist){
  A <- loader(paste0(s))
  lu <- A$lookup
  rr <- strsplit(s, "_")[[1]][[2]]
  est <- A$ests %>% mutate(rrate_est = rate, rate = NULL, run = rr)
  df1 <- left_join(lu, est, by = "id")
  df <- bind_rows(df, df1)
  print(t00<-looptimer(t00))
}
# some niceties
df_out <- df %>% 
  mutate(
    set = set,
    rrate_true = rrate,
    rrate_error = rrate_est - rrate_true,
    rrate_error_relative = rrate_error/rrate_true,
    Sigma  = factor(paste("sigma", df$sigma)),
    problem_set = sprintf("sigma=%3.2f, tad=%3.1f, upto=%02i, crash_after=%02i", sigma, 1/sample_by, upto, crash_after))

## ok lets store

fout <- sprintf("%s_gathered.rds", set)
saver(df_out, fout)





