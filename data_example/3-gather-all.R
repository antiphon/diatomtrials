# Gather all results

source("setup.R")


### all fits
set <- fpref
slist <- dirrer(paste0(set, "_fits.*"))

t00 <- looptimer(n=length(slist), fg = 2, bg=222, pref="[gather]")

df <- tibble()

for(s in slist){
  A <- loader(paste0(s))
  lu <- A$lookup
  rr <- 1#strsplit(s, "_")[[1]][[2]]
  est <- A$ests %>% mutate(rrate_est = rate, rate = NULL, run = rr)
  df1 <- left_join(lu, est, by = "id")
  df <- bind_rows(df, df1)
  print(t00<-looptimer(t00))
}
# some niceties
df_out <- df %>% 
  mutate(
    set = set
    )

## ok lets store

fout <- sprintf("%s_gathered.rds", set)
saver(df_out, fout)





