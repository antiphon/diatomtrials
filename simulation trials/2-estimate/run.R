# run this bit
source("0-setup.R")
library(looptimer)

# list sims
slist <- dirrer(paste0(set, "_run[0-9]*[.]rds"))

t00 <- looptimer(n=length(slist), fg = 2, bg=222, pref="[outer]")
for(s in slist){
  fpref <- gsub("[.]rds", "", s)
  A <- loader(paste0(fpref, ".rds"))
  lu <- A$lookup
  curves <- A$curves
  source("2-estimate-adhoc.R")
  source("2-estimate-adhoc-GR.R")
  source("2-estimate-models.R")
  #source("2-estimate-models-GR.R")
  source("2-estimate-nonparametric.R")
  print(t00 <- looptimer(t00))  
}

