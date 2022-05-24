# run all steps!

# Windows:
cores <- 1 # if on unix, set to your liking

# stimulate
ww <- setwd("1-simulate/")
for(i in 1:5) source("run.R")
setwd(ww)

# estibate
ww <- setwd("2-estimate/")
source("run.R")
setwd(ww)

#########
# slap together
ww <- setwd("3-summarise/")
source("run.R")
setwd(ww)


