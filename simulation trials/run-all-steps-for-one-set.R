# run all steps!

# Windows:
cores <- 1

# stimulate
ww <- setwd("1-simulate/")
for(i in 1:1) source("run.R")
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


