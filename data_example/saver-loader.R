# some helpers
# branch of fits
expdir <- "data_example"
exppath <- substr(getwd(), 1, regexpr(expdir, getwd()) + nchar(expdir))

fpath <- paste0(exppath, "/results/")

# check path
if(!dir.exists(fpath)) dir.create(fpath)

saver <- function(obj, file){
  saveRDS(obj, file = paste0(fpath, file))
}

loader <- function(fname){
  readRDS(file = paste0(fpath, fname))
}

dirrer <- function(...) dir(fpath, ...)

# file name for simulation. give name not in use
simfile <- function(set, k) {
  if(missing(k)) {
    k <- 0
    while(file.exists(paste0(fpath, fn <- paste0(set, "_sim", k<-k+1, ".rds") ))){}
  }
  else fn <- paste0(set, "_sim", k, ".rds")
  fn
}

#
loadsim <- function(set, k) {
    paste0(set, "_sim", k, ".rds")
}

