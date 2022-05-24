# some helpers
# branch of fits
expdir <- "simulation trials"
exppath <- substr(getwd(), 1, regexpr(expdir, getwd()) + nchar(expdir))

fpath <- paste0(exppath, "storage/")
respath <- paste0(exppath, "result_tables/")
plotpath <- paste0(exppath, "plots/")
#fpath <- paste0("storage/")

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
    while(file.exists(paste0(fpath, fn <- sprintf("%s_run%05i.rds", set, k<-k+1) ))){}
  }
  else fn <- sprintf("%s_run%05i.rds", set, k)
  fn
}

#
loadsim <- function(set, k) {
    paste0(set, "_run", k, ".rds")
}

