# Summarise all results

source("0-setup.R")


library(ggplot2)
library(tidyr)
library(dplyr)

theme_set(theme_bw())


pdftitle <- paste0(set)

### load results
df0 <- loader( sprintf("%s_gathered.rds", set) )
#

them_methods <- c("logistic", "richards", "gompertz", "peleg", "baranyi", "baranyiGR",
                  "loess gcv", "spline gcv",
                  "max_diff", "max_diff_s", "max_diff_med", "max_diff_med3",
                  paste0("trunc_ave2_3", c("_s", "_med", "_med3")),
                  paste0("trunc_ave3_4", c("_s", "_med", "_med3")))

#df1 <- df %>% filter(estimator %in% them_methods)

# Drop the censor + upto 14

df1 <- df0 %>% filter( !grepl("upto=07, crash_after=07", problem_set) )
                        #!grepl("AIC|logistic|3_4", estimator))

##################################################################################
# What are we plotting? Variance, bias2, MSE:
df2 <- df1 %>% 
  group_by(problem_set, estimator) %>%
  summarise(
    Bias     = mean(rrate_error, na.rm=TRUE),
    Variance = var(rrate_error , na.rm=TRUE),
    Bias2    = Bias^2,        
       MSE   = Variance + Bias2)



##################################################################################
# Plot: Big ones.

## Distributions of error
p <- df1 %>% 
  ggplot(aes(x = estimator, y = rrate_error, fill = estimator)) + 
  geom_hline(yintercept = 0)+
  geom_boxplot() + 
  facet_wrap(~problem_set) +
  coord_flip(ylim = c(-1,1)*5) + guides(fill = "none") 


pdf(file=paste0(plotpath, set, "_rrate_big_errorboxplot.pdf"), title = pdftitle, 20, 20)
print(p)
dev.off()


##################################################################################
# metrics by problem set
df2long <- df2 %>% 
  pivot_longer(names_to = "metric", values_to = "value", Variance:MSE)  %>%
  mutate(metric = factor(metric, levels = c("Variance", "Bias2", "MSE"))) %>%
  #mutate(reshape2::colsplit(problem_set, ", ", c("sigma", "tad", "upto", "crash"))) %>%
  tidyr::separate(problem_set, sep = ", ", c("sigma", "tad", "upto", "crash"), remove=FALSE) %>%
  # figure out nobs
  mutate(nobs =  1 + floor(as.numeric(gsub("[a-z=]*", "", tad)) * as.numeric(gsub("[a-z=]*", "", upto) ) ))


p2 <- df2long %>% 
  ggplot(aes( x = value, y = problem_set )) + 
  geom_point(aes(col = estimator, shape = estimator)) +
  scale_shape_manual(values = c(1:9, letters[1:10])) +
  coord_cartesian(xlim = c(0,1)*10) +
  facet_wrap(~metric, ncol = 1)

pdf(file=paste0(plotpath, set, "_rrate_big_metricbyproblemset.pdf"), title = pdftitle, 10, 20)
print(p2)
dev.off()


###############################################################
# Rankings.
ranks <- df2long %>%
  group_by(problem_set, metric) %>%
  mutate(pset_ranking = rank(value)) 

ranks_sigma <- df2long %>%
  group_by(problem_set, metric, sigma) %>%
  mutate(pset_ranking = rank(value)) 

ranks_nobs <- df2long %>% 
  group_by(problem_set, metric, nobs) %>%
  mutate(pset_ranking = rank(value)) 

ranks_nobs_sigma <- df2long %>% 
  group_by(problem_set, metric, sigma, nobs) %>%
  mutate(pset_ranking = rank(value)) 

##################################
# Helpers
plotter <- function(d, ...){
  ranks1 <-  d %>% 
    group_by(problem_set, metric, ...) %>%
    mutate(pset_ranking = rank(value)) 
  aranks1 <- ranks1 %>% 
    group_by(estimator, metric, ...) %>%
    summarise(ave_rank = mean(pset_ranking))
  # for ordering
  aranks0 <- ranks1 %>% 
    group_by(estimator, metric) %>%
    summarise(ave_rank = mean(pset_ranking))
  z <- aranks0 %>% filter(metric == "MSE")
  o <- function(d) d %>% mutate(estimator = factor(estimator, levels = z$estimator[order(z$ave_rank)]))
  ranks1 <- ranks1 %>% o()
  aranks1 <- aranks1 %>% o()
  #
  #
  ranks1 %>% ggplot(aes(x = estimator)) + 
    geom_point(aes(y = pset_ranking, shape = metric), alpha = .1, position = position_dodge(w=0.3), size = 2) +
    geom_point(data = aranks1, aes(y = ave_rank, col = estimator, shape = metric), 
               size = 4, position = position_dodge(w=0.3)) + 
    guides(col = "none") + 
    theme(legend.position = "bottom") + 
    xlab(NULL) + ylab(NULL)  +
    theme(axis.text.x=element_text(angle=30, vjust=.5))
}





# overall rankings
pall <- plotter(df2long) + ggtitle("Rankings of the methods over all problem sets") 
  

pdf(file=paste0(plotpath, set, "_rrate_ranking_overall_one_panel.pdf"), title = pdftitle, 12, 5)
print(pall)
dev.off()


# overall rankings along sigma
pall <- plotter(df2long, sigma) + 
  facet_wrap(~sigma, ncol = 1)
pdf(file=paste0(plotpath, set, "_rrate_ranking_overall_bysigma.pdf"), title = pdftitle, 12, 7)
print(pall +   ggtitle("Rankings of the methods over all problem sets") )
dev.off()

#### Without crashes
pall <- plotter(df2long %>% filter(grepl("crash_after=15", problem_set)), 
                sigma) + 
  facet_wrap(~sigma, ncol = 1) +
  ggtitle("Rankings of the methods over all problem sets without crashes") 

pdf(file=paste0(plotpath, set, "_rrate_ranking_nocrashes_bysigma.pdf"), title = pdftitle, 12, 7)
print(pall)
dev.off()

#### Without crashes and no cencoring
pall <- plotter(df2long %>% filter( grepl("crash_after=15", problem_set) & grepl("upto=14", problem_set)),
                sigma) + 
  facet_wrap(~sigma, ncol = 1) +
  ggtitle("Rankings of the methods over all problem sets without crashes and no censoring") 


pdf(file=paste0(plotpath, set, "_rrate_ranking_NoCrashesNoCensoring_bysigma.pdf"), title = pdftitle, 12, 7)
print(pall)
dev.off()

#### Without crashes and no cencoring
pall <- plotter(df2long%>% filter(
  grepl("crash_after=15", problem_set) & 
    grepl("upto=14", problem_set) & 
    grepl("tad=1.0", problem_set) ), sigma) +
  facet_wrap(~sigma, ncol = 1) +
  ggtitle("Rankings of the methods over all problem sets with only noise (tad=1)") 


pdf(file=paste0(plotpath, set, "_rrate_ranking_justNoise_bysigma.pdf"), title = pdftitle, 12, 7)
print(pall)
dev.off()



## Just crashes
pall <- plotter( df2long %>% filter(!grepl("crash_after=15", problem_set)), sigma) +
  facet_wrap(~sigma, ncol = 1) +
  ggtitle("Rankings of the methods over all problem sets with a crash") 


pdf(file=paste0(plotpath, set, "_rrate_ranking_alwaysCrash_bysigma.pdf"), title = pdftitle, 12, 7)
print(pall)
dev.off()

##############################################################
# By sample-size
# overall rankings
pall <- plotter(df2long, nobs, sigma) + 
  facet_grid(nobs ~ sigma) +
  ggtitle("Rankings of the methods over all problem sets")


pdf(file=paste0(plotpath, set, "_rrate_ranking_overall_bysigma_by_obs.pdf"), title = pdftitle, 14, 7)
print(pall)
dev.off()

