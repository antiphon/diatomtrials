# Summarise all results

library(maxgrowthcomparison)

ww <- setwd("3-summarise/")# just to get path references right

source("0-setup.R")
plotpath <- paste0(plotpath, "paper-")

library(ggplot2)
library(patchwork)
library(tidyr)
library(dplyr)

theme_set(theme_bw())


pdftitle <- paste0(set)

### load results
if(!exists("df1")){
df0 <- loader( sprintf("%s_gathered.rds", set) )
df0 <- df0 %>% mutate(problem_set = gsub("tad", "t.a.d.", problem_set) )
#

them_methods <- c("logistic", "richards", "gompertz", "peleg", "baranyi", "baranyiGR",
                  "loess gcv", "spline gcv",
                  "max_diff", "max_diff_s", "max_diff_med", "max_diff_med3",
                  paste0("trunc_ave2_3", c("_s", "_med", "_med3")),
                  paste0("trunc_ave3_4", c("_s", "_med", "_med3")))

## Order for the estimators, keep same

them_methods <- c("loess gcv", "spline gcv", "easylinearGR", "max_diff", 
                  "max_diff_med3", "trunc_ave2_3", "trunc_ave2_3_med3", 
                  "gompertz", "peleg", "baranyi")

#df1 <- df %>% filter(estimator %in% them_methods)

# Drop the censor + upto 14

df1 <- df0 %>% filter( !grepl("upto=07, crash_after=07", problem_set) &
                        estimator %in% them_methods)
                        #!grepl("AIC|logistic|3_4", estimator))
############
addstuff <- function(d) 
  d %>%
  #mutate(reshape2::colsplit(problem_set, ", ", c("sigma", "t.a.d.", "upto", "crash"))) %>%
  tidyr::separate(problem_set, sep = ", ", c("sigma", "t.a.d.", "upto", "crash"), remove=FALSE) %>%
  # figure out nobs
  mutate(nobs =  1 + floor(as.numeric(gsub("[a-z=]*", "", t.a.d.)) * as.numeric(gsub("[a-z=]*", "", upto) ) ),
         Sigma = recode(sigma, "sigma=0.01" = "low noise", 
                        "sigma=0.15" = "medium noise" , 
                        "sigma=0.30" = "high noise"),
         Sigma = factor(Sigma) %>% relevel("medium noise") %>% relevel("low noise"))

######################################################################
# What are we plotting? Variance, bias2, MSE:
df2 <- df1 %>% 
  group_by(problem_set, estimator) %>%
  summarise(
    Bias     = mean(rrate_error, na.rm=TRUE),
    Variance = var(rrate_error , na.rm=TRUE),
    Bias2    = Bias^2,        
    MSE   = Variance + Bias2) %>%
  ungroup()


}
setwd(ww)

# how many sims per set?
print(table(table(df1$problem_set)) )



##################################################################################
# Plot: Big ones.

## Distributions of error
p <- df1 %>% 
  ggplot(aes(x = estimator, y = rrate_error, fill = estimator)) + 
  geom_hline(yintercept = 0)+
  geom_boxplot() + 
  facet_wrap(~problem_set) +
  coord_flip(ylim = c(-1,1)*5) + guides(fill = "none") 


#pdf(file=paste0(plotpath, set, "_rrate_big_errorboxplot.pdf"), title = pdftitle, 20, 20)
#print(p)
#dev.off()


##################################################################################
# metrics by problem set
df2long <- df2 %>% 
  pivot_longer(names_to = "metric", values_to = "value", Variance:MSE)  %>%
  mutate(metric = factor(metric, levels = c("Variance", "Bias2", "MSE"))) %>%
  addstuff()

df2long_rel <- df2long %>% 
  group_by(problem_set) %>%
  arrange(estimator, metric) %>%
  mutate(value_rel_to_loess = (value - value[ estimator == "loess gcv" ])/(value[ estimator == "loess gcv"])  )



p2 <- df2long %>% 
  ggplot(aes( x = value, y = problem_set )) + 
  geom_point(aes(col = estimator, shape = estimator)) +
  scale_shape_manual(values = c(1:9, letters[1:10])) +
  coord_cartesian(xlim = c(0,1)*10) +
  facet_wrap(~metric, ncol = 1)

#pdf(file=paste0(plotpath, set, "_rrate_big_metricbyproblemset.pdf"), title = pdftitle, 10, 20)
#print(p2)
#dev.off()

################################################################
# boxplot of v-b-m, not naming problem sets
p2b <- df2long %>% filter(nobs > 4) %>%
  ggplot(aes(estimator, y = value, fill = metric )) + 
  geom_boxplot() + 
  coord_cartesian(ylim = c(0.0001,1)*10) +
  scale_y_log10() + 
  facet_grid(sigma~nobs) + coord_flip() 
# p2b# rubbish

#################################################################
# Relative to loess
p2b <- df2long_rel %>% filter(nobs > 4 & estimator != "loess gcv") %>%
  ggplot(aes(estimator, y = value_rel_to_loess, fill = metric )) + 
  geom_boxplot() + 
  facet_grid(sigma~nobs) + 
  coord_flip(ylim = c(0,10)) 
p2b
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
o <- function(d) d %>% mutate(estimator = factor(estimator, 
                                                 levels = them_methods))

############################################
#1. Without crashes and no cencoring
############################################

##### Show loess raw
raw1 <- df1 %>% addstuff() %>% filter( grepl("crash_after=15", problem_set) & grepl("upto=14", problem_set) & 
                              estimator == "loess gcv")
pall1 <- raw1 %>% 
  ggplot() + 
  #geom_boxplot(aes(Sigma, y = rrate_error, fill = t.a.d.))  + xlab(NULL) +
  geom_boxplot(aes(Sigma, y = rrate_error, fill = t.a.d.))  + xlab(NULL) + 
  scale_fill_viridis_d() + coord_cartesian(ylim=c(-1, 1)) +
  #scale_x_discrete(position="top") + 
  guides(fill =  guide_legend(title="Times a day")) +
  ylab("(estimated rate) - (true rate)")

print(pall1)
  



xmax <- 5
pall <- df2long_rel %>% filter( grepl("crash_after=15", problem_set) & grepl("upto=14", problem_set) & 
                                  estimator != "loess gcv") %>%
  mutate(
    is_too_big = (value_rel_to_loess > xmax)  * 1 + 1,
    value_rel_to_loess = replace(value_rel_to_loess, value_rel_to_loess > xmax, xmax)) %>%
  o() %>%
  ggplot() + 
  geom_hline(yintercept = 0) + 
  #geom_point(aes(estimator, value_rel_to_loess, col = metric, shape = t.a.d., size = 3 + is_too_big),
  geom_point(aes(estimator, value_rel_to_loess, shape = metric, col = t.a.d., size = 3 + !is_too_big), 
             position = position_dodge(w=0.4)) +
  coord_flip(ylim = c(-1, xmax*1) ) + scale_x_discrete(limits=rev) + 
  scale_y_continuous(breaks = 1*c(-1, 0, 1, 2, 5), labels = paste0(c(-1*1, 0, 1,2, ">5"),"x") ) +
  scale_color_viridis_d() + scale_size(range = c(1,2)) + 
  ylab(NULL) + 
  xlab(NULL) +
  guides(size="none", col = "none") +
  facet_wrap(~Sigma)

pcom <- (pall1 + ggtitle("loess gcv error") | pall + ggtitle("Change relative to loess gcv (smaller better)")) + 
  plot_layout(guides = "collect", widths = c(1,3)) +
  plot_annotation(tag_levels = "A") & theme(legend.position = "bottom")

pdf(file=fo <- paste0(plotpath, set, "_two_panel_loess_v_others_only_sigma_and_t.a.d..pdf"), title = pdftitle, 11, 3)
print(pcom)
dev.off()

# tiff as well / png
tk <- 100
#tiff(filename = gsub("pdf", "tiff", fo), width = 11 * tk, height = 3 * tk, res = 100)
png(filename = gsub("pdf", "png", fo), width = 11 * tk, height = 3 * tk, res = tk)
print(pcom)
dev.off()

# Tab:
# overall rankings
ranks1 <-  df2long %>% filter( grepl("crash_after=15", problem_set) & grepl("upto=14", problem_set) )%>%
  group_by(problem_set, metric, sigma, t.a.d.) %>%
  mutate(pset_ranking = rank(value)) 
aranks1 <- ranks1 %>% 
  group_by(estimator, metric) %>%
  summarise(ave_rank = mean(pset_ranking))

atab1 <- aranks1 %>% pivot_wider(values_from = "ave_rank", names_from = "metric")   #%>% filter( metric == "MSE")
atab1 <- atab1[as.integer(factor(atab1$estimator, levels = them_methods)) %>% order(),]

print(xtable::xtable(atab1), include.rownames = FALSE)

########################################################################
#
# 2. crash
##### Show loess raw
raw1 <- df1 %>% addstuff() %>% filter( grepl("crash_after=07", problem_set) & grepl("upto=14", problem_set) & 
                                         estimator == "loess gcv")
pall1 <- raw1 %>% 
  ggplot() + 
  #geom_boxplot(aes(Sigma, y = rrate_error, fill = t.a.d.))  + xlab(NULL) +
  geom_boxplot(aes(Sigma, y = rrate_error, fill = t.a.d.))  + xlab(NULL) + 
  scale_fill_viridis_d() + coord_cartesian(ylim=c(-1, 1)) +
  #scale_x_discrete(position="top") + 
  guides(fill =  guide_legend(title="Times a day")) +
  ylab("(estimated rate) - (true rate)")

print(pall1)




xmax <- 5
pall <- df2long_rel %>% filter( grepl("crash_after=07", problem_set) & grepl("upto=14", problem_set) & 
                                  estimator != "loess gcv") %>%
  mutate(
    is_too_big = (value_rel_to_loess > xmax)  * 1 + 1,
    value_rel_to_loess = replace(value_rel_to_loess, value_rel_to_loess > xmax, xmax)) %>%
  o() %>%
  ggplot() + 
  geom_hline(yintercept = 0) + 
  #geom_point(aes(estimator, value_rel_to_loess, col = metric, shape = t.a.d., size = 3 + is_too_big),
  geom_point(aes(estimator, value_rel_to_loess, shape = metric, col = t.a.d., size = 3 + !is_too_big), 
             position = position_dodge(w=0.4)) +
  coord_flip(ylim = c(-1, xmax*1) ) + scale_x_discrete(limits=rev) + 
  scale_y_continuous(breaks = 1*c(-1, 0, 1, 2, 5), labels = paste0(c(-1*1, 0, 1,2, ">5"),"x") ) +
  scale_color_viridis_d() + scale_size(range = c(1,2)) + 
  ylab(NULL) + 
  xlab(NULL) +
  guides(size="none", col = "none") +
  facet_wrap(~Sigma)

pcom <- (pall1 + ggtitle("loess gcv error") | pall + ggtitle("Change relative to loess gcv (smaller better)")) + 
  plot_layout(guides = "collect", widths = c(1,3)) +
  plot_annotation(tag_levels = "A") & theme(legend.position = "bottom")

pdf(file=fo <- paste0(plotpath, set, "_two_panel_loess_v_others_crash_sigma_and_t.a.d..pdf"), title = pdftitle, 11, 3)
print(pcom)
dev.off()

# tiff as well
tk <- 100
#tiff(filename = gsub("pdf", "tiff", fo), width = 11 * tk, height = 3 * tk, res = tk)
png(filename = gsub("pdf", "png", fo), width = 11 * tk, height = 3 * tk, res = tk)
print(pcom)
dev.off()


# Tab:
# overall rankings
ranks1 <-  df2long %>% filter( grepl("crash_after=07", problem_set) & grepl("upto=14", problem_set) )%>%
  group_by(problem_set, metric, sigma, t.a.d.) %>%
  mutate(pset_ranking = rank(value)) 
aranks1 <- ranks1 %>% 
  group_by(estimator, metric) %>%
  summarise(ave_rank = mean(pset_ranking))

atab1 <- aranks1 %>% pivot_wider(values_from = "ave_rank", names_from = "metric")   #%>% filter( metric == "MSE")
atab1 <- atab1[as.integer(factor(atab1$estimator, levels = them_methods)) %>% order(),]

print(xtable::xtable(atab1), include.rownames = FALSE)



########################################################################
#
# 3. censoring
##### Show loess raw
raw1 <- df1 %>% addstuff() %>% filter( grepl("crash_after=15", problem_set) & grepl("upto=07", problem_set) & 
                                         estimator == "loess gcv")
pall1 <- raw1 %>% 
  ggplot() + 
  #geom_boxplot(aes(Sigma, y = rrate_error, fill = t.a.d.))  + xlab(NULL) +
  geom_boxplot(aes(Sigma, y = rrate_error, fill = t.a.d.))  + xlab(NULL) + 
  scale_fill_viridis_d() + coord_cartesian(ylim=c(-1, 1)) +
  #scale_x_discrete(position="top") + 
  guides(fill =  guide_legend(title="Times a day")) +
  ylab("(estimated rate) - (true rate)")

print(pall1)




xmax <- 5
pall <- df2long_rel %>% filter( grepl("crash_after=15", problem_set) & grepl("upto=07", problem_set) & 
                                  estimator != "loess gcv") %>%
  mutate(
    is_too_big = (value_rel_to_loess > xmax)  * 1 + 1,
    value_rel_to_loess = replace(value_rel_to_loess, value_rel_to_loess > xmax, xmax)) %>%
  o() %>%
  ggplot() + 
  geom_hline(yintercept = 0) + 
  #geom_point(aes(estimator, value_rel_to_loess, col = metric, shape = t.a.d., size = 3 + is_too_big),
  geom_point(aes(estimator, value_rel_to_loess, shape = metric, col = t.a.d., size = 3 + !is_too_big), 
             position = position_dodge(w=0.4)) +
  coord_flip(ylim = c(-1, xmax*1) ) + scale_x_discrete(limits=rev) + 
  scale_y_continuous(breaks = 1*c(-1, 0, 1, 2, 5), labels = paste0(c(-1*1, 0, 1,2, ">5"),"x") ) +
  scale_color_viridis_d() + scale_size(range = c(1,2)) + 
  ylab(NULL) + 
  xlab(NULL) +
  guides(size="none", col = "none") +
  facet_wrap(~Sigma)

pcom <- (pall1 + ggtitle("loess gcv error") | pall + ggtitle("Change relative to loess gcv (smaller better)")) + 
  plot_layout(guides = "collect", widths = c(1,3)) +
  plot_annotation(tag_levels = "A") & theme(legend.position = "bottom")

pdf(file=fo <- paste0(plotpath, set, "_two_panel_loess_v_others_censor_sigma_and_t.a.d..pdf"), title = pdftitle, 11, 3)
print(pcom)
dev.off()


# tiff as well
tk <- 100
#tiff(filename = gsub("pdf", "tiff", fo), width = 11 * tk, height = 3 * tk, res = tk)
png(filename = gsub("pdf", "png", fo), width = 11 * tk, height = 3 * tk, res = tk)
print(pcom)
dev.off()


# Tab:
# overall rankings
ranks1 <-  df2long %>% filter( grepl("crash_after=15", problem_set) & grepl("upto=07", problem_set) )%>%
  group_by(problem_set, metric, sigma, t.a.d.) %>%
  mutate(pset_ranking = rank(value)) 
aranks1 <- ranks1 %>% 
  group_by(estimator, metric) %>%
  summarise(ave_rank = mean(pset_ranking))

atab1 <- aranks1 %>% pivot_wider(values_from = "ave_rank", names_from = "metric")   #%>% filter( metric == "MSE")
atab1 <- atab1[as.integer(factor(atab1$estimator, levels = them_methods)) %>% order(),]

print(xtable::xtable(atab1), include.rownames = FALSE)



#####################################################
#
# 4. Overall rankings



plotter <- function(d, ...){
  ranks1 <-  d %>% 
    group_by(problem_set, metric, ...) %>%
    mutate(pset_ranking = rank(value)) 
  aranks1 <- ranks1 %>% 
    group_by(estimator, metric, ...) %>%
    summarise(ave_rank = mean(pset_ranking))
  # for ordering
  ranks1 <- ranks1 %>% o()
  aranks1 <- aranks1 %>% o()
  #
  #
  ranks1 %>% ggplot(aes(x = estimator)) + 
    geom_point(aes(y = pset_ranking, shape = metric, col = metric), 
               alpha = .1, position = position_dodge(w=0.3), size = 2) +
    geom_point(data = aranks1, aes(y = ave_rank, shape = metric, col = metric), 
               size = 4, position = position_dodge(w=0.3)) + 
    scale_y_continuous(breaks = c(1,10), labels=c("best", "worst")) +
    theme(legend.position = "bottom") + 
    scale_color_viridis_d(option = "D", direction = -1) +
    xlab(NULL) + ylab(NULL) # +
    
}


pall <- plotter(df2long) #+ ggtitle("Rankings of the methods over all problem sets") 


pdf(file=fo <- paste0(plotpath, set, "_rrate_ranking_overall_one_panel.pdf"), title = pdftitle, 11, 3)
print(pall)
dev.off()
# tiff as well
tk <- 100
#tiff(filename = gsub("pdf", "tiff", fo), width = 11 * tk, height = 3 * tk, res = tk)
png(filename = gsub("pdf", "png", fo), width = 11 * tk, height = 3 * tk, res = tk)
print(pcom)
dev.off()



ranks1 <-  df2long %>% #filter( grepl("crash_after=15", problem_set) & grepl("upto=07", problem_set) )%>%
  group_by(problem_set, metric) %>%
  mutate(pset_ranking = rank(value)) 
aranks1 <- ranks1 %>% 
  group_by(estimator, metric) %>%
  summarise(ave_rank = mean(pset_ranking))

atab1 <- aranks1 %>% pivot_wider(values_from = "ave_rank", names_from = "metric")   #%>% filter( metric == "MSE")
atab1 <- atab1[as.integer(factor(atab1$estimator, levels = them_methods)) %>% order(),]

#print(xtable::xtable(atab1), include.rownames = FALSE)
