# Gather all experiments MSE tables, rank each method, and compute average ranks

library(tidyr)
library(dplyr)
library(ggplot2)


# just to be in a subdir
wdd <- setwd("plots")
source("../setup.R")

fl <- dir(respath, "sumdf")
df <- NULL
for(f in fl){
    dr <- readRDS(paste0(respath, f))
    # compute MSE etc
    scores <- dr %>% 
      group_by(parset_no_sigma, Sigma, estimator, trial) %>% 
      summarise(bias  = mean(rate_error),
                var   =  var(rate_error)
                ) %>%
      mutate(bias2 = bias^2,
             MSE = var + bias^2)
    df <- df %>% bind_rows(scores)    
}
setwd("..")

# trial descriptions
titles <- sapply(readLines("trials.txt"), function(l) if(grepl("#", l)) gsub("# ", "", l) else "")
titles <- titles[nchar(titles)>0] %>% unname()

df <- df %>% mutate(trial = factor(trial, labels = titles, levels = paste0("trial", 1:length(titles))))

## original picture order
method_levs <- levels(dr$estimator)

###
# Within each experiment, determine the rank of each estimator in each metric category.
###
dfl <- df %>% 
  pivot_longer(names_to = "stat", values_to = "value", var:MSE) %>% 
  mutate(metric = factor(stat, levels = c("var", "bias2", "MSE"), 
                         labels = c("Variance", "Bias^2", "MSE")),
         method = as.character(estimator) )
##
# Here is the deciding grouping.
dfl <- dfl %>% 
  #filter(Sigma != "sigma 0.05") %>% 
  group_by(trial, parset_no_sigma, Sigma, metric) %>%
  mutate( rank = rank(value))


# Average ranks:
dfo <- dfl 

ave_by_trial <- dfo %>% group_by(metric, method, trial) %>% summarise(rank = mean(rank))
ave_overall <- dfo %>% group_by(metric, method) %>% summarise(rank = mean(rank))

# Order by overall rank MSE
sub <- ave_overall %>% filter(metric == "MSE")# & trial == trial[3])
#method_levs <- sub$method[order(sub$rank)]
##
dfo <- dfo %>% 
  mutate(method = factor(method, levels = method_levs))

# plot
pic <- dfo %>% 
  ggplot(aes(x = method, y = rank)) + 
  geom_point(alpha = .05) +
  geom_point(data=ave_by_trial, aes(col = method), size = 5) +
  facet_grid(trial~metric) + 
  theme_bw() + 
  theme(axis.text.x=element_text(angle=30, hjust=1), legend.position = "none") +
  xlab("") + ylab("") +
  scale_y_continuous(breaks = c(1, 10), labels = c("best", "worst"))



pdf(file = "plots/experiment_ranking_bytrial.pdf", width = 10, height = 8)
print(pic)
dev.off()


### Alternative: Put all metrics in same panel
pic <- dfo %>% 
  #mutate(method = factor(method, levels = sub$method[order(sub$rank)]  )) %>%
  ggplot(aes(x = method, y = rank)) + 
  geom_point(alpha = .05, aes(shape = metric),
             position=position_dodge(w=.3)) +
  geom_point(data=ave_by_trial, aes(col = method, shape = metric), 
             size = 4, 
             position=position_dodge(w=.3)) +
  facet_wrap(~trial) +
  theme_bw() + 
  theme(axis.text.x=element_text(angle=30, hjust=1), 
        legend.position = "bottom") +
  guides(col = "none", shape=guide_legend(title=NULL)) +
  xlab("") + ylab("") +
  scale_y_continuous(breaks = c(1, 10), labels = c("best", "worst")) 

pdf(file = "plots/experiment_ranking_bytrial_one_panel.pdf", width = 8, height = 6)
print(pic + ggtitle("Ranking of the methods"))
dev.off()

#





# Overall ranking

# Order by rank MSE
#sub <- ave_overall %>% filter(metric == "MSE")
dfo <- dfo %>% 
  mutate(method = factor(method, levels = method_levs))

# plot
pic <- dfo %>%
  ggplot(aes(x = method, y = rank)) + 
  geom_point(alpha = .05) +
  geom_point(data=ave_overall, aes(col = method), size = 5) +
  facet_grid(~metric) + 
  theme_bw() + 
  theme(axis.text.x=element_text(angle=30, hjust=1), legend.position = "none") +
  xlab("") + ylab("") +
  scale_y_continuous(breaks = c(1, 10), labels = c("best", "worst"))



pdf(file = "plots/experiment_ranking_overall.pdf", width = 10, height = 3)
print(pic)
dev.off()


### Alternative: Put all metrics in same panel
ave_overall_metric <- dfo %>% group_by(method, metric) %>% summarise(rank = mean(rank))
pic <- dfo %>% mutate(method = factor(method, levels = sub$method[order(sub$rank)]  )) %>%
  ggplot(aes(x = method, y = rank)) + 
  geom_point(alpha = .05, aes(shape = metric),
             position=position_dodge(w=.3)) +
  geom_point(data=ave_overall, aes(col = method, shape = metric), 
             size = 4, 
             position=position_dodge(w=.3)) +
  theme_bw() + 
  theme(axis.text.x=element_text(angle=30, hjust=1), 
        legend.position = "bottom") +
  guides(col = "none", shape=guide_legend(title=NULL)) +
  xlab("") + ylab("") +
  scale_y_continuous(breaks = c(1, 10), labels = c("best", "worst")) 

pdf(file = "plots/experiment_ranking_overall_one_panel.pdf", width = 8, height = 5)
print(pic + ggtitle("Ranking of the methods over the trials"))
dev.off()

#

setwd(wdd)

