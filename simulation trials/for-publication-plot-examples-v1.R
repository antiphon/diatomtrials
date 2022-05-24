# Plot example curves of the simulations,
# For pulbications.
# v1. "Middle 6"

library(dplyr)
library(ggplot2)
# for inconvenience
ww <- setwd("plots/")
source("../setup.R")
# load a set. all runs should be the same
setwd(ww)
set <- "run01"
A <- loader(  dirrer(paste0(set, ".*1.rds"))  )
lu <- A$lookup
curves <- A$curves
# for each troubleset
neach <- 3

set.seed(1010)

luex <- lu %>% group_by(sigma, sample_by, upto, crash_after) %>%
  # keep only what:
  filter(!( crash_after == 7 & upto == 7) & 
           !sigma == 0.15 & sample_by == 1.0)  %>%
  sample_n(neach) %>% 
  filter()

df <- lapply(luex$id, function(i) tibble(x = curves[[i]][,1], y = curves[[i]][,2], id = i) ) %>% 
  bind_rows() %>%
  left_join(luex , by = "id") %>%
  mutate(parset = 
           sprintf("t.a.d.=%s %s %s", 1/sample_by, 
                   c("censoring after day 7", "")[1+1*(upto>7)], 
                   c("crash after day 7", "")[1 + 1*(crash_after>7)] ))%>%
         #parset = recode(parset, "t.a.d.=1"="t.a.d.=1 no issues")) %>%
  group_by(parset, sigma) %>% 
  mutate(subid = factor(id, levels = unique(id)) %>% as.integer() %>% as.character(),
         Sigma = sprintf("noise sd=%3.2f", sigma)) 
  # labels?
df_lab <- df %>% 
  group_by(id) %>% 
  mutate(gpar = sprintf("lag=%3.1f y(0)=%3.1f", lag, y0),
         glaby = (as.integer(subid)-1) * .75 +.05 ) %>%
  sample_n(1)


fp <- df %>% ggplot(aes(x, y, col = subid)) +
  geom_point(size = 1) +
  geom_line(alpha = .3) +
  guides(col = "none") +
  #scale_y_log10(limits = (c(0.01, NA))) +
  scale_y_log10() +
  scale_x_continuous(breaks = 0:7*2) +
  facet_grid(Sigma ~ parset) + labs(y=NULL, x=NULL) +
  geom_text(data = df_lab, aes(10, exp(-glaby), 
                               label = gpar, col = subid), size=3) +
  theme_bw() #+ ggtitle(set) 
fp

pdf(file = paste0("plots/paper-", set, "-curve-examples.pdf"), width = 9, height=4)
print(fp)
dev.off()
