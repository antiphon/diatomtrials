# Check curves

library(ggplot2)
library(dplyr)
theme_set(theme_bw())

source("0-setup.R")

fpref <- paste0(set, "_run", run)
A <- loader(paste0(fpref, ".rds"))
lu <- A$lookup
curves <- A$curves
curves0 <- A$curves_true

ss <- unique(lu$sigma)
rr <- unique(lu$nobs)

set.seed(12)

exid <- lu %>% group_by(parset) %>% sample_n(3)

df <- lapply(exid$id, function(id) {
    a<-curves[[id]]
    tibble(x=a[,1], y=a[,2], y0 = curves0[[id]][,2], id = id)
  }   ) %>% 
  bind_rows() %>% 
  left_join(exid) %>%
  group_by(parset) %>%
  mutate(gid = as.integer(factor(id)) ) %>% 
  ungroup() %>% mutate(gid = factor(gid))
  

title <- paste0(set, "_run", run)

p <- df %>% ggplot(aes(x, y, group = gid, col = gid))  +
  geom_line(aes(y = y0, group = gid), alpha = .2, size=3) +
  geom_line(aes(group = gid)) +
  geom_point(aes(group = gid), size = 1) +
  guides(col = "none") + 
  facet_wrap(~parset) + ggtitle(title)


n <- length(unique(df$parset))
w <- 1+3 * 4
h <- 3 * n/4
pdf(file=paste0(plotpath, title, "_ex_curves.pdf"), title = title, width = w, height=h+1)
print(p)
dev.off()


pl <- df %>% ggplot(aes(x, log(y), group = gid, col = gid))  +
  geom_line(aes(y = log(y0), group = gid), alpha = .2, size=3) +
  geom_line(aes(group = gid)) +
  geom_point(aes(group = gid), size = 1) +
  guides(col = "none") + 
  facet_wrap(~parset) + ggtitle(title)


pdf(file=paste0(plotpath, title, "_ex_log-curves.pdf"), title = title, width = w, height=h+1)
print(pl)
dev.off()

