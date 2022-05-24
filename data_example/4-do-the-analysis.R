# COmpute the tests etc on the derived rates

source("setup.R")


library(ggplot2)
library(patchwork)
library(tidyr)
library(dplyr)

theme_set(theme_bw())

source("1-load-data.R")

lu <- df0 <- A$lookup
gfi <- dirrer("gathered")
df <- loader(gfi)

pfpref <- paste0("plots/paper-", fpref)


#
them_methods <- c("spline gcv", 
                  #"max_diff_med3", 
                  "trunc_ave2_3_med3", 
                  #"peleg",
                  "gompertz"#, "logistic"
                  )

df <- df %>% filter(estimator %in% them_methods)

# Only two temperatures

df <- df %>% filter(assay %in% c(6, 18)) %>% 
  mutate(sample_size = sprintf("n=%2i", nobs),
         rate = rrate_est,
         estimator = factor(estimator, levels = them_methods)) 
#################
# T-tests
ests <- tibble()
for(e in unique(df$estimator)) {
  d <- df %>% filter(estimator == e) %>% 
    mutate(group = letters[as.integer(factor(assayC))]) %>%
    select(group, rate)
  es <- lm(rate~group, d)
  es0 <- lm(rate~-1+group, d)
  ss <- summary(es)
  ci <- confint(es) %>% as_tibble(rownames="coef")
  ci0 <- confint(es0)%>% as_tibble(rownames="coef")
  ss0 <- summary(es0)
  
  t0 <- ss0$coefficients %>% as_tibble(rownames="coef") %>% left_join(ci0)
  t1 <- ss$coefficients %>% as_tibble(rownames="coef") %>%left_join(ci) %>%
    filter(coef == "groupb") %>% mutate(coef = gsub("groupb", "difference", coef))
  
  ests <- ests %>% bind_rows( rbind(t0, t1) %>% mutate(estimator = e) )
}
ests <- ests %>% mutate(estimator = factor(estimator, levels = them_methods) ) %>%
  select(estimator, everything())

## annotations
df <- df %>% mutate(Temperature = recode(assayC, "temp06" =" 6 C", temp18="18 C"))


######
# Then we plot the results
f1 <- df %>% ggplot() +
  geom_boxplot(aes(estimator, rrate_est, fill = Temperature)) + 
  xlab(NULL) + ylab(NULL) +
  scale_fill_viridis_d() +
  coord_flip(ylim = c(-.1, 2 ))  +
  ggtitle("Estimated relative rates")
  
# 

f2 <- ests %>% filter(coef == "difference") %>% ggplot() + 
  geom_hline(yintercept = 0, col = "gray40") +
  geom_pointrange(aes(estimator, y=Estimate, ymin=`2.5 %`, ymax=`97.5 %`)) + 
  scale_color_viridis_d() +
  xlab(NULL) + ylab(NULL) +
  coord_flip(ylim = c(-.5,1)*1) + ggtitle("Difference and 95%CIs")


###############
# Also plot data
lu1 <- df %>% group_by(id) %>% filter(estimator == them_methods[1])
a <- lapply(lu1$id, function(i) tibble(id = i, x = A$curves[[i]][,1], y = A$curves[[i]][,2])  ) %>% bind_rows()
b <- lu1 %>% right_join(a)

pd <- b %>% ggplot() + 
  geom_line(aes(x, log(y), group = id, col = Temperature)) +
  scale_color_viridis_d() +
  xlab("days") + ylab("log-fluoro") + 
  facet_wrap(~Temperature) +
  guides(col = "none") + ggtitle("Observed growth serieses")
 

pan <- (pd | f1 + theme(legend.position = "bottom") | f2) + plot_layout(widths = c(2,1,1)) 
z <- pan  & theme(title = element_text(size=9))

# tree
pdf(file = paste0(pfpref, "triple-panel.pdf"), width=11, height=3)
print(z)
dev.off()

# just too
pan <- (pd | f1) + plot_layout(widths = c(2,1)) 
z <- pan  & theme(title = element_text(size=11))
pdf(file = paste0(pfpref, "double-panel.pdf"), width=11, height=3)
print(z)
dev.off()

# P-value table as well
xt <- xtable::xtable(ests %>% filter(coef == "difference") %>% select(-coef))
print(xt, include.rownames = FALSE)
