# Load the dataset
source("setup.R")
library(dplyr)


E <- readRDS( dir("data/", pattern=fpref, full.names = TRUE) ) #%>% last()
# make a unique id for each series
E <- E %>% mutate(id = sprintf("id_%s_%s", sasplate, saswell))

ids <- unique(E$id)

curves <- lapply(ids, function(id) E %>% filter(id == !!id) %>% 
                   select(Timefromfirst_d, Value_fluoro) %>% as.matrix())

names(curves) <- ids

lu <- E %>% 
  group_by(id) %>% 
  mutate(Tmin = min(Timefromfirst_d),
         Tmax = max(Timefromfirst_d),
         nobs = n(),
         y_max = max(Value_fluoro)) %>%
  filter(row_number() == 1) %>% 
  select(-Timefromfirst_d, -Value_fluoro)

A <- list(curves = curves,
          lookup = lu)


# ggplot(E, aes(Timefromfirst_d, Value_fluoro, group=id)) + geom_line()