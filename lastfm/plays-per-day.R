library(dplyr)
library(ggvis)
library(data.table)

lastfm <- tbl_df(fread("lastfm/dumps/lastfm-samosfator-dump-2015-11-13.csv", sep = ",", header = TRUE))

lastfm %>% 
  group_by(as.Date(date)) %>%
  summarise(n = n()) %>%
  plot(`as.Date(date)` ~ n, type = "h")
#   ggvis(~`as.Date(date)`, ~n, fill = ~n) %>%
#   layer_lines()