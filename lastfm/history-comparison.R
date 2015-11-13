library(dplyr)
library(ggvis)
library(data.table)

LAST.FM.samosfator <- tbl_df(fread("lastfm-samosfator-dump-2015-11-13.csv", sep = ",", header = TRUE))
LAST.FM.oasispo <- tbl_df(fread("lastfm-oasispo-dump-2015-11-13.csv", sep = ",", header = TRUE))

days_of_week <- c("понеділок", "вівторок", "середа", "четвер", "п'ятниця", "субота", "неділя")

samosfator.hours_distribution <- LAST.FM.samosfator %>%
  group_by(day = weekdays(as.POSIXct(LAST.FM.samosfator$date))) %>%
  summarise(plays = n()) %>%
  arrange(factor(samosfator.hours_distribution$day, levels = days_of_week)) %>%
  mutate(ratio = plays / nrow(LAST.FM.samosfator)) %>%
  mutate(day = factor(day, levels = days_of_week))

oasispo.hours_distribution <- LAST.FM.oasispo %>%
  group_by(day = weekdays(as.POSIXct(LAST.FM.oasispo$date))) %>%
  summarise(plays = n()) %>%
  arrange(factor(oasispo.hours_distribution$day, levels = days_of_week)) %>%
  mutate(ratio = plays / nrow(LAST.FM.oasispo)) %>%
  mutate(day = factor(day, levels = days_of_week))

samosfator.hours_distribution %>%
  ggvis(~day, ~ratio, fill := "blue") %>%
  layer_points() %>%
  layer_points(data = oasispo.hours_distribution,
               ~day, ~ratio, fill := "red")


