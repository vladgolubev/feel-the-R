library(dplyr)
library(ggvis)
library(data.table)

LAST.FM.samosfator <- tbl_df(fread("lastfm/dumps/lastfm-samosfator-dump-2015-11-13.csv", sep = ",", header = TRUE))
LAST.FM.oasispo <- tbl_df(fread("lastfm/dumps/lastfm-oasispo-dump-2015-11-13.csv", sep = ",", header = TRUE))

days_of_week <- c("понеділок", "вівторок", "середа", "четвер", "п'ятниця", "субота", "неділя")

samosfator.dayofweek_distribution <- LAST.FM.samosfator %>%
  group_by(day = weekdays(as.POSIXct(date))) %>%
  summarise(plays = n()) %>%
  mutate(ratio = plays / sum(plays),
         day = factor(day, levels = days_of_week)) %>%
  select(-plays)

oasispo.dayofweek_distribution <- LAST.FM.oasispo %>%
  group_by(day = weekdays(as.POSIXct(date))) %>%
  summarise(plays = n()) %>%
  mutate(ratio = plays / sum(plays),
         day = factor(day, levels = days_of_week)) %>%
  select(-plays)

comparison <- rbind(samosfator.dayofweek_distribution %>% mutate(username = "samosfator"),
                    oasispo.dayofweek_distribution %>% mutate(username = "oasispo"))

comparison %>%
  group_by(username) %>%
  ggvis(~day, ~ratio, stroke = ~username, strokeWidth := 4) %>%
  layer_lines()
