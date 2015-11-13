library(dplyr)
library(data.table)

LAST.FM.samosfator <- tbl_df(fread("lastfm-samosfator-dump-2015-11-13.csv", sep = ",", header = TRUE))
LAST.FM.oasispo <- tbl_df(fread("lastfm-oasispo-dump-2015-11-13.csv", sep = ",", header = TRUE))


samosfator.hours_distribution <- LAST.FM.samosfator %>%
  group_by(hour = format(as.POSIXct(LAST.FM.samosfator$date), "%H")) %>%
  summarise(plays = n()) %>%
  arrange(desc(plays))

oasispo.hours_distribution <- LAST.FM.oasispo %>%
  group_by(hour = format(as.POSIXct(LAST.FM.oasispo$date), "%H")) %>%
  summarise(plays = n()) %>%
  arrange(desc(plays))

plot(smooth.spline(samosfator.hours_distribution),
     type = "l",
     xlab = "Години доби (1-24)",
     ylab = "Скроблів",
     main = "Кількість скроблів по годинах",
     col = "blue")

lines(smooth.spline(oasispo.hours_distribution), col = "red")

legend('topleft', c("samosfator", "oasispo"), 
       lty=1, col=c("blue", "red"), bty='n', cex=.75)
