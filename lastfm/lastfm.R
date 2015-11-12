# Load the necessary libraries
library(jsonlite)
library(dplyr)
library(ggvis)

# Number of scrobbles to load
PAGES <- 500

# Empty vector to store the retrieved scrobbles dates
dates.raw <- numeric()

# Loop length is the number of lastfm pages with recent tracks
for (index in 1:ceiling(PAGES / 200)) {
  print(paste("Processing page #", index))
  data <-
    fromJSON(
      paste(
        "http://ws.audioscrobbler.com/2.0/?method=user.getRecentTracks&limit=200&user=samosfator&page=",
        index,
        "&extended=0&api_key=5ddb360d1e5fa830834e4b9ec479b7c6&format=json",
        sep = ""
      )
    )
  # Add a date to vector as a unix time numeric
  dates.raw <- c(dates.raw, as.numeric(unlist(data$recenttracks$track$date$uts)))
}

# Extract hours from parsed dates
dates.hours <- data.matrix(summary(factor(na.omit(sapply(dates.raw, function(date) {
  format(as.POSIXct(date, origin = "1970-01-01"), "%H")
})))))

# Extract weekdays from parsed dates
scrobbles_weekdays <- summary(factor(sapply(dates.parsed, weekdays)))
dates.weekdays <- data.frame(days = names(scrobbles_weekdays), playcount = unname(scrobbles_weekdays))

# Sort weekdays by natural order
dates.weekdays$days <- factor(dates.weekdays$days,
                              levels = c("понеділок", "вівторок", "середа", "четвер", "п'ятниця", "субота", "неділя"))
dates.weekdays <- arrange(dates.weekdays, days)

# Plot the spline of hours distribution of my scrobbles
plot(
  dates.hours, type = "l",
  main = "Most common hours for listening to music",
  xlab = "Hours (0-24)",
  ylab = "Plays count",
  lty = 3,
  axes = FALSE
)

# Draw X axis
axis(side = 1, at = c(0:24))
# Draw Y axis
axis(side = 2)

# Draw a simplified line
lines(smooth.spline(dates.hours))

# Draw days of week distribution graph
dates.weekdays %>% ggvis(~days, ~playcount, fill := "#eee") %>% layer_bars()