library(jsonlite)
library(dplyr)

LASTFM.USERNAME <- "samosfator"
LASTFM.PLAYCOUNT <- 70000
LASTFM.PAGES_COUNT <- ceiling(LASTFM.PLAYCOUNT / 200)

artists <- character()
titles <- character()
albums <- character()
dates <- .POSIXct(character(10))
loved <- logical()

for (index in 1:LASTFM.PAGES_COUNT) {
  print(paste("Processing page #", index, "of", LASTFM.PAGES_COUNT))
  
  json = fromJSON(
    paste(
      "http://ws.audioscrobbler.com/2.0/?method=user.getRecentTracks&limit=200&user=", LASTFM.USERNAME,
      "&page=", index,
      "&extended=1&api_key=5ddb360d1e5fa830834e4b9ec479b7c6&format=json",
      sep = ""
    )
  )
  
  artists <- c(artists, json$recenttracks$track$artist$name)
  titles <- c(titles, json$recenttracks$track$name)
  albums <- c(albums, json$recenttracks$track$album$`#text`)
  dates <- c(dates, as.POSIXct(as.numeric(json$recenttracks$track$date$uts), origin = "1970-01-01"))
  loved <- c(loved, as.logical(as.numeric(json$recenttracks$track$loved)))
}

LASTFM.DATA <- data.frame(
  artist = artists,
  title = titles,
  album = albums,
  date = format(dates[1:(length(dates) - 10)], "%Y-%m-%d %H:%M:%S %Z"),
  loved = loved,
  stringsAsFactors = FALSE
) %>% tbl_df() %>% filter(!is.na(date))

write.csv(LASTFM.DATA, row.names = FALSE, "lastfm-dump-20151113.csv")
