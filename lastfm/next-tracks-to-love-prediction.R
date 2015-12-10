library(dplyr)
library(ggvis)
library(data.table)
library(xgboost)

lastfm <- tbl_df(fread("lastfm/dumps/lastfm-samosfator-dump-2015-11-13.csv", sep = ",", header = TRUE))
# Remove unnecessary columns
lastfm <- lastfm %>% select(-album)
# Split date into separate columns
lastfm <- lastfm %>%
  mutate(year = as.integer(format(x = as.POSIXct(lastfm$date), "%y")),
         month = as.integer(format(x = as.POSIXct(lastfm$date), "%m")),
         day = as.integer(format(x = as.POSIXct(lastfm$date), "%d")),
         hour = as.integer(format(x = as.POSIXct(lastfm$date), "%H")),
         loved = as.integer(loved),
         plays = 0
         ) %>%
  select(-date)

artist.plays <- lastfm %>% group_by(artist) %>% summarise(n = n())

for (i in 1:nrow(lastfm)) {
  artist.current <- lastfm[i, ]$artist
  # print(artist.current)
  if (lastfm[lastfm$artist == artist.current, ]$plays == 0) {
    lastfm[lastfm$artist == artist.current, ]$plays <- artist.plays[artist.plays$artist == artist.current, ]$n
  }
  print(i)
}

dump <- lastfm

# xgboost parameters
param <- list("objective" = "multi:softprob",    # multiclass classification 
              "num_class" = 2, # number of classes 
              "eval_metric" = "auc",    # evaluation metric 
              "nthread" = 8,   # number of threads to be used 
              "max_depth" = 4,    # maximum depth of tree 
              "eta" = 1    # step size shrinkage
#               "subsample" = 0.85,    # part of data instances to grow tree 
#               "colsample_bytree" = 0.66,  # subsample ratio of columns when constructing each tree 
#               "min_child_weight" = 12  # minimum sum of instance weight needed in a child 
)

split <- sample(nrow(lastfm), floor(0.75 * nrow(lastfm)))
train.labels <- lastfm[split, ]$loved
train <- lastfm[split, ] %>% select(-loved, -artist, -title)
test <- lastfm[-split, ] %>% select(-loved)
test.tracknames <- paste(test$artist, test$title, sep = " - ")
test <- select(test, -artist, -title)

train <- model.matrix(~.-1, train)
test <- model.matrix(~.-1, test)

bst <- xgboost(param = param, data = train,
               label = train.labels,
               nrounds = 50)
prediction <- predict(bst, test)
prediction.result <- max.col(t(matrix(prediction, nrow = 2, ncol = length(prediction) / 2)), "last") - 1
summary(factor(prediction.result))

alreadyloved <- sort(unique(paste(dump[dump$loved == 1,]$artist, dump[dump$loved == 1,]$title, sep = " - ")))
recommendations <- sort(unique(test.tracknames[as.logical(prediction.result)]))
recommendations <- setdiff(recommendations, alreadyloved)

write(recommendations, file = "lastfm/next-tracks-to-love-prediction.txt")
