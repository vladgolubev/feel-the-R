library(data.table)
library(dplyr)
library(xgboost)

itogi <-
  fread("dou/itogi_goda_2015_cleaned.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE) %>%
  tbl_df() %>%
  select(-Timestamp,
         -`Лучший проект в сфере ИТ-образования?`,
         -`Лучший сайт для поиска работы?`,
         -`Самое значимое событие года?`,
         -`Лучшее профильное мероприятие года в Украине?`) %>%
  rename(position = `Кто вы?`,
         expectations =`Какие у вас ожидания от 2016?`,
         evaluation = `Как вы оцениваете этот год лично для себя?`) %>%
  filter(expectations != "")

itogi[itogi$expectations == "Все будет хорошо", ]$expectations <- "good"
itogi[itogi$expectations == "Надеюсь на лучшее/но есть сомнения", ]$expectations <- "norm"
itogi[itogi$expectations == "Все плохо/пора валить", ]$expectations <- "bad"

itogi[itogi$evaluation == "Один из лучших в моей жизни", ]$evaluation <- "good"
itogi[itogi$evaluation == "Нормальный год/все как обычно", ]$evaluation <- "norm"
itogi[itogi$evaluation == "Трудный год/но в целом все хорошо", ]$evaluation <- "bad"
itogi[itogi$evaluation == "Неудачный год/все плохо", ]$evaluation <- "verybad"

itogi[itogi$position == "Синьор (5-10 лет опыта)", ]$position <- "senior"
itogi[itogi$position == "Джуниор (меньше двух лет опыта)", ]$position <- "junior"
itogi[itogi$position == "Мега-синьор (10+ лет опыта)", ]$position <- "megasenior"
itogi[itogi$position == "Миддл (2-5 лет опыта)", ]$position <- "middle"
itogi[itogi$position == "", ]$position <- "undefined"

param <- list("objective" = "multi:softprob",    # multiclass classification 
              "num_class" = 3, # number of classes 
              "eval_metric" = "merror",    # evaluation metric 
              "nthread" = 4,   # number of threads to be used 
              "max_depth" = 4,    # maximum depth of tree 
              "eta" = 1,    # step size shrinkage
              "subsample" = 0.85,    # part of data instances to grow tree 
              "colsample_bytree" = 0.66,  # subsample ratio of columns when constructing each tree 
              "min_child_weight" = 12  # minimum sum of instance weight needed in a child 
)

split <- sample(nrow(itogi), floor(0.75 * nrow(itogi)))
train.labels <- itogi[split, ]$expectations
test.labels <- itogi[-split, ]$expectations

train.labels[train.labels == "bad"] <- 0
train.labels[train.labels == "norm"] <- 1
train.labels[train.labels == "good"] <- 2
train.labels <- as.integer(train.labels)


train <- itogi[split, ] %>% select(-expectations)
test <- itogi[-split, ] %>% select(-expectations)

train <- model.matrix(~.-1, train)
test <- model.matrix(~.-1, test)

bst <- xgboost(param = param, data = train,
               label = train.labels,
               nrounds = 50)
prediction <- predict(bst, test)
model <- xgb.dump(bst, with.stats = T)
prediction.result <- max.col(t(matrix(prediction, nrow = 3, ncol = length(prediction) / 3)), "last") - 1
summary(factor(prediction.result))

# Feature Importance
names <- dimnames(train)[[2]]
importance_matrix <- xgb.importance(names, model = bst)
xgb.plot.importance(importance_matrix[1:8,])

