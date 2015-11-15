library(jsonlite)
library(dplyr)

public_id <- 71208622
source("vk-stats/get_group_members.R", local = TRUE)
users <- getGroupMembers(public_id)

source("vk-stats/get_user_subscriptions.R", local = TRUE)
subscriptions.ids <- matrix(ncol = 2)

progress_index <- 1
for(user_id in users) {
  print(paste("Processing id #", (progress_index = progress_index + 1), "of", length(users)))
  subscriptions.ids <- rbind(subscriptions.ids, cbind(user_id, subscription_id = getUserSubscriptions(user_id)))
}

data <- tbl_df(data.frame(user_id = subscriptions.ids[,1], public_id = subscriptions.ids[,2])) %>% na.omit()

top20_publics.ids <- names(summary(factor(data$public_id))[1:21])
top20_publics.names <- fromJSON(paste("https://api.vk.com/method/groups.getById?group_ids=",
                             paste(top20_publics.ids, collapse = ","),
                             sep = ""
))$response$name

top20_publics <- data.frame(name = top20_publics.names, id = top20_publics.ids)

top20_publics.rating <- data %>% filter(public_id %in% top20_publics.ids) %>% select(public_id) %>% unlist() %>% unname() %>% factor() %>% summary() %>% data.frame()
top20_publics.rating <- top20_publics.rating %>% mutate(public_id = rownames(top20_publics.rating))
top20_publics.rating <- rename(top20_publics.rating, members_count = .)
top20_publics.rating <- top20_publics.rating %>% arrange(desc(members_count))
top20_publics.rating <- top20_publics.rating[2:21,]
top20_publics.names <- top20_publics.names[2:21]
top20_publics.rating <- mutate(top20_publics.rating, name = top20_publics.names)
top20_publics.rating <- top20_publics.rating %>% mutate(ratio = round(top20_publics.rating$members_count / length(users) * 100, 2))

result <- character()
for(index in 1:nrow(top20_publics.rating)) {
  result <- paste(result, paste(index, ". ", top20_publics.rating[index,]$ratio, "% - @club", top20_publics.rating[index,]$public_id, "(", top20_publics.rating[index,]$name, ")", sep = ""), sep = "\n")
}

write(result, "vk-stats/result.txt")
pie(top20_publics.rating$members_count, labels = top20_publics.rating$name, cex = 0.6)