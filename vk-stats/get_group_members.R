getGroupMembers <- function (group_id) {
  group_id <- 71208622
  users <- numeric()

  json <- paste("https://api.vk.com/method/groups.getMembers?group_id=", group_id, sep = "") %>% fromJSON()
  users <- c(users, json$response$users)

  if (json$response$count > 1000) {
    pages_count <- ceiling(json$response$count / 1000)
    for(index in 1:pages_count) {
      print(index * 1000)
      json <-
        paste("https://api.vk.com/method/groups.getMembers?group_id=",
              group_id, "&offset=", (index * 1000),
              sep = ""
        ) %>% fromJSON()
      users <- c(users, json$response$users)
    }
  }

  users <- unlist(users)
  return(users)
}
