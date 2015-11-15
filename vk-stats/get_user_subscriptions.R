getUserSubscriptions <- function(user_id) {
  json <-
    paste("https://api.vk.com/method/users.getSubscriptions?user_id=",
          user_id, "&count=200&v=5.40",
          sep = ""
    ) %>% fromJSON()

  return(unlist(json$response$groups$items))
}