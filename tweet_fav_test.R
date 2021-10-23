
library(rtweet)
library(dplyr)
library(lubridate)
library(reactable)


show_tweet <- function(user){
  get_timeline(user, n=3200) %>% 
    mutate(year = year(created_at)) %>% 
    filter(year == 2020) %>% 
    arrange(-favorite_count) %>% 
    slice(1) %>% 
    pull(status_id) %>% 
    paste0('https://twitter.com/', user,  '/status/', .) %>% browseURL()
}


favs_raw <- rtweet::get_favorites("brndngrhm", n = 3000)

hashtags <- favs_raw %>%
  select(status_id, hashtags) %>%
  as.data.frame()

hashtags$hashtags <- sapply(hashtags$hashtags, paste, collapse=",")

favs <- favs_raw %>%
  select(-hashtags) %>%
  left_join(., hashtags, by = "status_id") %>%
  arrange(desc(created_at)) %>%
  mutate(created_at = as.Date(created_at),
         hashtags = gsub(',','; ', hashtags),
         hashtags = ifelse(hashtags == "NA", " ", hashtags),
         tweet_link =  paste0("<a href='https://twitter.com/", screen_name,  "/status/", status_id, "' target='_blank'> Go To Tweet </a>"),
         content_link = ifelse(!is.na(urls_url), paste0("<a href='", urls_url,"' target='_blank'>", urls_url,"</a>"), urls_url)
         ) %>%
  as.data.frame() %>%
  select(screen_name, created_at, text, tweet_link, content_link, hashtags) %>%
  as.data.frame() %>%
  rename(User = screen_name,
         Posted = created_at,
         Tweet = text,
         `Tweet Link` = tweet_link,
         Hashtags = hashtags,
        `External Content` = content_link)

DT::datatable(favs, escape = F, rownames = F) 
