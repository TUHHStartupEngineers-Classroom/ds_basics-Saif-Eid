#######CHALLENGE #1

library(httr)
library(rtweet)
library(dplyr)
library (reactable)
library(rtweet)
library(rstudioapi)
library(glue)

twitter_token <- create_token(app = "Saif187e", 
                              consumer_key = keyring::key_get("my-database-2", "myusername"), 
                              consumer_secret = keyring::key_get("my-database", "myusername"),
                              access_token = keyring::key_get("my-database-3", "myusername"),
                              access_secret = keyring::key_get("my-database-1", "myusername"),
                              set_renv = TRUE)

tweet_df <- search_tweets("#Covid-19", n=10, include_rts = FALSE, token = NULL)
tweets <- tweet_df %>% select(user_id, status_id, text, favorite_count, retweet_count, screen_name, created_at)
tweet_data_table <- select(tweets, -user_id, -status_id)

reactable(tweet_data_table,
          filterable = TRUE, searchable = TRUE, bordered = TRUE, striped = TRUE, highlight = TRUE, defaultPageSize = 25, showPageSizeOptions = TRUE, showSortable = TRUE, pageSizeOptions = c(25,50,75,100,200), defaultSortOrder = "desc",
          columns = list(
            text = colDef (html = TRUE, minWidth = 190, resizable = TRUE),
            favorite_count = colDef (filterable = FALSE),
            retweet_count = colDef (filterable = FALSE),
            screen_name = colDef(defaultSortOrder = "asc"),
            created_at = colDef(defaultSortOrder = "asc")
          )
)

#######CHALLENGE #2

library(tidyverse) 
library(rvest)     
library(xopen)     
library(jsonlite)  
library(glue)      
library(stringi) 

url_1 <- "https://www.rosebikes.com/bikes/road/race"
url_2 <- "https://www.rosebikes.com/bikes/road/endurance"

html_bike_category_1 <- read_html(url_1)

bike_name_tbl_1 <- html_bike_category_1 %>%
  html_nodes(css = '.catalog-category-bikes__title-text') %>%
  html_text2() %>%
  enframe(name = "position", value = "model name")

bike_price_tbl_1 <- html_bike_category_1 %>%
  html_nodes(css = '.catalog-category-bikes__price-title') %>%
  html_text2() %>%
  stringr::str_remove(pattern = "[a-z]") %>% 
  stringr::str_remove(pattern = "[a-z]") %>% 
  stringr::str_remove(pattern = "[a-z]") %>% 
  stringr::str_remove(pattern = "[a-z]") %>% 
  stringr::str_remove(pattern = "[^a-zA-Z0-9_]") %>% 
  stringr::str_remove(pattern = "[^a-zA-Z0-9_]") %>% 
  enframe(name = "position", value = "price in Euros")

bike_desc_tbl_1 <- html_bike_category_1 %>%
  html_nodes(css = '.catalog-category-bikes__subtitle') %>%
  html_text2() %>%
  enframe(name = "position", value = "description")

bike_url_tbl_1  <- html_bike_category_1 %>%
  html_nodes(css = ".row .align-middle > a") %>%
  html_attr("href") %>%
  str_remove(pattern = "\\?.*") %>%
  enframe(name = "position", value = "subdirectory")

html_bike_category_2 <- read_html(url_2)

bike_name_tbl_2 <- html_bike_category_2 %>%
  html_nodes(css = '.catalog-category-bikes__title-text') %>%
  html_text2() %>%
  enframe(name = "position", value = "model name")

bike_price_tbl_2 <- html_bike_category_2 %>%
  html_nodes(css = '.catalog-category-bikes__price-title') %>%
  html_text2() %>%
  stringr::str_remove(pattern = "[a-z]") %>% 
  stringr::str_remove(pattern = "[a-z]") %>% 
  stringr::str_remove(pattern = "[a-z]") %>% 
  stringr::str_remove(pattern = "[a-z]") %>% 
  stringr::str_remove(pattern = "[^a-zA-Z0-9_]") %>% 
  stringr::str_remove(pattern = "[^a-zA-Z0-9_]") %>% 
  enframe(name = "position", value = "price in Euros")  

bike_desc_tbl_2 <- html_bike_category_2 %>%
  html_nodes(css = '.catalog-category-bikes__subtitle') %>%
  html_text2() %>%
  enframe(name = "position", value = "description")

bike_url_tbl_2  <- html_bike_category_2 %>%
  html_nodes(css = ".row .align-middle > a") %>%
  html_attr("href") %>%
  str_remove(pattern = "\\?.*") %>%
  enframe(name = "position", value = "subdirectory") 

bike_data_tbl_1     <-   (bike_name_tbl_1) %>% 
  left_join(bike_price_tbl_1) %>%
  left_join(bike_desc_tbl_1) %>% 
  left_join(bike_url_tbl_1)

bike_data_tbl_2 <- (bike_name_tbl_2) %>% 
  left_join(bike_price_tbl_2) %>%
  left_join(bike_desc_tbl_2) %>% 
  left_join(bike_url_tbl_2)

table <- rbind(bike_data_tbl_1, bike_data_tbl_2)
table