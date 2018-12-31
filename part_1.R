library(rvest)
library(tidyverse)
library(furrr)

#--------functions--------

get_mep_overview <- function(x){
  data_frame(
    mep_name = x %>% html_nodes(".member-name") %>% html_text(),
    mep_link = x %>% html_nodes(".single-member-container a") %>%
      html_attr("href") %>%
      paste0("http://www.europarl.europa.eu",.),
    mep_group = x %>% html_nodes(".ep-layout_group .ep_name") %>% html_text(),
    mep_party = x %>% html_nodes(".ep-layout_party .ep_name") %>% html_text(),
    mep_country = x %>% html_nodes(".ep-layout_country .ep_name") %>% html_text(),
    mep_id = mep_link %>% str_extract("\\d+$")
  )
}

get_mep_data <- function(x){
  page <- read_html(x)
  
  data_frame(
    mep_id = x %>% str_extract("\\d+$"),
    website = page %>% html_nodes(".link_website") %>% html_attr("href") %>% list(),
    email = page %>% html_nodes(".link_email a") %>% html_attr("href") %>% list(),
    twitter = page %>% html_nodes(".link_twitt a") %>% html_attr("href") %>% list(),
    facebook = page %>% html_nodes(".link_fb a") %>% html_attr("href") %>% list(),
    youtube = page %>% html_nodes(".link_youtube a") %>% html_attr("href") %>% list(),
    linkedin = page %>% html_nodes(".link_linkedin a") %>% html_attr("href") %>% list(),
    instagram = page %>% html_nodes(".link_instagram a") %>% html_attr("href") %>% list(),
    mep_birthday = page %>% html_nodes("#birthDate") %>% html_text() %>% list()
  )
}

#--------data--------

mep_page <- read_html("http://www.europarl.europa.eu/meps/en/full-list/all")

mep_df <- get_mep_overview(mep_page)

plan(multiprocess)

mep_social_media <- future_map_dfr(mep_df$mep_link, get_mep_data, .progress = TRUE)

#------clean data--------

mep_twitter <- mep_social_media %>%
  unnest(twitter) %>%
  mutate(screen_name = str_remove(twitter, "\\?lang=[a-z]{2}")) %>% # removes the "?lang=es" at the end of the link
  mutate(screen_name = str_remove(screen_name, "\\/media")) %>% # removes the media at the end of some links
  mutate(screen_name = str_remove(screen_name, "\\/$")) %>% # removes slashes at the end
  mutate(screen_name = str_extract(screen_name, "\\w+$")) %>% # extracts the last word before the end of the string
  mutate(screen_name = str_to_lower(screen_name))

write_rds(mep_twitter, "mep_twitter.RDS")