library(rvest)
library(htmltools)
library(tidyverse)
library(jsonlite)
library(httr)

#Second try using read_html from xml2 package
my_scrape3 <- read_html("https://www.1001tracklists.com/tracklist/9l2wdv1/two-friends-big-bootie-mix-018-2020-10-26.html")


#trying to download the copy and pasted html
track_number <- my_scrape3 %>% 
  html_nodes("[id^='tlp']") %>%
  html_nodes("[id$='number_column']") %>% 
  html_text()

cue_time <- my_scrape3 %>% 
  html_nodes("[id^='cue_']") %>%
  html_text()

song_name <- my_scrape3 %>%
  html_nodes("[id^='tlp']") %>% 
  html_nodes("[id^='tlptr_']") %>%
  html_nodes("[class='tlToogleData']") %>% 
  html_nodes("[itemprop='name']") %>% html_attr("content") 

artist_name <- my_scrape3 %>%
  html_nodes("[id^='tlp']") %>% 
  html_nodes("[id^='tlptr_']") %>%
  html_nodes("[class='tlToogleData']") %>% 
  html_nodes("[itemprop='byArtist']") %>% html_attr("content") 

songlist_tibble <- tibble(
  track_number = track_number,
  artist_name = artist_name,
  song_name = song_name) %>% 
  mutate(track_number = trimws(track_number),
         track_number = na_if(track_number, "")) %>% 
  filter(!is.na(track_number)) %>% 
  mutate(track_number = gsub("grab", "", track_number, ignore.case = T),
         track_number_string = str_split(track_number, " "),
         track_number = map_chr(track_number_string, 1),
         track_number = na_if(track_number, "w/")) %>% 
  fill(track_number) %>% 
  mutate(track_number = as.numeric(track_number),
         track_start_time = map_chr(track_number_string, 4),
         track_start_time = na_if(track_start_time, "")) %>% 
  fill(track_start_time) %>% 
  mutate(track_start_time = case_when(
    is.na(track_start_time) ~ "0:00",
    TRUE ~ track_start_time
  )) %>% 
  mutate(track_end_time = case_when(
    lead(track_start_time, 1) != track_start_time ~ lead(track_start_time, 1))) %>% 
  fill(track_end_time, .direction = "up") %>% 
  mutate(song_name_list = str_split(song_name, " - ", n = 2),
         song_name = map_chr(song_name_list, 2),
         track_end_time = case_when(
          track_number == 44 ~ "61:09",
          TRUE ~ track_end_time),
         volume = 18) %>% 
  select(volume, track_number, track_start_time, track_end_time, artist_name, song_name) 






