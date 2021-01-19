





songlist_tibble[157,1]

html_nodes("[class$='blueTxt']") %>% 
  html_text()
#html_attr(., "id")
html_nodes("tr") %>% 
  html_nodes("[id^='tlp_']")
html_nodes("[id^='tlptr_']") %>% 
  
  View(artist_name)
html_nodes("[id$='playcolumn']") %>% 
  html_text()

html_nodes(., "[id]")




test4 <- my_scrape3 %>% 
  html_nodes("td") %>% 
  html_attr("class")

test5 <- my_scrape3 %>% 
  html_nodes(".tlToogleData") %>%
  html_nodes("table")
html_table()

test6 <- my_scrape3 %>% 
  html_nodes("table") 

table_class <- which(html_attr(test6, "class") == "default full tl hover")

test7 <- html_table(test6[table_class], fill = T) %>%
  .[[1]]

test8 <- tibble(
  no. = test7[,1],
  song = test7[,3]) %>% 
  as_tibble() %>% 
  filter(!is.na(song))


#colnames<-(test7, c("#", "na", "title", "n", "t"))
#  as_tibble()
#  filter(!is.na("Artist - Title (Remix)"))


View(test7[1:241,] %>% cbind(test_tibble[1:241,]) %>% 
       select(1,3,6))


test4
my_scrape2 %>% 
  html_nodes("div") %>%
  html_attr("id")

my_scrape2 %>% 
  html_nodes("[id$='tlp_1']") %>%
  html_attr(., "id")

test <- my_scrape2 %>% 
  html_nodes("meta")

xml2::
  test[[1]]

my_scrape2 %>% 
  html_nodes(".blueTxt")

test %>% 
  html_nodes(".name")

test %>% 
  html_nodes(".tlp6_tracknumber_value")

test_tibble <- tibble(
  artists = artists,
  names = names) %>% 
  mutate(split_names = str_split(names, pattern = " - "),
         n_dash = map_dbl(.x = split_names, .f = length),
         song_name = map_chr(split_names, 2),
         artists2 = map_chr(split_names, 1),
         match = case_when(
           artists == artists2 ~ 1,
           TRUE ~ 0
         )) %>% 
  select(artists, song_name, names)

analysis <- test_tibble %>% 
  group_by(song_name) %>% 
  summarise(n = n()) %>% 
  arrange(-n)




length(test_tibble$song[[4]])  
map_chr(test_tibble$song, 1)


#Get entire page using GET (not as effective, think this is more important for API)
my_scrape <- GET("https://www.1001tracklists.com/tracklist/9l2wdv1/two-friends-big-bootie-mix-018-2020-10-26.html")

test <- my_scrape %>% 
  content(as="text") %>% 
  fromJSON()

#Found the div class for each song box
test <- my_scrape2 %>% 
  html_nodes(".tlToogleData") 

#Grab the <meta itemprop content for names and artists
names <- test %>% 
  html_nodes("[itemprop='name']") %>% html_attr("content") 

artists <- test %>% 
  html_nodes("[itemprop='byArtist']") %>% html_attr("content") 



