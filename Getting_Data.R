# Loading Libraries -------------------------------------------------------
library(tidyverse)
library(Rspotify)
library(genius)

source("Spotify_Key.R")

# Getting Tibble of RTJ Albums and Songs -------------------------------------------------------
#Getting Artist ID
(RTJ<-searchArtist("Run the Jewels",token=keys))

#Getting Albums and their IDs
(RTJ_Albums<-getAlbums("4RnBFZRiMLRyZy0AzzTg2C",token=keys) %>% as_tibble())
RTJ_Albums <- RTJ_Albums %>% distinct(name,.keep_all = T)

#Getting an empty tibble with the proper column names
RTJ_Songs <- tibble(id=character(0),name=character(0),track_numer=integer(0),album=character(0))

#Making the tibble of Albums and their songs
for (i in 1:nrow(RTJ_Albums)) {
  print(i)
  #Get the given album ID
  album_id <- RTJ_Albums$id[i]
  #Get the songs for that album, turn it into a tibble, grab just the columns we need, unlist them, 
  #Add a column for the album name using the album id
  tobind <- getAlbum(album_id,token=keys) %>% as_tibble() %>% select(id, name,track_number) %>% 
    mutate_all(unlist) %>% mutate(album=RTJ_Albums %>% filter(id==album_id) %>% pull(name))
  #Bind the tibble made from the given album id to the big tibble
  RTJ_Songs <- RTJ_Songs %>% rbind(tobind)
}
