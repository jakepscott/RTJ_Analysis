# Loading Libraries -------------------------------------------------------
library(tidyverse)
library(Rspotify)
library(genius)
library(geniusr)

source("Spotify_Key.R")

# Getting Tibble of RTJ Albums and Songs -------------------------------------------------------
#Getting Artist ID
(RTJ<-searchArtist("Run the Jewels",token=keys))

#Getting Albums and their IDs
(RTJ_Albums<-getAlbums("4RnBFZRiMLRyZy0AzzTg2C",token=keys) %>% as_tibble())

#MAKING SURE TO REMOVE THEIR INSTRUMENTAL ALBUM
RTJ_Albums <- RTJ_Albums %>% distinct(name,.keep_all = T) %>% 
  filter(name!="Run the Jewels Instrumentals",
         name!="Meow The Jewels",
         name!="Spotify Sessions")

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

#Adding a column for artist name
RTJ_Songs <- RTJ_Songs %>% mutate(artist="Run The Jewels")

# Getting Lyrics ----------------------------------------------------------

Lyrics <- RTJ_Songs %>% mutate(Lyrics=1)

Lyrics <- Lyrics %>% mutate(song2=name) %>% 
  #Removing any parentheses, ususally used for features
  separate(col = song2, into = c("song2", "extra"), sep = " [(]") %>%
  select(-extra) %>% 
  #Removing hyphens (both with and without spaces in front), they usually come before a "live from XYZ"
  separate(song2, into = c("song2", "extra"), sep = " -") %>%
  select(-extra) %>% 
  separate(song2, into = c("song2", "extra"), sep = "-") %>% 
  select(-extra) %>%
  #Removing punctuation
  mutate(song2=str_remove_all(string = song2, pattern = "[[:punct:]]"))

#Geting lyrics using genius_lyrics
for (i in 1:nrow(Lyrics)) {
  tryCatch({
    print(i)
    Lyrics$Lyrics[i] <- genius_lyrics(artist = Lyrics$artist[i],
                                      song = Lyrics$song2[i],
                                      info = "simple") %>% 
      dplyr::select(lyric)
  }, error=function(e){print(e)}
  )
}

missed <- Lyrics[1,] %>% head(0)

for (i in 1:nrow(Lyrics)) {
  print(i)
  if (!is.character(Lyrics$Lyrics[[i]])) {
    missed[i,] <- Lyrics[i,]
  }   else {
    missed[i,] <- NA
  }
}

#Seeing how many songs I got the lyrics for versus how many I missed
missed <- missed %>% filter(!is.na(name))
percent_captured <- round((nrow(Lyrics)-nrow(missed))/nrow(Lyrics)*100,digits = 1)


# Getting Lyrics for the Songs genius_lyrics could not get ----------------
Lyrics$Lyrics[which(Lyrics$name=="a few words for the firing squad (radiation)")] <- get_lyrics_url("https://genius.com/Run-the-jewels-a-few-words-for-the-firing-squad-radiation-lyrics") %>% select(line)
Lyrics$Lyrics[which(Lyrics$name=="Hey Kids (Bumaye) [feat. Danny Brown]")] <- get_lyrics_url("https://genius.com/Run-the-jewels-hey-kids-bumaye-lyrics") %>% select(line)
Lyrics$Lyrics[which(Lyrics$name=="Thieves! (Screamed the Ghost) [feat. Tunde Adebimpe]")] <- get_lyrics_url("https://genius.com/Run-the-jewels-thieves-screamed-the-ghost-lyrics") %>% select(line)
Lyrics$Lyrics[which(Lyrics$name=="Panther Like a Panther (Miracle Mix) [feat. Trina]")] <- get_lyrics_url("https://genius.com/Run-the-jewels-panther-like-a-panther-miracle-mix-lyrics") %>% select(line)
Lyrics$Lyrics[which(Lyrics$name=="36 Inch Chain - Live From SXSW / 2015")] <- get_lyrics_url("https://genius.com/Run-the-jewels-36-inch-chain-live-from-sxsw-2015-lyrics") %>% select(line)
Lyrics$Lyrics[which(Lyrics$name=="Tougher Colder Killer - Live From SXSW / 2015")] <- get_lyrics_url("https://genius.com/Run-the-jewels-tougher-colder-killer-live-from-sxsw-2015-lyrics") %>% select(line)

#saveRDS(Lyrics,"Data/Lyrics.rds")

#Cleaning up the data to just have the name of the song, album, and lyrics
Lyrics <- read_rds("Data/Lyrics.rds")
RTJ_Songs <- Lyrics %>% select(album,name,Lyrics)

#Making the album column a factor, with the levels in order of album release date
RTJ_Songs$album <- factor(RTJ_Songs$album,
                          levels=c("Run the Jewels",
                                   "Run The Jewels 2",
                                   "Run the Jewels 3",
                                   "RTJ4"),
                          labels=c("Run the Jewels",
                                   "RTJ 2",
                                   "RTJ 3",
                                   "RTJ 4"))

#Saving the data
saveRDS(RTJ_Songs,"Data/RTJ_Songs.rds")


