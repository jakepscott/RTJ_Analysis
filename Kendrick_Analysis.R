# Loading Libraries -------------------------------------------------------
library(tidyverse)
library(Rspotify)
library(genius)
library(geniusr)

source("Spotify_Key.R")

# Getting Tibble of Kendrick Albums and Songs -------------------------------------------------------
#Getting Artist ID
(Kendrick<-searchArtist("Kendrick Lamar",token=keys))

#Getting Albums and their IDs
(Kendrick_Albums<-getAlbums("2YZyLoL8N0Wb9xBt1NhZWg",token=keys) %>% as_tibble())

#MAKING SURE TO REMOVE THEIR INSTRUMENTAL ALBUM
Kendrick_Albums <- Kendrick_Albums %>% distinct(name,.keep_all = T) %>% 
  filter(name=="DAMN." |
         name=="To Pimp A Butterfly" |
         name=="good kid, m.A.A.d city" |
         name=="Section.80")

#Getting an empty tibble with the proper column names
Kendrick_Songs <- tibble(id=character(0),name=character(0),track_numer=integer(0),album=character(0))

#Making the tibble of Albums and their songs
for (i in Kendrick_Albums$id) {
  print(i)
  #Get the songs for that album, turn it into a tibble, grab just the columns we need, unlist them, 
  #Add a column for the album name using the album id
  tobind <- getAlbum(i,token=keys) %>% as_tibble() %>% select(id, name,track_number) %>% 
    mutate_all(unlist) %>% mutate(album=Kendrick_Albums %>% filter(id==i) %>% pull(name))
  #Bind the tibble made from the given album id to the big tibble
  Kendrick_Songs <- Kendrick_Songs %>% rbind(tobind)
}

#Adding a column for artist name
Kendrick_Songs <- Kendrick_Songs %>% mutate(artist="Kendrick Lamar")

# Getting Lyrics ----------------------------------------------------------

Lyrics <- Kendrick_Songs %>% mutate(Lyrics=1)

Lyrics <- Lyrics %>% mutate(song2=name) %>% 
  #Removing any parentheses, ususally used for features
  separate(col = song2, into = c("song2", "extra"), sep = " [(]") %>%
  select(-extra) %>% 
  #Removing any FEAT.
  separate(col = song2, into = c("song2", "extra"), sep = " FEAT") %>%
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
Lyrics$Lyrics[which(Lyrics$name=="For Free? - Interlude")] <- get_lyrics_url("https://genius.com/Kendrick-lamar-for-free-interlude-lyrics") %>% select(line)
Lyrics$Lyrics[which(Lyrics$name=="For Sale? - Interlude  ")] <- get_lyrics_url("https://genius.com/Kendrick-lamar-for-sale-interlude-lyrics") %>% select(line)
Lyrics$Lyrics[which(Lyrics$name=="You Ain't Gotta Lie (Momma Said)")] <- get_lyrics_url("https://genius.com/Kendrick-lamar-you-aint-gotta-lie-momma-said-lyrics") %>% select(line)
Lyrics$Lyrics[which(Lyrics$name=="F*ck Your Ethnicity")] <- get_lyrics_url("https://genius.com/Kendrick-lamar-fuck-your-ethnicity-lyrics") %>% select(line)
Lyrics$Lyrics[which(Lyrics$name=="No Make-Up (Her Vice) (feat. Colin Munroe)")] <- get_lyrics_url("https://genius.com/Kendrick-lamar-no-makeup-her-vice-lyrics") %>% select(line)
Lyrics$Lyrics[which(Lyrics$name=="Poe Mans Dreams (His Vice) (feat. GLC)")] <- get_lyrics_url("https://genius.com/Kendrick-lamar-poe-mans-dreams-his-vice-lyrics") %>% select(line)
Lyrics$Lyrics[which(Lyrics$name=="Keisha's Song (Her Pain) (feat. Ashtro Bot)")] <- get_lyrics_url("https://genius.com/Kendrick-lamar-keishas-song-her-pain-lyrics") %>% select(line)
Lyrics$Lyrics[which(Lyrics$name=="Kush & Corinthians (feat. BJ The Chicago Kid)")] <- get_lyrics_url("https://genius.com/Kendrick-lamar-kush-and-corinthians-his-pain-lyrics") %>% select(line)
Lyrics$Lyrics[which(Lyrics$name=="Ab-Souls Outro (feat. Ab-Soul)")] <- get_lyrics_url("https://genius.com/Kendrick-lamar-ab-souls-outro-lyrics") %>% select(line)

#saveRDS(Lyrics,"Data/Raw_Kendrick_Lyrics.rds")

#Cleaning up the data to just have the name of the song, album, track number, and lyrics
Lyrics <- read_rds("Data/Raw_Kendrick_Lyrics.rds")
Kendrick_Songs <- Lyrics %>% select(album,name,Lyrics,track_number)

#Making the album column a factor, with the levels in order of album release date
Kendrick_Songs$album <- factor(Kendrick_Songs$album,
                          levels=c("DAMN.",
                                   "To Pimp A Butterfly",
                                   "good kid, m.A.A.d city",
                                   "Section.80"),
                          labels=c("DAMN.",
                                   "To Pimp A Butterfly",
                                   "good kid, m.A.A.d city",
                                   "Section.80"))

#Saving the data
saveRDS(Kendrick_Songs,"Data/Kendrick_Songs.rds")



# Getting Lyrical Data ----------------------------------------------------
Kendrick_Songs <- read_rds("Data/Kendrick_Songs.rds")

# Tokenizing the lyrics ---------------------------------------------------
#Right now the lyrics are stored in a list column, with each entry for each song being a set of 
#strings, approximately one string per line in a song. I need to get it so that each entry in the 
#lyric column in just one line for that song

#First I make an empty tibble for binding
Kendrick_lyrics <- tibble(lyrics=character(0),
                     song=character(0),
                     album=character(0),
                     track_number=integer(0))

#Here I take a given entry, unlist it, turn it into a tibble, and pair that tibble of lyrics lines 
#with the name of the song it came from. This results in a tibble where each line from each song 
#is paired with the song (and album) it came from
for (i in 1:nrow(Kendrick_Songs)) {
  print(i)
  to_bind <- Kendrick_Songs$Lyrics[[i]] %>% tibble(lyrics=.) %>% mutate(song=Kendrick_Songs$name[i],
                                                                   album=Kendrick_Songs$album[i],
                                                                   track_number=Kendrick_Songs$track_number[i])
  Kendrick_lyrics <- Kendrick_lyrics %>% rbind(to_bind)
}

#Reordering the columns purely for readability reasons
Kendrick_lyrics <- Kendrick_lyrics %>% select(album,song,lyrics,track_number)

#Unnesting tokens (so each row is a word and the album+song it came from)
Kendrick_lyrics <- Kendrick_lyrics %>% unnest_tokens(word,lyrics)

