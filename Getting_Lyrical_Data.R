# Loading Libraries and data ------------------------------------------------------------
library(tidyverse)
library(tidytext)

RTJ_Songs <- read_rds("Data/RTJ_Songs.rds")


# Tokenizing the lyrics ---------------------------------------------------
#Right now the lyrics are stored in a list column, with each entry for each song being a set of 
#strings, approximately one string per line in a song. I need to get it so that each entry in the 
#lyric column in just one line for that song

#First I make an empty tibble for binding
RTJ_lyrics <- tibble(lyrics=character(0),
                     song=character(0),
                     album=character(0),
                     track_number=integer(0))

#Here I take a given entry, unlist it, turn it into a tibble, and pair that tibble of lyrics lines 
#with the name of the song it came from. This results in a tibble where each line from each song 
#is paired with the song (and album) it came from
for (i in 1:nrow(RTJ_Songs)) {
  print(i)
  to_bind <- RTJ_Songs$Lyrics[[i]] %>% tibble(lyrics=.) %>% mutate(song=RTJ_Songs$name[i],
                                                                   album=RTJ_Songs$album[i],
                                                                   track_number=RTJ_Songs$track_number[i])
  RTJ_lyrics <- RTJ_lyrics %>% rbind(to_bind)
}

#Reordering the columns purely for readability reasons
RTJ_lyrics <- RTJ_lyrics %>% select(album,song,lyrics,track_number)

#Unnesting tokens (so each row is a word and the album+song it came from)
RTJ_lyrics <- RTJ_lyrics %>% unnest_tokens(word,lyrics)

#Censoring Words
RTJ_lyrics <- RTJ_lyrics %>% mutate(word_clean=case_when(word=="fuck"~"f*ck",
                                word=="fucked"~"f*cked",
                                word=="shit"~"sh*t",
                                word=="bitch"~"b*tch",
                                word=="dick"~"d*ck",
                                TRUE~as.character(word)))

#Saving
saveRDS(RTJ_lyrics,"Data/RTJ_lyrics.rds")
