# Loading Libraries -------------------------------------------------------
library(tidyverse)
library(Rspotify)
library(genius)
library(geniusr)
library(readr)
library(tidytext)
library(ggimage)

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
                          levels=c("Section.80",
                                   "good kid, m.A.A.d city",
                                   "To Pimp A Butterfly",
                                   "DAMN."
                                   ),
                          labels=c("Section.80",
                                   "good kid, m.A.A.d city",
                                   "To Pimp A Butterfly",
                                   "DAMN."))

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


#Censoring Words
Kendrick_lyrics <- Kendrick_lyrics %>% mutate(word_clean=case_when(word=="fuck"~"f*ck",
                                                         word=="fucked"~"f*cked",
                                                         word=="fucking"~"f*cking",
                                                         word=="shit"~"sh*t",
                                                         word=="bitch"~"b*tch",
                                                         word=="dick"~"d*ck",
                                                         word=="nigga"~"n*gga",
                                                         TRUE~as.character(word)))

#Removing missing words
Kendrick_lyrics <- Kendrick_lyrics %>% filter(!is.na(word))

#Saving
saveRDS(Kendrick_lyrics,"Data/Kendrick_lyrics.rds")


# Relative Importance Measures --------------------------------------------

#Getting font for graphs
windowsFonts(`Roboto Condensed`=windowsFont("Roboto Condensed"))

#Reading in Data
Kendrick_lyrics <- read_rds("Data/Kendrick_lyrics.rds")
data("stop_words")
Kendrick_lyrics <- Kendrick_lyrics %>% anti_join(stop_words)


# tf_idf ------------------------------------------------------------------
#Getting tf_idf values
tf_idf <- Kendrick_lyrics %>% count(album,word_clean) %>% bind_tf_idf(term = word_clean,document = album,n = n)

#Getting top 10 words for each album by tf-idf
top_10_tf_idf <- tf_idf %>% 
  group_by(album) %>% 
  top_n(10,tf_idf) %>% 
  ungroup()

#Making the plot smooth so I can use album covers for the bars
#By smooth I mean I need values to place an album from zero to the tf-idf value for each word
#So I need a row for 0 to .00312 by increments of .001 for buzzin in DAMN. 
smooth_top_10_tf_idf <- top_10_tf_idf %>% head(0) %>% mutate(difference_smooth=double(0))

for (i in 1:nrow(top_10_tf_idf)) {
  print(i)
  for (z in seq(0,top_10_tf_idf$tf_idf[i],by=.001)) {
    to_bind <- top_10_tf_idf[i,] %>% mutate(tf_idf_smooth=z)
    smooth_top_10_tf_idf <- smooth_top_10_tf_idf %>% rbind(to_bind)
  }
}

#Getting the album covers themselves
album_covers <- tibble(album=c("DAMN.", "To Pimp A Butterfly", "good kid, m.A.A.d city", "Section.80"),
                       album_cover=c("Data/DAMN_Album.jpg",
                                     "Data/To_Pimp_A_Butterfly.jpg",
                                     "Data/goodkidmadcity.jpg",
                                     "Data/Section80.jpg"))

smooth_top_10_tf_idf <- left_join(smooth_top_10_tf_idf,album_covers)

#Making album back into an ordered factor
smooth_top_10_tf_idf$album <- factor(smooth_top_10_tf_idf$album,
                                     levels=c("Section.80",
                                              "good kid, m.A.A.d city",
                                              "To Pimp A Butterfly",
                                              "DAMN."
                                     ),
                                     labels=c("Section.80",
                                              "good kid, m.A.A.d city",
                                              "To Pimp A Butterfly",
                                              "DAMN."))

smooth_top_10_tf_idf %>%
  mutate(word_clean=reorder_within(x=word_clean,by = tf_idf,within = album)) %>% 
  ggplot(aes(x=word_clean,y=tf_idf_smooth,fill=album)) +
  geom_image(aes(image=album_cover),asp = 2, size = .04) +
  facet_wrap(~album,scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  labs(y="Relative Importance",
       caption = "Plot: @jakepscott2020 | Data: Spotify and Genius",
       title="Which words are uniquely important for each album?",
       subtitle = "Relative importance calculated by subtracting percent of words made up by a given word outside of a given album from the percent of total words within that album made up by that word") +
  theme_minimal(base_family = "Roboto Condensed", base_size = 12) +
  theme(plot.title.position = "plot",
        plot.title = element_text(face="bold", size = rel(2.5), color="white"),
        plot.subtitle = element_text(size=rel(1),colour = "grey70"),
        plot.caption = element_text(face = "italic", size = rel(0.8), 
                                    color = "grey70"),
        axis.title.y = element_blank(),
        axis.title.x = element_text(color="white"),
        axis.text = element_text(color="white",size = rel(1)), 
        panel.grid = element_blank(),
        strip.text = element_text(face="bold",colour = "white",size=rel(1.2)),
        plot.background = element_rect(fill="grey10"))


# Getting Proportional Importance Values ----------------------------------
#First I make an empty tibble which will eventually contain the album, word, and proportion
#of all words made up by that word outside the given album. So if the album is DAMN. and the word
#is run, the percent_outside column will be the proportion of words outside DAMN. that are "run"
outside_values <- tibble(album=character(0),word=character(0),percent_outside=double(0))
tictoc::tic()
for (i in c("Section.80", "good kid, m.A.A.d city", "To Pimp A Butterfly", "DAMN.")) {
  print(i)
  for (z in Kendrick_lyrics %>% filter(album==i) %>% distinct(word) %>% pull(word)) {
    #Get the number of words outside the album
    total_outside_words <- Kendrick_lyrics %>% 
      filter(album!=i) %>% 
      distinct(word) %>% 
      nrow() 
    #Get the number of times the given word appears outside the given album
    total_word_of_interest_outside <- Kendrick_lyrics %>% 
      filter(album!=i,
             word==z) %>% 
      nrow()
    #Make the proportion
    percent_outside_to_paste <- (total_word_of_interest_outside/total_outside_words)*100
    #Make into a tibble
    to_bind <- tibble(album=i,word=z,percent_outside=percent_outside_to_paste)
    #Bind onto a big tibble which will have each album-word pair and the corresponding percent_outside column
    outside_values <- outside_values %>% rbind(to_bind)
  }
}
tictoc::toc()
#Join the outside_values tibble to the Kendrick_lyrics tibble, so for each word with will now have the value
#for the proportion of words outside the given album are made up by that given word. So if the album is
#DAMN and the word is "week", the percent_outside column, which is .029, means that week makes up .029% of words in 
#"Section.80", "good kid, m.A.A.d city", and "To Pimp A Butterfly"
Relative_Importance <- Kendrick_lyrics %>% 
  select(album,word,word_clean) %>% 
  left_join(outside_values,by=c("album","word"))

#Making album back into an ordered factor
Relative_Importance$album <- factor(Relative_Importance$album,
                                    levels=c("Section.80",
                                             "good kid, m.A.A.d city",
                                             "To Pimp A Butterfly",
                                             "DAMN."
                                    ),
                                    labels=c("Section.80",
                                             "good kid, m.A.A.d city",
                                             "To Pimp A Butterfly",
                                             "DAMN."))

# Getting Within Album Proportion -----------------------------------------
Relative_Importance <- Relative_Importance %>% 
  group_by(album,word) %>% 
  mutate(n=n()) %>% 
  ungroup() %>% 
  group_by(album) %>% 
  mutate(total_words=n()) %>% 
  ungroup() %>%
  mutate(percent_inside=(n/total_words)*100)


# Getting Difference ------------------------------------------------------
Relative_Importance <- Relative_Importance %>% mutate(difference=percent_inside-percent_outside)

#Saving so I don't need to run the above for loop each time
saveRDS(Relative_Importance,"Data/Kendrick_Relative_Importance.rds")

# Graphing ----------------------------------------------------------------
Relative_Importance <- read_rds("Data/Kendrick_Relative_Importance.rds")
Relative_Importance_Clean <- Relative_Importance %>% distinct(album,word,.keep_all = T)

# Graphing with Album Covers ----------------------------------------------
#Smoothing the graph
top_10 <- Relative_Importance_Clean %>% 
  group_by(album) %>% 
  top_n(10,difference) %>% 
  ungroup()

smooth_top_10 <- top_10 %>% head(0) %>% mutate(difference_smooth=double(0))

for (i in 1:nrow(top_10)) {
  print(i)
  for (z in seq(0,top_10$difference[i],by=.075)) {
    to_bind <- top_10[i,] %>% mutate(difference_smooth=z)
    smooth_top_10 <- smooth_top_10 %>% rbind(to_bind)
  }
}

#Getting the album covers
album_covers <- tibble(album=c("DAMN.", "To Pimp A Butterfly", "good kid, m.A.A.d city", "Section.80"),
                       album_cover=c("Data/DAMN_Album.jpg",
                                     "Data/To_Pimp_A_Butterfly.jpg",
                                     "Data/goodkidmadcity.jpg",
                                     "Data/Section80.jpg"))
#Joining to the data
smooth_top_10 <- left_join(smooth_top_10,album_covers)
#Making the album column an ordered factor again 
smooth_top_10$album <- factor(smooth_top_10$album,
                              levels=c("Section.80",
                                       "good kid, m.A.A.d city",
                                       "To Pimp A Butterfly",
                                       "DAMN."
                              ),
                              labels=c("Section.80",
                                       "good kid, m.A.A.d city",
                                       "To Pimp A Butterfly",
                                       "DAMN."))


smooth_top_10 %>%
  mutate(word_clean=reorder_within(x=word_clean,by = difference,within = album)) %>% 
  ggplot(aes(x=word_clean,y=difference_smooth,fill=album)) +
  geom_image(aes(image=album_cover),asp = 2, size = .04,nudge_y=-.025, nudge_x = .001) +
  facet_wrap(~album,scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  labs(y="Relative Importance",
       caption = "Plot: @jakepscott2020 | Data: Spotify and Genius",
       title="Which words are uniquely important for each album?",
       subtitle = "Relative importance calculated by subtracting percent of words made up by a given word outside of a given album from the percent of total words within that album made up by that word") +
  theme_minimal(base_family = "Roboto Condensed", base_size = 12) +
  theme(plot.title.position = "plot",
        plot.title = element_text(face="bold", size = rel(2.5), color="white"),
        plot.subtitle = element_text(size=rel(1),colour = "grey70"),
        plot.caption = element_text(face = "italic", size = rel(0.8), 
                                    color = "grey70"),
        axis.title.y = element_blank(),
        axis.title.x = element_text(color="white"),
        axis.text = element_text(color="white",size = rel(1)), 
        panel.grid = element_blank(),
        strip.text = element_text(face="bold",colour = "white",size=rel(1.2)),
        plot.background = element_rect(fill="grey20"))
