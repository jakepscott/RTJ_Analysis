# Loading Libraries and Data ----------------------------------------------
library(tidyverse)
library(readr)
windowsFonts(`Roboto Condensed`=windowsFont("Roboto Condensed"))
RTJ_lyrics <- read_rds("Data/RTJ_lyrics.rds")
data("stop_words")
#Removing stop words
RTJ_lyrics <- RTJ_lyrics %>% anti_join(stop_words)

#Setting up empty tible. This will contain data on each album-word combination, including the 
#the proportion of words outside that album that are a given word, and a scaling factor, which measures
#what proportion of songs outside the given album does the given word appear in
outside_values <- tibble(album=character(0),
                         word=character(0),
                         prop_outside=double(0),
                         outside_scaling=double(0))
for (i in c("Run the Jewels", "RTJ 2", "RTJ 3", "RTJ 4")) {
  print(i)
  for (z in RTJ_lyrics %>% filter(album==i) %>% distinct(word) %>% pull(word)) {
    #Get the number of words outside the album
    total_outside_words <- RTJ_lyrics %>% 
      filter(album!=i) %>% 
      distinct(word) %>% 
      nrow()
    #Get the number of times the given word appears outside the given album
    total_word_of_interest_outside <- RTJ_lyrics %>% 
      filter(album!=i, word==z) %>% 
      nrow()
    #Make the proportion
    prop_outside_to_paste <- (total_word_of_interest_outside/total_outside_words)
    
    #Get the number of songs outside the given album
    num_outside_songs <- RTJ_lyrics %>% filter(album!=i) %>% distinct(song) %>% nrow() #num of outside songs
    #Get the number of songs outside the given album that the given word appears in
    songs_word_is_in <- RTJ_lyrics %>%
      filter(album!=i, 
             word==z) %>%
      distinct(song) %>% 
      nrow()
    #Make into proportion (which I use as the scaling factor)
    outside_scaling_to_paste <- songs_word_is_in/num_outside_songs 
    
    #Make into a tibble
    to_bind <- tibble(album=i,
                      word=z,
                      prop_outside=prop_outside_to_paste,
                      outside_scaling=outside_scaling_to_paste)
    
    #Bind onto a big tibble which will have each album-word pair and the corresponding prop_outside column
    outside_values <- outside_values %>% rbind(to_bind)
  }
}

#Multiply the proportion of words outside the given album made up by the given word by the 
#proption of songs outside the given album the given word is in (dispersion_corrected_measure)
outside_values <- outside_values %>% mutate(scaled_prop_outside=prop_outside*outside_scaling)

#Combine this importance measure with the lyrics tibble
Disp_Relative_Importance <- RTJ_lyrics %>% 
  select(album,song,word,word_clean) %>% 
  left_join(outside_values,by=c("album","word"))

#Making album back into an ordered factor
Disp_Relative_Importance$album <- factor(Disp_Relative_Importance$album,
                                    levels=c("Run the Jewels",
                                             "RTJ 2",
                                             "RTJ 3",
                                             "RTJ 4"),
                                    labels=c("Run the Jewels",
                                             "RTJ 2",
                                             "RTJ 3",
                                             "RTJ 4"))

#Get percent of words in given album made up by given word
Disp_Relative_Importance <- Disp_Relative_Importance %>% 
  #Get number of times given word appears in given album 
  group_by(album,word) %>%
  mutate(n=n()) %>% 
  ungroup() %>% 
  #Get number of words in that album
  group_by(album) %>% 
  mutate(total_words=n()) %>% 
  ungroup() %>%
  #Divide number of times given word appears by total words to get proportion
  mutate(prop_inside=(n/total_words)) 

#Get proportion of songs in a given album that a given word appears in
Disp_Relative_Importance <- Disp_Relative_Importance %>% 
  group_by(album,word) %>% 
  #Get number of songs given word appears in
  mutate(song_appearances=length(unique(song))) %>% 
  ungroup() %>% 
  group_by(album) %>% 
  #get number of songs in given album
  mutate(total_songs=length(unique(song))) %>% 
  ungroup() %>% 
  #Divide number of songs given word appears in by total number of songs in that album
  mutate(prop_songs_in=(song_appearances/total_songs)) %>% 
  #Multipling proportion of words in an album made up by word x by the proportion of songs word x is in
  mutate(scaled_prop_inside=prop_inside*prop_songs_in) 

#Getting difference between scaled inside proportion and scaled outside proportion, which is the scaled
#importance measure
Disp_Relative_Importance <- Disp_Relative_Importance %>% mutate(difference=scaled_prop_inside-scaled_prop_outside)

#Saving so I don't have to run the loop each time
saveRDS(Disp_Relative_Importance,"Data/Disp_Relative_Importance.rds")

#Getting just distinct entries (only need one row for each album-word pair now since I am done counting)
Disp_Relative_Importance <- read_rds("Data/Disp_Relative_Importance.rds")
Disp_Relative_Importance_Clean <- Disp_Relative_Importance %>% distinct(album,word,.keep_all = T)

#Making a quick and dirty plot
Disp_Relative_Importance_Clean %>% 
  group_by(album) %>% 
  top_n(10,difference) %>% 
  ungroup() %>% 
  mutate(word_clean=reorder_within(x=word_clean,by = difference,within = album)) %>% 
  ggplot(aes(x=word_clean,y=difference,fill=album)) +
  geom_col() +
  facet_wrap(~album,scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  scale_y_continuous(expand=c(0,0))
