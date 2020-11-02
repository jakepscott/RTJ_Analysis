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


# Album Covers for Bars ---------------------------------------------------

#Getting album colors

album_covers <- tibble(album=c("Run the Jewels", "RTJ 2", "RTJ 3", "RTJ 4"),
                       album_cover=c("Data/RTJ1_Album_Cover.PNG",
                                     "Data/RTJ2_Album_Cover.PNG",
                                     "Data/RTJ3_Album_Cover.PNG",
                                     "Data/RTJ4_Album_Cover.PNG"))

Disp_Relative_Importance_Clean <- left_join(Disp_Relative_Importance_Clean,album_covers)
Disp_Relative_Importance_Clean$album <- factor(Disp_Relative_Importance_Clean$album,
                                          levels=c("Run the Jewels",
                                                   "RTJ 2",
                                                   "RTJ 3",
                                                   "RTJ 4"),
                                          labels=c("Run the Jewels",
                                                   "RTJ 2",
                                                   "RTJ 3",
                                                   "RTJ 4"))

#Getting smoothed top 10

top_10 <- Disp_Relative_Importance_Clean %>% 
  group_by(album) %>% 
  top_n(10,difference) %>% 
  ungroup()

smooth_top_10 <- top_10 %>% head(0) %>% mutate(difference_smooth=double(0))

for (i in 1:nrow(top_10)) {
  print(i)
  for (z in seq(0,top_10$difference[i],by=.00075)) {
    to_bind <- top_10[i,] %>% mutate(difference_smooth=z)
    smooth_top_10 <- smooth_top_10 %>% rbind(to_bind)
  }
}

smooth_top_10 %>%
  mutate(word_clean=reorder_within(x=word_clean,by = difference,within = album)) %>% 
  ggplot(aes(x=word_clean,y=difference_smooth,fill=album)) +
  geom_image(aes(image=album_cover),asp = 2, size = .045) +
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
