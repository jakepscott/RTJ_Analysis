outside_values <- tibble(album=character(0),
                         word=character(0),
                         prop_outside=double(0),
                         outside_scaling=double(0))
for (i in 1:4) {
  #For each album...
  print(paste("i is",i))
  album_names <- c("Run the Jewels", "RTJ 2", "RTJ 3", "RTJ 4")
  album_to_analyze <- album_names[i]
  for (z in 1:nrow(RTJ_lyrics %>% filter(album==album_to_analyze) %>% distinct(word))) {
    #For each word in the given album
    print(paste("z is",z))
    words <- RTJ_lyrics %>% filter(album==album_to_analyze) %>%  distinct(word) %>% pull(word)
    word_of_interest <- words[z]
    
    #Get the number of words outside the album
    total_outside_words <- RTJ_lyrics %>% filter(album!=album_to_analyze) %>% distinct(word) %>% nrow()
    #Get the number of times the given word appears outside the given album
    total_word_of_interest_outside <- RTJ_lyrics %>% filter(album!=album_to_analyze,
                                                            word==word_of_interest) %>% nrow()
    
    
    #Make the proportion
    prop_outside_to_paste <- (total_word_of_interest_outside/total_outside_words)
    
    #Get the percent of songs outside the album of interest that the given word is in
    num_outside_songs <- RTJ_lyrics %>% filter(album!=album_to_analyze) %>% distinct(song) %>% nrow() #num of outside songs
    songs_word_is_in <- RTJ_lyrics %>% #Get number of songs word is in
      filter(album!=album_to_analyze, 
             word==word_of_interest) %>% distinct(song) %>% nrow()
    outside_scaling_to_paste <- songs_word_is_in/num_outside_songs #make scaling measure
    #Make into a tibble
    to_bind <- tibble(album=album_to_analyze,
                      word=word_of_interest,
                      prop_outside=prop_outside_to_paste,
                      outside_scaling=outside_scaling_to_paste)
    #Bind onto a big tibble which will have each album-word pair and the corresponding prop_outside column
    outside_values <- outside_values %>% rbind(to_bind)
  }
}
outside_values <- outside_values %>% mutate(scaled_prop_outside=prop_outside*outside_scaling)

Relative_Importance <- RTJ_lyrics %>% 
  select(album,word,word_clean) %>% 
  left_join(outside_values,by=c("album","word"))

#Making album back into an ordered factor
Relative_Importance$album <- factor(Relative_Importance$album,
                                    levels=c("Run the Jewels",
                                             "RTJ 2",
                                             "RTJ 3",
                                             "RTJ 4"),
                                    labels=c("Run the Jewels",
                                             "RTJ 2",
                                             "RTJ 3",
                                             "RTJ 4"))

Relative_Importance <- Relative_Importance %>% 
  #Getting a song column back
  cbind(RTJ_lyrics$song) %>% 
  as_tibble() %>% 
  rename("song"=`RTJ_lyrics$song`) %>% 
  #Getting percent_inside
  group_by(album,word) %>% 
  mutate(n=n()) %>% 
  ungroup() %>% 
  group_by(album) %>% 
  mutate(total_words=n()) %>% 
  ungroup() %>%
  mutate(prop_inside=(n/total_words)) %>% 
  #Getting proportion of songs a given word is in
  group_by(album,word) %>% 
  mutate(song_appearances=length(unique(song))) %>% 
  ungroup() %>% 
  group_by(album) %>% 
  mutate(total_songs=length(unique(song))) %>% 
  ungroup() %>% 
  mutate(prop_songs_in=(song_appearances/total_songs)) %>% 
  #Multipling proportion of words in an album made up by word x by the proportion of songs word x is in
  mutate(scaled_prop_inside=prop_inside*prop_songs_in) 

test <- Relative_Importance %>% mutate(difference=scaled_prop_inside-scaled_prop_outside)
