library(tidyverse)
library(readr)
library(tidytext)
library(ggimage)

windowsFonts(`Roboto Condensed`=windowsFont("Roboto Condensed"))
RTJ_lyrics <- read_rds("Data/RTJ_lyrics.rds")
data("stop_words")
RTJ_lyrics <- RTJ_lyrics %>% anti_join(stop_words)

# Getting Outside Proportion ----------------------------------------------
#First I make an empty tibble which will eventually contain the album, word, and proportion
#of all words made up by that word outside the given album. So if the album is RTJ2 and the word
#is run, the percent_outside column will be the proportion of words outside RTJ2 that are "run"
outside_values <- tibble(album=character(0),word=character(0),percent_outside=double(0))
tictoc::tic()
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
#Join the outside_values tibble to the RTJ_lyrics tibble, so for each word with will now have the value
#for the proportion of words outside the given album are made up by that given word. So if the album is
#RTJ4 and the word is week, the percent_outside column, which is .029, means that week makes up .029% of words in 
#RTJ1,RTJ2,and RTJ3
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
saveRDS(Relative_Importance,"Data/Relative_Importance.rds")