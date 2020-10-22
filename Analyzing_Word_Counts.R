# Loading Libraries and data ------------------------------------------------------------
library(tidyverse)
library(tidytext)
library(ggrepel)

RTJ_lyrics <- read_rds("Data/RTJ_lyrics.rds")


# Raw Counts (With Stopwords) ---------------------------------------------
#Number of words by album
RTJ_lyrics %>% 
  count(album) %>% 
  ggplot(aes(x=fct_reorder(album,n),y=n,fill=album))+
  geom_col() +
  geom_text(aes(label=n),nudge_y=-400) +
  scale_y_continuous(expand = c(0,0)) +
  coord_flip() +
  theme(legend.position = "none",
        axis.title = element_blank())

#Looking at words per song
RTJ_lyrics %>% 
  group_by(song) %>% 
  summarise(n=n(),album=album,track_number=track_number) %>% 
  distinct() %>% 
  mutate(song2=song) %>% 
  #Removing any parentheses, ususally used for features
  separate(col = song2, into = c("song2", "extra"), sep = " [(]") %>%
  select(-extra) %>% 
  #Removing hyphens (both with and without spaces in front), they usually come before a "live from XYZ"
  separate(song2, into = c("song2", "extra"), sep = " -") %>%
  select(-extra) %>% 
  separate(song2, into = c("song2", "extra"), sep = "-") %>% 
  select(-extra) %>%
  #Removing punctuation
  mutate(song2=str_remove_all(string = song2, pattern = "[[:punct:]]")) %>% 
  ggplot(aes(x=fct_reorder(song2,n),y=n)) +
  geom_col(aes(fill=album),color="black") +
  scale_y_continuous(expand=c(0,0)) +
  coord_flip()

#Most common words by album
RTJ_lyrics %>% 
  count(album,word_clean) %>% 
  arrange(desc(n)) %>% 
  group_by(album) %>% top_n(10) %>% 
  mutate(word_clean=reorder_within(x = word_clean,by = n,within = album)) %>% 
  ggplot(aes(x=word_clean,y=n)) +
  geom_col(aes(fill=album)) +
  facet_wrap(~album,scales = "free") +
  coord_flip() +
  scale_x_reordered()


# Word Counts Without Stopwords -------------------------------------------
data("stop_words")
RTJ_lyrics <- RTJ_lyrics %>% anti_join(stop_words)

#Number of words by album
RTJ_lyrics %>% 
  count(album) %>% 
  ggplot(aes(x=fct_reorder(album,n),y=n,fill=album))+
  geom_col() +
  geom_text(aes(label=n,y=n-150)) +
  scale_y_continuous(expand = c(0,0)) +
  coord_flip() +
  theme(legend.position = "none",
        axis.title = element_blank())

#Looking at words per song
RTJ_lyrics %>% 
  group_by(song) %>% 
  summarise(n=n(),album=album,track_number=track_number) %>% 
  distinct() %>% 
  mutate(song2=song) %>% 
  #Removing any parentheses, ususally used for features
  separate(col = song2, into = c("song2", "extra"), sep = " [(]") %>%
  select(-extra) %>% 
  #Removing hyphens (both with and without spaces in front), they usually come before a "live from XYZ"
  separate(song2, into = c("song2", "extra"), sep = " -") %>%
  select(-extra) %>% 
  separate(song2, into = c("song2", "extra"), sep = "-") %>% 
  select(-extra) %>%
  #Removing punctuation
  mutate(song2=str_remove_all(string = song2, pattern = "[[:punct:]]")) %>% 
  ggplot(aes(x=fct_reorder(song2,n),y=n)) +
  geom_col(aes(fill=album),color="black") +
  scale_y_continuous(expand=c(0,0)) +
  coord_flip()

#Most common words by album
RTJ_lyrics %>% 
  count(album,word_clean) %>% 
  arrange(desc(n)) %>% 
  group_by(album) %>% 
  top_n(10) %>% 
  mutate(word_clean=reorder_within(x = word_clean,by = n,within = album)) %>% 
  ggplot(aes(x=word_clean,y=n)) +
  geom_col(aes(fill=album)) +
  facet_wrap(~album,scales = "free") +
  coord_flip() +
  scale_x_reordered() +
  theme(legend.position = "none",
        axis.title = element_blank())

#Most common words by album adjusted for number of words in album
RTJ_lyrics %>% 
  group_by(album) %>% 
  mutate(total_words=n()) %>% 
  ungroup() %>% 
  group_by(album,word) %>% 
  mutate(n=n()) %>% 
  ungroup() %>% 
  mutate(proportion=n/total_words*100) %>% 
  arrange(desc(proportion)) %>% 
  select(album,word_clean,proportion) %>% 
  distinct() %>% 
  group_by(album) %>% 
  top_n(10) %>% 
  mutate(word_clean=reorder_within(x = word_clean,by = proportion,within = album)) %>% 
  ggplot(aes(x=word_clean,y=proportion)) +
  geom_col(aes(fill=album)) +
  facet_wrap(~album,scales = "free") +
  coord_flip() +
  scale_x_reordered() +
  labs(y="Proportion of Total Words") +
  theme(legend.position = "none",
        axis.title.y = element_blank())



