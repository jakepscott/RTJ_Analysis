# Loading Libraries and data ------------------------------------------------------------
library(tidyverse)
library(tidytext)

RTJ_lyrics <- read_rds("Data/RTJ_lyrics.rds")


# Raw Counts (With Stopwords) ---------------------------------------------

RTJ_lyrics %>% 
  count(album,word) %>% 
  arrange(desc(n)) %>% 
  group_by(album) %>% top_n(10) %>% 
  mutate(word=reorder_within(x = word,by = n,within = album)) %>% 
  ggplot(aes(x=word,y=n)) +
  geom_col(aes(fill=album)) +
  facet_wrap(~album,scales = "free") +
  coord_flip() +
  scale_x_reordered()

#Not super useful, let's remove stop words


# Word Counts Without Stopwords -------------------------------------------
data("stop_words")
RTJ_lyrics <- RTJ_lyrics %>% anti_join(stop_words)

RTJ_lyrics %>% 
  count(album,word) %>% 
  arrange(desc(n)) %>% 
  group_by(album) %>% top_n(10) %>% 
  mutate(word=reorder_within(x = word,by = n,within = album)) %>% 
  ggplot(aes(x=word,y=n)) +
  geom_col(aes(fill=album)) +
  facet_wrap(~album,scales = "free") +
  coord_flip() +
  scale_x_reordered() +
  theme(legend.position = "none",
        axis.title = element_blank())



