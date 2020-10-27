library(tidyverse)
library(readr)
library(tidytext)
library(ggimage)

windowsFonts(`Roboto Condensed`=windowsFont("Roboto Condensed"))
RTJ_lyrics <- read_rds("Data/RTJ_lyrics.rds")
data("stop_words")
RTJ_lyrics <- RTJ_lyrics %>% anti_join(stop_words) %>% 
  filter(!(word %in% c("oui","ayy","ooh","uh","ah","la", "ol", "li","er","ay")))


# tf_idf ------------------------------------------------------------------
tf_idf <- RTJ_lyrics %>% count(album,word_clean) %>% bind_tf_idf(term = word_clean,document = album,n = n)

#Basic Plot
tf_idf %>% 
  group_by(album) %>% 
  top_n(10, tf_idf) %>% 
  mutate(word_clean=reorder_within(x=word_clean,by = tf_idf,within = album)) %>% 
  ggplot(aes(x=word_clean,y=tf_idf,fill=album)) +
  geom_col() +
  facet_wrap(~album,scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  scale_y_continuous(expand=c(0,0))
