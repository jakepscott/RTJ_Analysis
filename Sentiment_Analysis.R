# Loading Libraries and data-------------------------------------------------------
library(tidyverse)
library(tidytext)

RTJ_lyrics <- read_rds("Data/RTJ_lyrics.rds")
windowsFonts(`Roboto Condensed`=windowsFont("Roboto Condensed"))


# Getting and Joining Sentiment -------------------------------------------
#Getting sentiment
afinn <- get_sentiments("afinn")

#Joining
RTJ_sentiment <- RTJ_lyrics %>% inner_join(afinn) %>% rename("sentiment"=value)


# Raw Sentiment by Album --------------------------------------------------
RTJ_sentiment %>% 
  count(sentiment) %>% 
  ggplot() +
  geom_col(aes(sentiment,n)) +
  geom_vline(xintercept = mean(RTJ_sentiment$sentiment))
  scale_x_continuous(breaks = seq(-5,5,1))

  RTJ_sentiment %>% 
    group_by(album) %>% 
    mutate(avg_sentiment=mean(sentiment)) %>% 
    ungroup() %>% 
    group_by()
    ggplot() +
    geom_col(aes(sentiment,n)) +
    geom_vline(aes(xintercept = avg_sentiment)) +
    facet_wrap(~album) +
    scale_x_continuous(breaks = seq(-5,5,1))
  