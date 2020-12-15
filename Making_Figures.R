library(tidyverse)
library(readr)
library(tidytext)
library(ggimage)
library(stringr)

windowsFonts(`Roboto Condensed`=windowsFont("Roboto Condensed"))
RTJ_lyrics <- read_rds("Data/RTJ_lyrics.rds")
data("stop_words")
RTJ_lyrics <- RTJ_lyrics %>% anti_join(stop_words)


# tf_idf ------------------------------------------------------------------
#Getting tf-idf
tf_idf <- RTJ_lyrics %>% count(album,word_clean) %>% bind_tf_idf(term = word_clean,document = album,n = n)

#Make basic plot
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


#Smoothing the graph. To plot the bars using album covers, I need a sequence of tf-idf values leading up
#to the actual value for each word album pair, so I can place an album cover each interval up to 
#the actual value using ggimage.
top_10_tf_idf <- tf_idf %>% 
  group_by(album) %>% 
  top_n(10,tf_idf) %>% 
  ungroup() %>% 
  mutate(word_clean=str_to_title(word_clean))

#Plot with album cover for bars
smooth_top_10_tf_idf <- top_10_tf_idf %>% head(0) %>% mutate(difference_smooth=double(0))

#For every word album pair for the top ten words by tf-idf for each album,
#I get a sequence of values up to the actual tf-idf value
for (i in 1:nrow(top_10_tf_idf)) {
  print(i)
  for (z in seq(0,top_10_tf_idf$tf_idf[i],by=.001)) {
    to_bind <- top_10_tf_idf[i,] %>% mutate(tf_idf_smooth=z)
    smooth_top_10_tf_idf <- smooth_top_10_tf_idf %>% rbind(to_bind)
  }
}

#Making a tibble of links for each album
album_covers <- tibble(album=c("Run the Jewels", "RTJ 2", "RTJ 3", "RTJ 4"),
                       album_cover=c("Data/RTJ1_Album_Cover.PNG",
                                     "Data/RTJ2_Album_Cover.PNG",
                                     "Data/RTJ3_Album_Cover.PNG",
                                     "Data/RTJ4_Album_Cover.PNG"))

#Joining album links so each album cover is paired with each word
smooth_top_10_tf_idf <- left_join(smooth_top_10_tf_idf,album_covers)

#Making album back into an ordered factor
smooth_top_10_tf_idf$album <- factor(smooth_top_10_tf_idf$album,
                                    levels=c("Run the Jewels",
                                             "RTJ 2",
                                             "RTJ 3",
                                             "RTJ 4"),
                                    labels=c("Run the Jewels",
                                             "RTJ 2",
                                             "RTJ 3",
                                             "RTJ 4"))

#Plotting with album covers for bar
smooth_top_10_tf_idf %>%
  dplyr::filter(!(word_clean %in% 
                   c("3", "anchors","begins","cats", "choppy","cowering", "molly", "shawty"))) %>% 
  mutate(word_clean=reorder_within(x=word_clean,by = tf_idf,within = album)) %>% 
  ggplot(aes(x=word_clean,y=tf_idf_smooth,fill=album)) +
  geom_image(aes(image=album_cover),asp = 2, size = .035) +
  facet_wrap(~album,scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  labs(y="Relative Importance",
       caption = "Plot: @jakepscott2020 | Data: Spotify and Genius",
       title=expression(paste("Which words does ", italic("tf-idf"), " identify as uniquely important?", 
                              sep = " ")),
       subtitle = "Using term frequency-inverse document frequency") +
  theme_minimal(base_family = "Roboto Condensed", base_size = 12) +
  theme(plot.title.position = "plot",
        plot.title = element_text(face="bold", size = rel(1.5), color="white"),
        plot.subtitle = element_text(size=rel(1),colour = "grey70"),
        plot.caption = element_text(face = "italic", size = rel(0.8), 
                                    color = "grey70"),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(color="white",size = rel(.7)),
        axis.text.y = element_text(color="white",size = rel(1)), 
        panel.grid = element_blank(),
        strip.text = element_text(face="bold",colour = "white",size=rel(1)),
        plot.background = element_rect(fill="grey20"))

#Saving plot
#ggsave("Figures/tfidf.png", dpi=600)



# Proportional Importance ----------------------------------------------------------------
#Loading data
Relative_Importance <- read_rds("Data/Relative_Importance.rds")
#Removing duplicate rows
Relative_Importance_Clean <- Relative_Importance %>% distinct(album,word,.keep_all = T)

#Making a basic plot
Relative_Importance_Clean %>% 
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


# Graphing with Album Covers ----------------------------------------------
#Making a tibble of links for each album
album_covers <- tibble(album=c("Run the Jewels", "RTJ 2", "RTJ 3", "RTJ 4"),
                       album_cover=c("Data/RTJ1_Album_Cover.PNG",
                                     "Data/RTJ2_Album_Cover.PNG",
                                     "Data/RTJ3_Album_Cover.PNG",
                                     "Data/RTJ4_Album_Cover.PNG"))
#Joining album links so each album cover is paired with each word
Relative_Importance_Clean <- left_join(Relative_Importance_Clean,album_covers)

#Making album back into an ordered factor
Relative_Importance_Clean$album <- factor(Relative_Importance_Clean$album,
                     levels=c("Run the Jewels",
                              "RTJ 2",
                              "RTJ 3",
                              "RTJ 4"),
                     labels=c("Run the Jewels",
                              "RTJ 2",
                              "RTJ 3",
                              "RTJ 4"))


#Smoothing the graph. To plot the bars using album covers, I need a sequence of prop-importance values 
#leading up to the actual value for each word album pair, so I can place an album cover each interval up to 
#the actual value using ggimage.

top_10 <- Relative_Importance_Clean %>% 
  group_by(album) %>% 
  top_n(10,difference) %>% 
  ungroup() %>% 
  mutate(word_clean=str_to_title(word_clean))

smooth_top_10 <- top_10 %>% head(0) %>% mutate(difference_smooth=double(0))

#For every word album pair for the top ten words by prop-importance for each album,
#I get a sequence of values up to the actual tf-idf value
for (i in 1:nrow(top_10)) {
  print(i)
  for (z in seq(0,top_10$difference[i],by=.1)) {
    to_bind <- top_10[i,] %>% mutate(difference_smooth=z)
    smooth_top_10 <- smooth_top_10 %>% rbind(to_bind)
  }
}

#graphing with album covers for bars
smooth_top_10 %>%
  mutate(word_clean=reorder_within(x=word_clean,by = difference,within = album)) %>% 
  ggplot(aes(x=word_clean,y=difference_smooth,fill=album)) +
  geom_image(aes(image=album_cover),asp = 2, size = .04,nudge_y=-.025, nudge_x = .001) +
  facet_wrap(~album,scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  labs(y="Relative Importance",
       caption = "Plot: @jakepscott2020 | Data: Spotify and Genius",
       title="Which words does proportional importance \nidentify as uniquely important?",
       subtitle = "Horizontal axis measured in percentage point terms, multiplying proportion \nby 100") +
  theme_minimal(base_family = "Roboto Condensed", base_size = 12) +
  theme(plot.title.position = "plot",
        plot.title = element_text(face="bold", size = rel(1.5), color="white"),
        plot.subtitle = element_text(size=rel(1),colour = "grey70"),
        plot.caption = element_text(face = "italic", size = rel(0.8), 
                                    color = "grey70"),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(color="white",size = rel(.7)),
        axis.text.y = element_text(color="white",size = rel(1)), 
        panel.grid = element_blank(),
        strip.text = element_text(face="bold",colour = "white",size=rel(1)),
        plot.background = element_rect(fill="grey20"))

#Saving plot
#ggsave("Figures/PI.png", dpi=600)
