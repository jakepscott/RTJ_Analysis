library(tidyverse)
library(readr)
library(tidytext)
library(ggimage)

windowsFonts(`Roboto Condensed`=windowsFont("Roboto Condensed"))
RTJ_lyrics <- read_rds("Data/RTJ_lyrics.rds")
data("stop_words")
RTJ_lyrics <- RTJ_lyrics %>% anti_join(stop_words)


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


#Smoothing the graph
top_10_tf_idf <- tf_idf %>% 
  group_by(album) %>% 
  top_n(10,tf_idf) %>% 
  ungroup()

#Plot with album cover for bars
smooth_top_10_tf_idf <- top_10_tf_idf %>% head(0) %>% mutate(difference_smooth=double(0))

for (i in 1:nrow(top_10_tf_idf)) {
  print(i)
  for (z in seq(0,top_10_tf_idf$tf_idf[i],by=.001)) {
    to_bind <- top_10_tf_idf[i,] %>% mutate(tf_idf_smooth=z)
    smooth_top_10_tf_idf <- smooth_top_10_tf_idf %>% rbind(to_bind)
  }
}

album_covers <- tibble(album=c("Run the Jewels", "RTJ 2", "RTJ 3", "RTJ 4"),
                       album_cover=c("Data/RTJ1_Album_Cover.PNG",
                                     "Data/RTJ2_Album_Cover.PNG",
                                     "Data/RTJ3_Album_Cover.PNG",
                                     "Data/RTJ4_Album_Cover.PNG"))

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

smooth_top_10_tf_idf %>%
  mutate(word_clean=reorder_within(x=word_clean,by = tf_idf,within = album)) %>% 
  ggplot(aes(x=word_clean,y=tf_idf_smooth,fill=album)) +
  geom_image(aes(image=album_cover),asp = 2, size = .035) +
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

# Graphing ----------------------------------------------------------------
Relative_Importance <- read_rds("Data/Relative_Importance.rds")
Relative_Importance_Clean <- Relative_Importance %>% distinct(album,word,.keep_all = T)


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

album_covers <- tibble(album=c("Run the Jewels", "RTJ 2", "RTJ 3", "RTJ 4"),
                       album_cover=c("Data/RTJ1_Album_Cover.PNG",
                                     "Data/RTJ2_Album_Cover.PNG",
                                     "Data/RTJ3_Album_Cover.PNG",
                                     "Data/RTJ4_Album_Cover.PNG"))

Relative_Importance_Clean <- left_join(Relative_Importance_Clean,album_covers)
Relative_Importance_Clean$album <- factor(Relative_Importance_Clean$album,
                     levels=c("Run the Jewels",
                              "RTJ 2",
                              "RTJ 3",
                              "RTJ 4"),
                     labels=c("Run the Jewels",
                              "RTJ 2",
                              "RTJ 3",
                              "RTJ 4"))




#Smoothing the graph
top_10 <- Relative_Importance_Clean %>% 
  group_by(album) %>% 
  top_n(10,difference) %>% 
  ungroup()

smooth_top_10 <- top_10 %>% head(0) %>% mutate(difference_smooth=double(0))

for (i in 1:nrow(top_10)) {
  print(i)
  for (z in seq(0,top_10$difference[i],by=.1)) {
    to_bind <- top_10[i,] %>% mutate(difference_smooth=z)
    smooth_top_10 <- smooth_top_10 %>% rbind(to_bind)
  }
}


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

  