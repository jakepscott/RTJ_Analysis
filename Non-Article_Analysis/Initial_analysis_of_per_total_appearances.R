RTJ_lyrics %>% filter(!(word=="slave" & album!="RTJ 4")) %>%
  count(album,word_clean) %>% 
  bind_tf_idf(term = word_clean,document = album,n = n) %>% 
  filter(word_clean=="slave")


RTJ_lyrics %>% 
  count(album,word_clean) %>% 
  bind_tf_idf(term = word_clean,document = album,n = n) %>% 
  filter(word_clean=="slave" |
           word_clean=="la") %>% 
  arrange(word_clean) %>% 
  group_by(word_clean) %>% 
  mutate(test=n/sum(n))
