
# Getting Top words using tf_idf and my measure ---------------------------

#tf_idf
top_10_tf_idf <- tf_idf %>% 
  group_by(album) %>%
  top_n(10, tf_idf) %>%
  ungroup()

#My measure
top_10_RI <- top_10


# What words does mine grab but tf_idf does not and why --------------------
top_10_RI %>% anti_join(top_10_tf_idf)

#This filters the dataframe of important words from my measure to just hold the ones not captured by 
#tf_idf
diffs_mine <- tf_idf %>% head(0)
for (i in (top_10_RI %>% anti_join(top_10_tf_idf) %>% pull(word_clean))) {
  to_bind <- tf_idf %>% filter(word_clean==i)
  diffs_mine <- diffs_mine %>% rbind(to_bind)
}


# Which ones my measure picks up but tf-idf doesn't -----------------------
diffs_mine %>% arrange(word_clean,desc(n)) %>% left_join(Relative_Importance_Clean) %>% 
  select(album, word_clean,n,percent_inside,percent_outside,difference,tf,idf,tf_idf) %>% View()


# Zero Value Issue --------------------------------------------------------
#Notice 11 of these words appear in all 4 albums, meaning tf-idf will give them a value of zero
diffs_mine %>% arrange(word_clean,desc(n)) %>% left_join(Relative_Importance_Clean) %>% 
  select(album, word_clean,n,percent_inside,percent_outside,difference,tf,idf,tf_idf) %>% 
  count(word_clean) %>% arrange(desc(n))

#Look at "Making_Tables.R" to see the table for kill, run, and hey


# Extensive Margin Issue --------------------------------------------------
#***Slave***
#Slave gets a tf_idf of .00812 if the one instance of it in RTJ 2 and RTJ 3 are removed, whereas without 
#removing them the tf_idf is 0.00169

#With the 2 instances
RTJ_lyrics %>% 
  filter(!(word=="slave" & album!="RTJ 4")) %>% 
  count(album,word_clean) %>% 
  bind_tf_idf(term = word_clean,document = album,n = n) %>% 
  filter(word_clean=="slave")
#Without the two instances
RTJ_lyrics %>% 
  filter(!(word=="slave" & album!="RTJ 4")) %>% 
  count(album,word_clean) %>% 
  bind_tf_idf(term = word_clean,document = album,n = n) %>% 
  filter(word_clean=="slave")

#With 200 instances outside
RTJ_lyrics %>% 
  count(album,word_clean) %>% 
  mutate(n=ifelse(album=="RTJ 2" | album=="RTJ 3", 100, n)) %>% 
  bind_tf_idf(term = word_clean,document = album,n = n) %>% 
  filter(word_clean=="slave")

# a tf_idf of .008 would make it the **3rd most important word** in RTJ 4 if all we do is remove 2 
#instances of it
#outside of RTJ 4
top_10_tf_idf %>% filter(album=="RTJ 4") %>% arrange(desc(tf_idf))

#***Holding***
#Holding gets a tf_idf of .0105 if the one instance of it in RTJ 1 and RTJ 3 are removed, whereas without 
#removing them the tf_idf is 0.00218  
RTJ_lyrics %>% 
  filter(!(word=="holding" & album!="RTJ 2")) %>% 
  count(album,word_clean) %>% 
  bind_tf_idf(term = word_clean,document = album,n = n) %>% 
  filter(word_clean=="holding")
#a tf_idf of .0105 would make it it the ***most** important word in RTJ 2 if all we do
##is remove 2 instances of it outside of RTJ 2
top_10_tf_idf %>% filter(album=="RTJ 2") %>% arrange(desc(tf_idf))


# What  words does tf_idf grab but mine does not and why --------------------
top_10_tf_idf %>% anti_join(top_10_RI) %>% View()

diffs_tf <- Relative_Importance %>% head(0)
for (i in (top_10_tf_idf %>% anti_join(top_10_RI) %>% pull(word_clean))) {
  to_bind <- Relative_Importance %>% filter(word_clean==i)
  diffs_tf <- diffs %>% rbind(to_bind)
}

diffs_tf %>% arrange(desc(n))


# Kill is clear example of why my measure is better -----------------------
#Kill is biggest for RTJ 3 with my measure and not even there with tf_idf
tf_idf %>% filter(word_clean=="kill")
#This is because kill appears in all 4 albums, so it gets an idf value of *0*
#I think this is a mistake. The word kill is clearly uniqeuly important to RTJ 3, with it appearing 
#over **6 times** more as it does in the next closest album

