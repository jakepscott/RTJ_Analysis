
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


diffs_mine %>% arrange(word_clean,desc(n)) %>% left_join(Relative_Importance) %>% 
  select(word_clean,n,percent_inside,percent_outside,difference,tf,idf,tf_idf)



#Mine may be better when there is less of a clear distinction, in which case idf is a bad measure,
#Moves things down too much

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

