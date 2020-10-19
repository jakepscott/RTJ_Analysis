
# Getting Top words using tf_idf and my measure ---------------------------

#tf_idf
top_10_tf_idf <- tf_idf %>% 
  group_by(album) %>%
  top_n(10, tf_idf) %>%
  ungroup()

#My measure
top_10_RI <- top_10


# What others words does mine grab but tf_idf does not and why --------------------
top_10_RI %>% anti_join(top_10_tf_idf)

diffs <- tf_idf %>% head(0)
for (i in (top_10_RI %>% anti_join(top_10_tf_idf) %>% pull(word_clean))) {
  to_bind <- tf_idf %>% filter(word_clean==i)
  diffs <- diffs %>% rbind(to_bind)
}

diffs %>% arrange(desc(n)) %>% group_by(word_clean) %>% top_n(1,n) %>% left_join(top_10_RI)
diffs %>% arrange(word_clean,desc(n)) %>% left_join(Relative_Importance) %>% 
  select(word_clean,n,percent_inside) %>% View()


#Mine may be better when there is less of a clear distinction, in which case idf is a bad measure,
#Moves things down too much

# What  words does tf_idf grab but  mine does not and why --------------------
top_10_tf_idf %>% anti_join(top_10_RI) %>% View()

diffs <- Relative_Importance %>% head(0)
for (i in (top_10_tf_idf %>% anti_join(top_10_RI) %>% pull(word_clean))) {
  to_bind <- Relative_Importance %>% filter(word_clean==i)
  diffs <- diffs %>% rbind(to_bind)
}

diffs %>% arrange(desc(n))


# Kill is clear example of why my measure is better -----------------------
#Kill is biggest for RTJ 3 with my measure and not even there with tf_idf
tf_idf %>% filter(word_clean=="kill")
#This is because kill appears in all 4 albums, so it gets an idf value of *0*
#I think this is a mistake. The word kill is clearly uniqeuly important to RTJ 3, with it appearing 
#over **6 times** more as it does in the next closest album

