#Getting top words for my Relative Importance Measure and tf_idf
top_10_tf_idf <- tf_idf %>% 
  group_by(album) %>%
  top_n(10, tf_idf) %>%
  ungroup()
top_10_RI <- top_10

#What words does my measure grab that tf_idf does not
top_10_RI %>% anti_join(top_10_tf_idf)

#What words does tf_idf capture that my measure does not
top_10_tf_idf %>% anti_join(top_10_RI) %>% View()


# Kill is clear example of why my measure is better -----------------------
#Kill is biggest for RTJ 3 with my measure and not even there with tf_idf
tf_idf %>% filter(word_clean=="kill")
#This is because kill appears in all 4 albums, so it gets an idf value of *0*
#I think this is a mistake. The word kill is clearly uniqeuly important to RTJ 3, with it appearing 
#over **6 times** more as it does in the next closest album



# What others words does mine grab but tf_idf does not and why --------------------
diff <- function(word){
  tf_idf %>% filter(word_clean==word)
}

diff("slave")
diff("dollar")
diff("y'all")
diff("rtj")
diff("live")
diff("kill")
diff("mama")
diff("run")
diff("win")
diff("d*ck")
diff("day")
diff("holding")
diff("feeling")
diff("mouth")
diff("pick")
diff("jewels")
diff("die")
diff("hey")
diff("close")


#Mine may be better when there is less of a clear distinction, in which case idf is a bad measure,
#Moves things down too much

