library(gtable)
table <- diffs_mine %>% arrange(word_clean,desc(n)) %>% left_join(Relative_Importance_Clean) %>% 
  select(album,n,difference,percent_inside,tf_idf,word_clean) %>% 
  filter(word_clean=="kill") %>% 
  select(-word_clean) %>% 
  mutate(album=case_when(album=="RTJ 2"~"Run the Jewels 2",
                         album=="RTJ 3"~"Run the Jewels 3",
                         album=="RTJ 4"~"Run the Jewels 4",
                         album=="Run the Jewels"~"Run the Jewels")) %>% 
  mutate(percent_inside=round(percent_inside,2),
         difference=round(difference,2))


table %>% 
  select("Album"=album,
         "Appearances"=n,
         "Term Frequency"=percent_inside,
         difference,
         "tf-idf"=tf_idf) %>%
  gt() %>% 
  tab_header(title = md("**Use of the word kill in Run the Jewels albums**")) %>% 
  #opt_align_table_header(align = "left") %>%  
  cols_align(align = "right",
             columns = 2:5) %>% 
  cols_label(difference="Proportional Importance") %>% 
  data_color(
    columns = vars(Appearances),
    colors = scales::col_numeric(
      palette = c("white", "#3fc1c9"),
      domain = NULL
    ))
