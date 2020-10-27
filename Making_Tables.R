library(gtable)
library(tidyverse)

# Setting up data ---------------------------------------------------------
table <- diffs_mine %>% 
  arrange(word_clean,desc(n)) %>% 
  left_join(Relative_Importance_Clean) %>% 
  select(album,n,difference,percent_inside,tf_idf,word_clean) %>% 
  mutate(album=case_when(album=="RTJ 2"~"Run the Jewels 2",
                         album=="RTJ 3"~"Run the Jewels 3",
                         album=="RTJ 4"~"Run the Jewels 4",
                         album=="Run the Jewels"~"Run the Jewels")) %>% 
  mutate(percent_inside=round(percent_inside,3),
         difference=round(difference,3),
         tf_idf=round(tf_idf,3))


# Kill --------------------------------------------------------------------
table %>% 
  filter(word_clean=="kill") %>% 
  select(-word_clean) %>% 
  select("Album"=album,
         "Appearances"=n,
         #"Term Frequency"=percent_inside,
         difference,
         "tf-idf"=tf_idf) %>%
  #Initializing table
  gt() %>% 
  #Setting title
  tab_header(title = md("Use of the word **kill**")) %>% 
  tab_options(
    table.border.top.color = "white",
    column_labels.border.top.color = "white",
    column_labels.border.top.width = px(3),
    column_labels.border.bottom.color = "black",
    table_body.hlines.color = "white",
    table.border.bottom.color = "white",
    table.border.bottom.width = px(3),
    heading.align = "center"
  ) %>% 
  #opt_align_table_header(align = "left") %>%  
  cols_align(align = "right",
             columns = 2:4) %>% 
  cols_label(difference="Proportional Importance") %>% 
  data_color(
    columns = vars(Appearances),
    colors = scales::col_numeric(
      palette = c("white", "#3fc1c9"),
      domain = NULL
    ))


# run ---------------------------------------------------------------------

table %>% 
  filter(word_clean=="run") %>% 
  select(-word_clean) %>% 
  select("Album"=album,
         "Appearances"=n,
         #"Term Frequency"=percent_inside,
         difference,
         "tf-idf"=tf_idf) %>%
  #Initializing table
  gt() %>% 
  #Setting title
  tab_header(title = md("Use of the word **run**")) %>% 
  tab_options(
    table.border.top.color = "white",
    column_labels.border.top.color = "white",
    column_labels.border.top.width = px(3),
    column_labels.border.bottom.color = "black",
    table_body.hlines.color = "white",
    table.border.bottom.color = "white",
    table.border.bottom.width = px(3),
    heading.align = "center"
  ) %>% 
  #opt_align_table_header(align = "left") %>%  
  cols_align(align = "right",
             columns = 2:4) %>% 
  cols_label(difference="Proportional Importance") %>% 
  data_color(
    columns = vars(Appearances),
    colors = scales::col_numeric(
      palette = c("white", "#3fc1c9"),
      domain = NULL
    ))

# hey ---------------------------------------------------------------------

table %>% 
  filter(word_clean=="hey") %>% 
  select(-word_clean) %>% 
  select("Album"=album,
         "Appearances"=n,
         #"Term Frequency"=percent_inside,
         difference,
         "tf-idf"=tf_idf) %>%
  #Initializing table
  gt() %>% 
  #Setting title
  tab_header(title = md("Use of the word **hey**")) %>% 
  tab_options(
    table.border.top.color = "white",
    column_labels.border.top.color = "white",
    column_labels.border.top.width = px(3),
    column_labels.border.bottom.color = "black",
    table_body.hlines.color = "white",
    table.border.bottom.color = "white",
    table.border.bottom.width = px(3),
    heading.align = "center"
  ) %>% 
  #opt_align_table_header(align = "left") %>%  
  cols_align(align = "right",
             columns = 2:4) %>% 
  cols_label(difference="Proportional Importance") %>% 
  data_color(
    columns = vars(Appearances),
    colors = scales::col_numeric(
      palette = c("white", "#3fc1c9"),
      domain = NULL
    ))

# Slave ---------------------------------------------------------------------

table %>% 
  filter(word_clean=="slave") %>% 
  select(-word_clean) %>% 
  select("Album"=album,
         "Appearances"=n,
         #"Term Frequency"=percent_inside,
         difference,
         "tf-idf"=tf_idf) %>%
  #Initializing table
  gt() %>% 
  #Setting title
  tab_header(title = md("Use of the word **slave**")) %>% 
  tab_options(
    table.border.top.color = "white",
    column_labels.border.top.color = "white",
    column_labels.border.top.width = px(3),
    column_labels.border.bottom.color = "black",
    table_body.hlines.color = "white",
    table.border.bottom.color = "white",
    table.border.bottom.width = px(3),
    heading.align = "center"
  ) %>% 
  #opt_align_table_header(align = "left") %>%  
  cols_align(align = "right",
             columns = 2:4) %>% 
  cols_label(difference="Proportional Importance") %>% 
  data_color(
    columns = vars(Appearances),
    colors = scales::col_numeric(
      palette = c("white", "#3fc1c9"),
      domain = NULL
    ))
