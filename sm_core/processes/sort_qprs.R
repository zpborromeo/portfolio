
library(tidyverse)
library(pdftools)



qprs <- fs::dir_ls('Triumphant Q3') %>% 
  as_tibble() %>% 
  rowwise() %>% 
  mutate(sort_var = str_extract(value, "^[^-]*[^ -]"),
         sort_last = last(str_split(sort_var, " ")[[1]]),
         sort_first = str_remove(sort_var, "Triumphant Q3/"),
         sort_first = str_remove_all(sort_first, coll("Mr.")),
         sort_first = str_remove_all(sort_first, coll("Dr.")),
         sort_first = str_remove_all(sort_first, coll("Mrs.")),
         sort_first = str_remove_all(sort_first, coll("&")),
         sort_first = str_trim(sort_first),
         sort_first = str_squish(sort_first),
         sort_first = first(str_split(sort_first, " ")[[1]])) %>% 
  arrange(sort_last, sort_first) %>% 
  pull(value) %>% 
  pdftools::pdf_combine(output = 'tpm_combined_qprs.pdf')
