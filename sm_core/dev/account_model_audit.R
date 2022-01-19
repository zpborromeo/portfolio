source('utils.R')
library(lubridate)

bd_accounts <- readxl::read_excel("Excel_Files/bd_accounts.xlsx") %>% 
  clean_names()

sf_accounts <- readxl::read_excel("Excel_Files/sf_accounts.xlsx") %>% 
  clean_names()

account_merge <- bd_accounts %>% 
  left_join(sf_accounts, by="account_number") %>% 
  filter(!is.na(financial_account_financial_account_name)) %>% 
  mutate(style_match = if_else(style == model, TRUE, FALSE)) %>% 
  filter(style_match == FALSE || is.na(style_match)) %>% 
  select(account_number, as_of_date, financial_account_financial_account_name, style, model, style_match) %>% 
  filter(as_of_date == "01/18/2022") %>% 
  rename("bd_style" = style) %>% 
  rename("sf_model" = model)

matched_styes <- account_merge %>% 
  filter(style_match == TRUE)

unmatched_styles <- account_merge %>% 
  filter(style_match != TRUE)

na_styles <- account_merge %>% 
  filter(is.na(style_match))

save_document <- write.csv(matched_styes, "C:/sm-core/sm_core/Excel_Files/matched_styles.csv")
save_document <- write.csv(unmatched_styles, "C:/sm-core/sm_core/Excel_Files/unmatched_styles.csv")
save_document <- write.csv(na_styles, "C:/sm-core/sm_core/Excel_Files/na_styles.csv")
