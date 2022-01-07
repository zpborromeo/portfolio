source('utils.R')

tagsList <- httr::content(httr::GET(str_c(bd_api$call_url, '/v1/tag/account'), auth_header, body = "", encode='form'))

unnested_tags <- tagsList %>% 
  enframe() %>% 
  select(value) %>% 
  unnest_wider(value)

update_tags_list <- readxl::read_excel("Excel_Files/update_account_tags.xlsx") %>%
  clean_names() %>% 
  mutate(account_number = as.double(account_number))

batch_date <- get_latest_settlement_date(lubridate::today() - 1)

bd_accounts <- load_black_diamond_accounts(batch_date)

accounts_to_update <- bd_accounts %>%
  mutate(AccountNumber = as.double(AccountNumber)) %>%
  left_join(update_tags_list, by=c("AccountNumber" = "account_number")) %>% 
  select(Id, AccountNumber, aum_type) %>% 
  filter(!is.na(aum_type)) %>% 
  filter(!is.na(AccountNumber))

save_document <- write.csv(accounts_to_update, "New_Accounts_List/accounts_to_update-aumtype.csv")


bd_response <- accounts_to_update %>% 
  pmap(function(...) {
    
    current <- tibble(...)
    
    account_body <- glue::glue('[
        {
          "name": "AUM Type",
          "value": "<<current$aum_type>>",
        }]', .open = "<<", .close = ">>")
    
    account_response1 <-  httr::PUT(str_c(bd_api$call_url, '/v1/account/', current$Id, '/tag'), auth_header, body = account_body, encode='form')
    
  })