source('utils.R')

tagsList <- httr::content(httr::GET(str_c(bd_api$call_url, '/v1/tag/account'), auth_header, body = "", encode='form'))

unnested_tags <- tagsList %>% 
  enframe() %>% 
  select(value) %>% 
  unnest_wider(value)

update_tags_list <- readxl::read_excel("update_account_tags.xlsx") %>%
  clean_names()

batch_date <- get_latest_settlement_date(lubridate::today())

bd_accounts <- load_black_diamond_accounts(batch_date)

accounts_to_update <- bd_accounts %>%
  mutate(AccountNumber = as.numeric(AccountNumber)) %>%
  left_join(update_tags_list, by=c("AccountNumber" = "account_number")) %>% 
  select(Id, AccountNumber, advisor_name, payout_name) %>% 
  filter(!is.na(advisor_name))

bd_response <- accounts_to_update %>% 
  pmap(function(...) {
    
    current <- tibble(...)
    
    account_body <- glue::glue('[
        {
          "Advisor Name": "<<current$advisor_name>>",
          "Payout Name": "<<current$payout_name>>"
        }]', .open = "<<", .close = ">>")
    
    account_response1 <-  httr::PUT(str_c(bd_api$call_url, '/v1/account/', current$Id, '/tag'), auth_header, body = account_body, encode='form')
    
  })