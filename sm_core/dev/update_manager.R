source('utils.R')

update_manager <- readxl::read_excel("Excel_Files/update_manager.xlsx") %>% 
  clean_names()

batch_date <- get_latest_settlement_date(lubridate::today())

bd_accounts <- load_black_diamond_accounts(batch_date)

bd_accts <- bd_accounts %>% 
  clean_names() %>% 
  select(account_number, id) %>% 
  right_join(update_manager, by=c("account_number" = "account_number"))


managers <- httr::content(httr::GET(str_c(bd_api$call_url, '/v1/managers'), auth_header, body = '', encode='form')) %>% 
  enframe() %>% 
  select(value) %>% 
  unnest_wider(value)

bd_response <- bd_accts %>% 
  pmap(function(...) {
    
    current <- tibble(...)
    
    account_body <- glue::glue('[
          {
              "id": "z05503641",
              "firstName": ",
              "lastName": "Affinity 10"
          }]', .open = "<<", .close = ">>")
    
    account_response <-  httr::PUT(str_c(bd_api$call_url, '/v1/account/', current$id, '/manager'), auth_header, body = account_body, encode='form')
    
  })
