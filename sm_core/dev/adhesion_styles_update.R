source('utils.R')

stylesData <- readxl::read_excel("style_update.xlsx")

emptyStyles <- readxl::read_excel("empty_styles.xlsx") %>% 
  clean_names()

merged_adhesion_accts <- emptyStyles %>% 
  mutate(account_number = as.numeric(account_number)) %>% 
  left_join(stylesData, by=c("account_number" = "account_number")) %>% 
  filter(!is.na(style_name)) %>% 
  select(portfolio_id, portfolio_name, account_id, account_number, account_name, style, manager, style_name)

batch_date <- get_latest_settlement_date(lubridate::today())

bd_accounts <- load_black_diamond_accounts(batch_date)

accounts_to_update <- bd_accounts %>% 
  mutate(AccountNumber = as.numeric(AccountNumber)) %>% 
  left_join(merged_adhesion_accts, by=c("AccountNumber" = "account_number")) %>% 
  select(Id, AccountNumber, style_name) %>% 
  filter(!is.na(style_name))


bd_response <- accounts_to_update %>% 
  pmap(function(...) {
    
    current <- tibble(...)
    
    account_body <- glue::glue('{
          "name": "<<current$style_name>>",
          "id": "<<current$Id>>",
          "assignedBenchmarkName": "",
          "isSMAClassified": false,
          "smaClassification": ""
        }', .open = "<<", .close = ">>")
    
    httr::PUT(str_c(bd_api$call_url, '/v1/account/', current$Id, '/style'), auth_header, body = account_body, encode='form')
    
  })