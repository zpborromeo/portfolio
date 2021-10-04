setwd("C:\\sm-core\\sm_core")

source('utils.R')

update_tags_list <- readxl::read_excel("test_tags.xlsx") %>% 
  clean_names()

bd_response <- update_tags_list %>% 
  pmap(function(...) {
    
    current <- tibble(...)
    
    account_body <- glue::glue('[
        {
          "name": "Advisor Name",
          "value": "<<current$advisor_name>>"
        }, 
        {
        "name": "Payout Name",
          "value": "<<current$payout_name>>"
        }, 
        {
        "name": "AUM Type",
        "value": "<<current$aum_type>>"
        }]', .open = "<<", .close = ">>")
    
    httr::PUT(str_c(bd_api$call_url, '/v1/account/', current$account_id, '/tag'), auth_header, body = account_body, encode='form')
  })



