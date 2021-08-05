# perform management style change

source('utils.R')

style_key <- tbl(pool, "style") %>% 
  select(salesforce_name, black_diamond_name, black_diamond_id) %>% 
  collect()


tasks <- get_open_sf_tasks(billing_api_user_id)

msc_tasks <- get_msc_tasks(tasks)

msc_cases <- get_sf_cases_by_id(msc_tasks$WhatId)

financial_accounts <- get_financial_account_by_case(msc_cases)

accounts_to_update <- build_style_update_accounts(msc_cases, financial_accounts, style_key, bd_account_ids)

accounts_to_update <- accounts_to_update %>% 
  filter(!is.na(bd_id))


bd_response <- accounts_to_update %>% 
  pmap(function(...) {
    
    current <- tibble(...)
    
    style_body <- glue::glue('{
          "name": "<<current$black_diamond_name>>",
          "id": "<<current$black_diamond_id>>",
          "assignedBenchmarkName": "",
          "isSMAClassified": false,
          "smaClassification": ""
        }', .open = "<<", .close = ">>")
    
    httr::PUT(str_c(bd_api$call_url, '/v1/account/', current$bd_id, '/style'), auth_header, body = style_body, encode='form')
    
  })


sf_response <- msc_tasks %>%
  filter(WhatId %in% accounts_to_update$Id) %>%
  complete_sf_task()



