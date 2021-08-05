# Author: Zach Borromeo
# Start Date: July 16, 2021
# Last Modified Date: July 16, 2021

# Functionality: To mass create a large number of client users based on list of
# data provided by financial advisors.

fee_schedules <- httr::content(httr::GET('https://api.bdreporting.com/v1/feeSchedule', auth_header, body = "", encode='form'))

clean_fee_schedules <- fee_schedules %>% 
  enframe() %>% 
  select(value) %>% 
  unnest_wider(value)

acct_fee_details <- httr::content(httr::GET('https://api.bdreporting.com/v1/account/z0524802341/feeSchedule', auth_header, body = "", encode='form'))

source('utils.R')

library(readxl)

#Pull Data from Excel Document

fee_update_list <- readxl::read_xlsx("fee_update.xlsx")


bd_account_ids <- bd_accounts %>% 
  select(Id, AccountNumber, FeeSchedules) %>% 
  right_join(fee_update_list, by = c("AccountNumber" = "Account Number")) %>% 
  janitor::clean_names() %>% 
  filter(fee_schedules != 'NULL') %>% 
  unnest_wider(fee_schedules)




bd_response <- bd_account_ids %>% 
  pmap(function(...) {
    
    current <- tibble(...)
    
    account_update <- glue::glue('{
      "feeScheduleID": "<<current$acct_fee_id>>",
      "discount": 0,
      "notes": "",
      "billingConfigID": "<<current$billing_config>>",
      "includesNonBillables": false,
      "feeSchedule": {
        "id": 0,
        "name": "",
        "discount": 0,
        "description": "",
        "notes": "",
        "feeScheduleType": "",
        "rateType": ""
      },
      "config": {
        "id": 0,
        "name": ""
      }
    }', .open = "<<", .close = ">>")

    httr::content(httr::PUT(str_c(bd_api$call_url, '/v1/account/', current$id, '/feeschedules'), auth_header, body = account_update, encode='form'))
    
  })
