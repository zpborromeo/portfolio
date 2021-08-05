# Author: Zach Borromeo
# Start Date: July 7, 2021
# Last Modified Date: July 7, 2021


batch_date <- lubridate::ymd("2021-07-12")
bd_accounts <- load_black_diamond_accounts(batch_date)

bd_portfolios <- load_black_diamond_portfolios(batch_date)




# account data
financial_account_data <- bd_accounts %>% 
  unnest(style, keep_empty = TRUE) %>% 
  unnest(fee_schedules, keep_empty = TRUE) %>% 
  rename(account_fee_schedule = Name) %>% 
  rowwise() %>% 
  mutate(client_city = if_else(is.null(address), "", address$City),
         client_zip = if_else(is.null(address), "", address$Zip),
         client_state = if_else(is.null(address), "", address$State),
         client_name = if_else(is.null(address), "", str_c(address$FirstName, " ", address$LastName)),
         client_address = if_else(is.null(address), "", str_c(address$Line1, " ", address$Line2, " ", address$Line3)),
         advisor_name = tags$Value[tags$Name == "Advisor Name"],
         sms_traded = tags$Value[tags$Name == "SMS Traded"],
         billing_spec = tags$Value[tags$Name == "Billing Spec"],
         billing_cycle = tags$Value[tags$Name == "Billing Cycle"],
         billed_custodian = tags$Value[tags$Name == "Billed Custodian"],
         market_value = sum(holdings$MarketValue))
 