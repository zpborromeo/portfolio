source('utils.R')

account_update <- readxl::read_xlsx("Excel_Files/zenith-sw-accts.xlsx")

response <- account_update %>% 
  split(f = rep(1:ceiling(nrow(account_update) / 10), each = 10)[1:nrow(account_update)]) %>% 
  map_dfr(~{
    
    sf_update(.x, object_name = "FinServ__FinancialAccount__c")
    
    
  })