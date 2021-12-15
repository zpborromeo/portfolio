source('utils.R')

update_business_accounts <- readxl::read_excel("Excel_Files/update_business_accounts.xlsx") 

response_update_businesses <- update_business_accounts %>% 
  split(f = rep(1:ceiling(nrow(update_business_accounts) / 10), each = 10)[1:nrow(update_business_accounts)]) %>% 
  map_dfr(~{
    
    sf_update(.x, object_name = "Account")
    
  })