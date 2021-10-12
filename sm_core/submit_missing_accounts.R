source('utils.R')

ba_new_account_cases <- sf_query("select Id,
  Account_Number_from_Custodian__c
  from Case
  where Client_Contact_Lookup__c = '0032M00003KSIgDQAX'")


bd_accounts_not_in_sf <- readxl::read_excel("account_auto_case_gen.xlsx") %>% 
  left_join(ba_new_account_cases, by = "Account_Number_from_Custodian__c") %>% 
  distinct()

response <- bd_accounts_not_in_sf %>% 
  split(f = rep(1:ceiling(nrow(bd_accounts_not_in_sf) / 25), each = 25)[1:nrow(bd_accounts_not_in_sf)]) %>% 
  map_dfr(~{
    
    sf_update(.x, object_name = "Case")
    
    
  })

