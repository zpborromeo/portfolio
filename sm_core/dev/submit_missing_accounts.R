source('utils.R')

# ba_new_account_cases <- sf_query("select Id,
#   Account_Number_from_Custodian__c
#   from Case
#   where Client_Contact_Lookup__c = '0032M00003KSIgDQAX'")

# bd_accounts_not_in_sf <- readxl::read_excel("account_auto_case_gen.xlsx") %>% 
#   left_join(ba_new_account_cases, by = "Account_Number_from_Custodian__c") %>% 
#   distinct()

bd_accounts_not_in_sf <- readxl::read_excel("account_auto_case_gen.xlsx")

response <- bd_accounts_not_in_sf %>% 
  split(f = rep(1:ceiling(nrow(bd_accounts_not_in_sf) / 25), each = 25)[1:nrow(bd_accounts_not_in_sf)]) %>% 
  map_dfr(~{
    
    sf_create(.x, object_name = "Case")
    
    
  })


# contact_type_query1 <- sprintf("select Id,
#     Account_Number_from_Custodian__c
#     from Case
#     WHERE Id in ('%s')", 
#     paste0(unique(response$id[!is.na(response$id)]), collapse = "','"))
# 
# contact_ID <- sf_query(contact_type_query1)
# 
# 
# failed_accts <- bd_accounts_not_in_sf %>% 
#   filter(!(Account_Number_from_Custodian__c %in% contact_ID$Account_Number_from_Custodian__c))
# 
# write_csv(failed_accts, "failed_accts.csv")