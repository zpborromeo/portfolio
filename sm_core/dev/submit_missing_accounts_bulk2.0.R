source('utils.R')

case_type_query <- glue::glue("select Id,
Name
from RecordType
where Name like '%New Account%'")

new_account_cases <- sf_query(case_type_query)

ba_new_account_cases <- sf_query("select Id,
  Account_Number_from_Custodian__c
  from Case
  where Client_Contact_Lookup__c = '0032M00003KSIgDQAX'")


bd_accounts_not_in_sf <- readxl::read_excel("account_auto_case_gen.xlsx")

# bd_accounts_not_in_sf <- readxl::read_excel("account_auto_case_gen.xlsx") %>% 
#   left_join(ba_new_account_cases, by = "Account_Number_from_Custodian__c") %>% 
#   distinct()

response <- sf_create(bd_accounts_not_in_sf, api_type = "Bulk 2.0", object_name = "Case")

# response <- sf_update(bd_accounts_not_in_sf, api_type = "Bulk 2.0", object_name = "Case")