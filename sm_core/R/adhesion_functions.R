
# get all accounts in Salesforce that should be an Adhesion account
get_sf_adhesion_accounts <- function(style_cases, financial_accounts){
  
  adhesion_accounts <- style_cases %>% 
    left_join(financial_accounts, by = c("FinServ__FinancialAccount__c" = "Id")) %>% 
    mutate(account = if_else(is.na(FinServ__FinancialAccountNumber__c), Account_Number_from_Custodian__c, 
                             FinServ__FinancialAccountNumber__c)) %>% 
    filter(str_detect(Will_acct_be_SMS_traded__c, "Adhesion")) %>% 
    clean_names() %>% 
    mutate(created_date = lubridate::ymd_hms(created_date)) %>% 
    arrange(created_date) %>% 
    group_by(account) %>% 
    summarise(account = last(account),
              adhesion = last(will_acct_be_sms_traded_c)) %>% 
    filter(!is.na(account)) %>% 
    pull(account)
  
  adhesion_accounts
  
}



