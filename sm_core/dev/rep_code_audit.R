source('utils.R')


rep_code_query <- glue::glue("SELECT Name,
                           Financial_Advisor_Firm_Lookup__c,
                           Financial_Advisor_Lookup__c,
                           Rep_Code_Custodian__c,
                           Advisor_Type__c,
                           Custodian_Pricing__c,
                           Data_Provider__c,
                           Status__c
                    FROM Rep_Codes__c")


rep_codes <- sf_query(rep_code_query)

rep_codes <- rep_codes %>% 
  rename(rep_code = Name)
  

firm_query <- sprintf("SELECT Id,
                           Name
                    FROM Account
                    WHERE Id in ('%s')", 
                                   paste0(rep_codes$Financial_Advisor_Firm_Lookup__c, 
                                          collapse = "','"))

firms <- sf_query(firm_query, object_name = 'Account', api_type = c("Bulk 1.0"))

firms <- firms %>% 
  rename(firm_id = Id, firm_name = Name)


advisor_query <- sprintf("SELECT Id,
                           Name
                    FROM Contact
                    WHERE Id in ('%s')", 
                      paste0(rep_codes$Financial_Advisor_Lookup__c, collapse = "','"))

advisors <- sf_query(advisor_query, object_name = 'Contact', api_type = c("Bulk 1.0"))

advisors <- advisors %>% 
  rename(advisor_id = Id, sf_advisor_name = Name)

rep_codes <- rep_codes %>% 
  left_join(firms, by = c("Financial_Advisor_Firm_Lookup__c" = "firm_id")) %>% 
  left_join(advisors, by = c("Financial_Advisor_Lookup__c" = "advisor_id")) %>% 
  select(rep_code, sf_advisor_name)


rep_codes <- rep_codes %>% 
  mutate(rep_code = as.character(rep_code))


rep_codes <- rep_codes %>% 
  mutate(rep_code = str_trim(rep_code),
         rep_code = str_squish(rep_code))


clean_rep_codes <- rep_codes %>% 
  mutate(rep_code = str_trim(rep_code),
         rep_code = if_else(str_sub(rep_code, 2, 2) == 8, str_sub(rep_code, 2, nchar(rep_code)), rep_code))

new_wave <- readxl::read_excel('new_wave.xlsx')

test <- new_wave %>% 
  clean_names() %>% 
  select(account_number, primary_rep_code, portfolio_group, advisor_name) %>% 
  mutate(primary_rep_code = as.character(primary_rep_code),
         primary_rep_code = str_trim(primary_rep_code),
         primary_rep_code = str_squish(primary_rep_code)) %>% 
  left_join(rep_codes, by = c('primary_rep_code' = 'rep_code'))







