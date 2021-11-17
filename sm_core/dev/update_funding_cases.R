source('utils.R')

funding_cases_report <- salesforcer::sf_run_report('00O2M0000099wS1UAI') %>% 
  clean_names()

funding_cases_table <- funding_cases_report %>% 
  select(case_id, status, percent_funded, financial_account_balance) %>% 
  rename(id = case_id)

updated_funding_cases_partial <- funding_cases_table %>%
  mutate(percent_funded = str_remove(percent_funded, coll('%')), 
         percent_funded = str_remove(percent_funded, coll(',')),
         financial_account_balance = str_remove(financial_account_balance, coll('$')),
         percent_funded = as.numeric(percent_funded)) %>%
  filter(status == "Account Opened") %>% 
  mutate(status = if_else(financial_account_balance >= 1.00, "Partially Funded", status)) %>%
  filter( status == "Partially Funded") %>%
  select(-c(percent_funded, financial_account_balance))

updated_funding_cases_fully <- funding_cases_table %>%
  mutate(percent_funded = str_remove(percent_funded, coll('%')), 
         percent_funded = str_remove(percent_funded, coll(',')),
         financial_account_balance = str_remove(financial_account_balance, coll('$')),
         percent_funded = as.numeric(percent_funded)) %>%
  filter(status == "Account Opened") %>% 
  mutate(status = if_else(percent_funded >= 75.00, "Fully Funded", status)) %>% 
  filter(status == "Fully Funded") %>%
  select(-c(percent_funded, financial_account_balance))


response_partiallyfunded <- updated_funding_cases_partial %>% 
  split(f = rep(1:ceiling(nrow(updated_funding_cases_partial) / 10), each = 10)[1:nrow(updated_funding_cases_partial)]) %>% 
  map_dfr(~{
    
    sf_update(.x, object_name = "Case")
    
  })

response_fullyfunded <- updated_funding_cases_fully %>% 
  split(f = rep(1:ceiling(nrow(updated_funding_cases_fully) / 10), each = 10)[1:nrow(updated_funding_cases_fully)]) %>% 
  map_dfr(~{
    
    sf_update(.x, object_name = "Case")
    
  })

