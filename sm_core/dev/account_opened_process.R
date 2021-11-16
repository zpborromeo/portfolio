source('utils.R')

sna_cases_report <- salesforcer::sf_run_report('00O2M0000099yoJUAQ') %>% 
  clean_names()

cases_list <- sna_cases_report %>% 
  rename('account_number' = account_number_from_custodian)

new_accounts_list <- readxl::read_excel("C:/Users/Zach/OneDrive - Sowell Management/New_Accounts_List/New_Accounts_List.xlsx") %>% 
  clean_names()

# may need to include different files reads for Fidelity, TDA, and Schwab

clean_accounts_list <- new_accounts_list %>% 
  mutate(account_number = str_remove(account_number, coll('-')))

account_case_merge <- clean_accounts_list %>% 
  left_join(cases_list, by = c('account_number'))

accounts_without_cases <- account_case_merge %>% 
  filter(is.na(case_id))

accounts_with_cases <- account_case_merge %>% 
  filter(!is.na(case_id)) %>% 
  select(account_number, case_id, status) %>% 
  mutate(status = "Account Opened")

response <- accounts_with_cases %>% 
  split(f = rep(1:ceiling(nrow(accounts_with_cases) / 10), each = 10)[1:nrow(accounts_with_cases)]) %>% 
  map_dfr(~{
    
    sf_update(.x, object_name = "Case")
    
  })

save_document <- write.xlsx(accounts_without_cases, "C:/Users/Zach/OneDrive - Sowell Management/New_Accounts_List/accounts_without_cases.xlsx", overwrite = TRUE)