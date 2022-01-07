source('utils.R')

sna_cases_report <- salesforcer::sf_run_report('00O2M0000099yoJUAQ') %>% 
  clean_names()

current_date <- get_latest_settlement_date(lubridate::today())

cases_list <- sna_cases_report %>% 
  rename('account_number' = account_number_from_custodian) %>% 
  mutate(account_number = as.double(account_number))

new_accounts_list_fid <- readxl::read_excel("New_Accounts_List/New_Accounts_List-Fid.xlsx") %>% 
  clean_names()

new_accounts_list_schwab <- readxl::read_excel("New_Accounts_List/New_Accounts_List-Schwab.xlsx") %>% 
  clean_names()

new_accounts_list_td <- readxl::read_excel("New_Accounts_List/New_Accounts_List-TD.xlsx") %>% 
  clean_names()

#clean acocunts list per custodian

clean_accounts_list_fid <- new_accounts_list_fid %>% 
  mutate(account_number = str_remove(account_number, coll('-'))) %>% 
  mutate(account_number = as.double(account_number))

clean_accounts_list_schwab <- new_accounts_list_schwab %>% 
  rename('account_number' = account) %>% 
  mutate(account_number = str_remove(account_number, coll('-'))) %>% 
  mutate(account_number = as.double(account_number))

clean_accounts_list_td <- new_accounts_list_td %>%
  rename('td_status' = status)  %>% 
  mutate(account_number = as.double(account_number))
  # %>% 
  # mutate(open_date = lubridate::ymd(open_date)) %>% 
  # filter(open_date == current_date) %>% 
  # mutate(account_number = str_remove(account_number, coll('-')))

#account merge on Fidelity list

account_case_merge_fid <- clean_accounts_list_fid %>% 
  left_join(cases_list, by = c('account_number'))

accounts_without_cases_fid <- account_case_merge_fid %>% 
  filter(is.na(case_id))

accounts_with_cases_fid <- account_case_merge_fid %>% 
  filter(!is.na(case_id)) %>% 
  select(case_id, status) %>% 
  mutate(status = "Account Opened")%>% 
  rename('Id' = case_id)

#account merge on Schwab list

account_case_merge_schwab <- clean_accounts_list_schwab %>% 
  left_join(cases_list, by = c('account_number'))

accounts_without_cases_schwab <- account_case_merge_schwab %>% 
  filter(is.na(case_id))

accounts_with_cases_schwab <- account_case_merge_schwab %>% 
  filter(!is.na(case_id)) %>% 
  select(case_id, status) %>% 
  mutate(status = "Account Opened")%>% 
  rename('Id' = case_id)

#account merge on TDA list

account_case_merge_td <- clean_accounts_list_td %>%
  left_join(cases_list, by = c('account_number'))

accounts_without_cases_td <- account_case_merge_td %>% 
  filter(is.na(case_id))

accounts_with_cases_td <- account_case_merge_td %>% 
  filter(!is.na(case_id)) %>% 
  select(case_id, status) %>% 
  mutate(status = "Account Opened") %>% 
  rename('Id' = case_id)

#responses for case searches

response_fid <- accounts_with_cases_fid %>% 
  split(f = rep(1:ceiling(nrow(accounts_with_cases_fid) / 10), each = 10)[1:nrow(accounts_with_cases_fid)]) %>% 
  map_dfr(~{
    
    sf_update(.x, object_name = "Case")
    
  })

save_document <- write.csv(accounts_without_cases_fid, "New_Accounts_List/accounts_without_cases_fid.csv")


response_schwab <- accounts_with_cases_schwab %>% 
  split(f = rep(1:ceiling(nrow(accounts_with_cases_schwab) / 10), each = 10)[1:nrow(accounts_with_cases_schwab)]) %>% 
  map_dfr(~{
    
    sf_update(.x, object_name = "Case")
    
  })

save_document <- write.csv(accounts_without_cases_schwab, "New_Accounts_List/accounts_without_cases_schwab.csv")

response_td <- accounts_with_cases_td %>% 
  split(f = rep(1:ceiling(nrow(accounts_with_cases_td) / 10), each = 10)[1:nrow(accounts_with_cases_td)]) %>% 
  map_dfr(~{
    
    sf_update(.x, object_name = "Case")
    
  })

save_document <- write.csv(accounts_without_cases_td, "New_Accounts_List/accounts_without_cases_td.csv")

