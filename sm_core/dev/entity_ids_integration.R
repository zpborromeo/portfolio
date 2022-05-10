source('utils.R')

addepar_accounts <- readxl::read_excel("Excel_Files/addepar-accts.xlsx") %>% 
  clean_names() %>% 
  mutate(account_number = as.numeric(account_number)) %>% 
  rename("holding_account_number" = account_number)

sf_sandbox_accounts <- readxl::read_excel("Excel_Files/sandbox-accounts.xlsx") %>% 
  clean_names() %>% 
  rename("financial_account_name_sandbox" = financial_account_financial_account_name) %>% 
  mutate(account_number = as.numeric(account_number)) %>% 
  rename("holding_account_number" = account_number)

sf_production_accounts <- readxl::read_excel("Excel_Files/production-accounts.xlsx") %>% 
  clean_names()%>% 
  rename("financial_account_name_production" = financial_account_financial_account_name) %>% 
  mutate(account_number = as.numeric(account_number)) %>% 
  rename("holding_account_number" = account_number)

production_addepar_join <- sf_production_accounts %>% 
  left_join(addepar_accounts, by = c("holding_account_number")) %>% 
  select(financial_account_name_production, financial_account_id, entity_id, holding_account_number) %>% 
  filter(!is.na(entity_id)) %>% 
  filter(!is.na(holding_account_number)) %>% 
  rename("Id" = financial_account_id)

sandbox_addepar_join <- sf_sandbox_accounts %>% 
  left_join(addepar_accounts, by = c("holding_account_number")) %>% 
  select(financial_account_name_sandbox, financial_account_id, entity_id, holding_account_number) %>% 
  filter(!is.na(entity_id)) %>% 
  filter(!is.na(holding_account_number)) %>% 
  rename("Id" = financial_account_id)

save_document <- write.csv(production_addepar_join, "Excel_Files/production_addepar_join.csv")
save_document <- write.csv(sandbox_addepar_join, "Excel_Files/sandbox_addepar_join.csv")

production_accounts_entityIDs <- production_addepar_join %>% 
  select(Id, entity_id) %>% 
  rename("Addepar_Entity_ID__c" = entity_id)


response <- production_accounts_entityIDs %>% 
  split(f = rep(1:ceiling(nrow(production_accounts_entityIDs) / 10), each = 10)[1:nrow(production_accounts_entityIDs)]) %>% 
  map_dfr(~{
  
    sf_update(.x, object_name = "FinServ__FinancialAccount__c")
    
  })
