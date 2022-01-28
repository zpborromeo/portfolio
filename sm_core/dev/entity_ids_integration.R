source('utils.R')

addepar_accounts <- readxl::read_excel("Excel_Files/addepar-accts.xlsx") %>% 
  clean_names()

sf_sandbox_accounts <- readxl::read_excel("Excel_Files/sandbox-accounts.xlsx") %>% 
  clean_names() %>% 
  rename("financial_account_name_sandbox" = financial_account_financial_account_name) %>% 
  rename("holding_account_number" = account_number)

sf_production_accounts <- readxl::read_excel("Excel_Files/production-accounts.xlsx") %>% 
  clean_names()%>% 
  rename("financial_account_name_production" = financial_account_financial_account_name) %>% 
  rename("holding_account_number" = account_number)

production_addepar_join <- sf_production_accounts %>% 
  left_join(addepar_accounts, by = c("holding_account_number")) %>% 
  select(financial_account_name_production, financial_account_id, entity_id, holding_account_number) %>% 
  filter(!is.na(entity_id))

sandbox_addepar_join <- sf_sandbox_accounts %>% 
  left_join(addepar_accounts, by = c("holding_account_number")) %>% 
  select(financial_account_name_sandbox, financial_account_id, entity_id, holding_account_number) %>% 
  filter(!is.na(entity_id))

save_document <- write.csv(production_addepar_join, "Excel_Files/production_addepar_join.csv")
save_document <- write.csv(sandbox_addepar_join, "Excel_Files/sandbox_addepar_join.csv")