source('utils.R')


batch_date <- get_latest_settlement_date(lubridate::today())
bd_accounts <- load_black_diamond_accounts(batch_date)
bd_portfolios <- load_black_diamond_portfolios(batch_date)
bd_portfolio_groups <- load_black_diamond_portfolio_groups(batch_date)

portfolio_groups <- get_portfolio_groups(bd_portfolios, bd_portfolio_groups)

portfolios <-  get_portfolios(bd_portfolios)

portfolio_fee_schedules <- get_portfolio_fee_schedules(bd_portfolios)

clean_accounts <- get_clean_accounts(bd_accounts, portfolios,
                                     portfolio_groups, portfolio_fee_schedules)

attributes <- get_addepar_attributes(clean_accounts)


# get salesforce attibutes

sf_financial_accounts <- sf_query("SELECT
                           Financial_Advisor__r.Name,
                           Financial_Advisor__r.Account.Name,
                           FinServ__FinancialAccountNumber__c,
                           Name,
                           Custodian_Rep_Id__c,
                           FinServ__FinancialAccountType__c,
                           FinServ__PrimaryOwner__r.Name,
                           Custodian_Bank__c,
                           Client_Fee__c,
                           Model__c,
                           FinServ__Address1__c,
                           FinServ__City__c,
                           FinServ__State__c,
                           FinServ__PostalCode__c
                    FROM FinServ__FinancialAccount__c",
                                  api_type = c("Bulk 2.0"), object_name = 'FinServ__FinancialAccount__c')


sf_account_attributes <- sf_financial_accounts %>% 
  filter(FinServ__FinancialAccountNumber__c %in% attributes$account_number)


colnames(sf_account_attributes) <- sub("FinServ\\__", "", colnames(sf_account_attributes))
colnames(sf_account_attributes) <- sub("BD\\_", "", colnames(sf_account_attributes))
colnames(sf_account_attributes) <- sub("\\__c", "", colnames(sf_account_attributes))
colnames(sf_account_attributes) <- sub("\\__r", "", colnames(sf_account_attributes))

sf_account_attributes <- sf_account_attributes %>% 
  clean_names() %>% 
  rename(account_name = name,
         team = financial_advisor_account_name,
         advisor_name = financial_advisor_name,
         custodian = custodian_bank,
         address = address1, account_number = financial_account_number,
         account_registration_type = financial_account_type,
         zip = postal_code, style = model, rep_code = custodian_rep_id,
         portfolio = primary_owner_name, fee_schedule_name = client_fee) %>% 
  relocate(account_number, .before = everything())


# ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# style
new_account_cases <- sf_query("select Id,
                           CaseNumber,
                           Account_Number_from_Custodian__c,
                           RecordType.Name,
                           Model_Selection__c,
                           Client_Fee__c,
                           Model_type__c,
                           CreatedDate
                    from Case
                    where RecordType.Name like '%Submit New Account%'")


clean_new_account_cases <- new_account_cases %>% 
  clean_names() %>% 
  rename(account_number = account_number_from_custodian_c,
         model = model_selection_c, client_fee = client_fee_c, model_type = model_type_c) %>% 
  mutate(model = if_else(model_type == "ETF" & record_type_name == 'EBA - Submit New Account', str_c(model, model_type, sep = ' '), model)) %>% 
  select(account_number, case_number, created_date, model, client_fee, record_type_name) %>% 
  filter(!is.na(account_number)) 





style_change_cases <- sf_query("select CaseNumber,
                           Account_Number_from_Custodian__c,
                           FinServ__FinancialAccount__r.FinServ__FinancialAccountNumber__c,
                           RecordType.Name,
                           New_model__c,
                           Model_type__c,
                           Advisor_Code__c,
                           CreatedDate
                    from Case
                    where RecordType.Name like '%Style%'")

clean_style_change_cases <- style_change_cases %>% 
  clean_names() %>% 
  rename(account_number = fin_serv_financial_account_r_fin_serv_financial_account_number_c,
         model = new_model_c, model_type = model_type_c, rep_code = advisor_code_c) %>% 
  mutate(model = if_else(model_type == "ETF" & record_type_name == 'EBA - Submit New Account', str_c(model, model_type, sep = ' '), model)) %>% 
  filter(!is.na(account_number)) %>% 
  select(account_number, case_number, created_date, model, rep_code, record_type_name)



billing_and_reporting_cases <- sf_query("select CaseNumber,
                           Account_Number_from_Custodian__c,
                           FinServ__FinancialAccount__r.FinServ__FinancialAccountNumber__c,
                           RecordType.Name,
                           New_Fee__c,
                           CreatedDate
                    from Case
                    where RecordType.Name like '%Billing & Reporting%'")

clean_billing_and_reporting_cases <- billing_and_reporting_cases %>% 
  clean_names() %>% 
  filter(!is.na(new_fee_c)) %>% 
  rename(account_number = fin_serv_financial_account_r_fin_serv_financial_account_number_c,
         client_fee = new_fee_c) %>% 
  mutate(account_number = if_else(is.na(account_number), 
                                  account_number_from_custodian_c, account_number)) %>% 
  filter(!is.na(account_number)) %>% 
  select(account_number, case_number, created_date, client_fee, record_type_name)


styles <- clean_new_account_cases %>% 
  bind_rows(clean_style_change_cases) %>% 
  arrange(desc(created_date)) %>% 
  group_by(account_number) %>% 
  summarise(case_style = first(model))

fee_schedule_names <-clean_new_account_cases %>% 
  bind_rows(clean_billing_and_reporting_cases) %>% 
  arrange(desc(created_date)) %>% 
  group_by(account_number) %>% 
  summarise(case_client_fee = first(client_fee))


clean_attribute_cases <- clean_new_account_cases %>% 
  bind_rows(clean_style_change_cases) %>% 
  bind_rows(clean_billing_and_reporting_cases)


sf_account_attributes <- sf_account_attributes %>% 
  left_join(styles, by = c("account_number")) %>% 
  left_join(fee_schedule_names, by = c("account_number"))



wb <- openxlsx::createWorkbook()

openxlsx::addWorksheet(wb, 'addepar_bd_data')
openxlsx::addWorksheet(wb, 'salesforce_data')

openxlsx::writeData(wb, 'addepar_bd_data', attributes)
openxlsx::writeData(wb, 'salesforce_data', sf_account_attributes)

openxlsx::saveWorkbook(wb, file = "system_data_comp.xlsx", overwrite = TRUE)
