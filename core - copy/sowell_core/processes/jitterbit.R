# jitterbit mimic

sec_asset_types <- tbl(pool, 'security_master') %>% 
  select(black_diamond_asset_id, sec_asset_type) %>% 
  collect()

# core pre-processing
filtered_bd_accounts <- bd_accounts %>% 
  clean_names() %>% 
  unnest(team, keep_empty = TRUE) %>% 
  rename(team = Name, account_name = name) %>% 
  filter(!(team %in% teams_to_remove)) %>% 
  filter(is.na(closed_date)) %>% 
  filter(custodian %in% custodians_to_include) %>% 
  filter(!(data_provider %in% data_providers_to_remove)) %>% 
  select(-c(AccountId)) %>% 
  rowwise() %>% 
  mutate(account_number = str_remove(long_number, "^[^_]+_")) %>% 
  mutate(payout_name = tags$Value[tags$Name == "Payout Name"],
         payout_name = str_squish(payout_name),
         payout_name = str_trim(payout_name)) %>% 
  filter(!(payout_name %in% teams_to_remove)) %>%
  filter(!(is.na(team) & is.na(payout_name))) %>% 
  filter(as_of_date == '2021-07-12T00:00:00')
  


# account data
financial_account_data <- filtered_bd_accounts %>% 
  unnest(style, keep_empty = TRUE) %>% 
  unnest(fee_schedules, keep_empty = TRUE) %>% 
  rename(account_fee_schedule = Name) %>% 
  rowwise() %>% 
  mutate(client_city = if_else(is.null(address), "", address$City),
         client_zip = if_else(is.null(address), "", address$Zip),
         client_state = if_else(is.null(address), "", address$State),
         client_name = if_else(is.null(address), "", str_c(address$FirstName, " ", address$LastName)),
         client_address = if_else(is.null(address), "", str_c(address$Line1, " ", address$Line2, " ", address$Line3)),
         advisor_name = tags$Value[tags$Name == "Advisor Name"],
         sms_traded = tags$Value[tags$Name == "SMS Traded"],
         billing_spec = tags$Value[tags$Name == "Billing Spec"],
         billing_cycle = tags$Value[tags$Name == "Billing Cycle"],
         billed_custodian = tags$Value[tags$Name == "Billed Custodian"],
         market_value = sum(holdings$MarketValue)) %>% 
  select(-c(tags, address, holdings, TeamId, FeeScheduleID, Discount, Description,
            Notes, FeeScheduleType, RateType,
            BillingConfigurationID, BillingConfiguration,
            RepFeeSplit, IncludeNonBillables,
            PortfolioBreakpoint)) %>% 
  select(long_number, market_value, account_name, account_registration_type,
         team, payout_name, advisor_name, custodian, billed_custodian, account_fee_schedule,
         billing_spec, billing_cycle, style, client_name, client_address,
         client_city,  client_state, client_zip, closed_date, account_number) %>% 
  filter(market_value > 1) %>% 
  rename(FinServ__FinancialAccountNumber__c = account_number,
         FinServ__Balance__c = market_value,
         FinServ__Description__c = account_name,
         FinServ__FinancialAccountType__c = account_registration_type,
         BD_Team_Name__c = team,
         Pay_Out_Name_Text__c = payout_name,
         Advisor_Description__c = advisor_name,
         Custodian_Bank__c = custodian,
         Billed_Custodian__c = billed_custodian,
         Client_Fee__c = account_fee_schedule,
         BD_Billing_Spec__c = billing_spec,
         BD_Billing_Cycle__c = billing_cycle,
         Model__c = style,
         BD_Client_Name__c = client_name,
         FinServ__Address1__c = client_address,
         FinServ__City__c = client_city,
         FinServ__State__c = client_state,
         FinServ__PostalCode__c = client_zip,
         FinServ__CloseDate__c = closed_date)


# testing

bd_export <- readxl::read_excel('bd_port_jitter.xlsx') %>% 
  mutate(account_number = str_remove(`Original Account Number`, "^[^_]+_"))

rec_delta <- bd_export %>% 
  filter(account_number %in% rec$account_number)

delta <- bd_export %>% 
  filter(!(account_number %in% financial_account_data$FinServ__FinancialAccountNumber__c))

# salesforce field labels
# closed_date is blank, need to format correctly - check what value is used in SF if blank

# holding data 

financial_holding_data <- filtered_bd_accounts %>% 
  unnest(holdings, keep_empty = TRUE) %>% 
  filter(MarketValue > 1) %>% 
  filter(!(str_detect(AssetName, coll("^")) | str_detect(AssetName, coll("*")))) %>% 
  select(as_of_date, MarketValue, account_number,
         account_name, AssetName, DisplayCusip, ClassName, 
         IssueType, SegmentName, Sector, AssetId) %>% 
  left_join(sec_asset_types, by = c("AssetId" = "black_diamond_asset_id")) %>% 
  mutate(External_Id__c = str_c(account_number, DisplayCusip)) %>% 
  rename(FinServ__LastUpdated__c = as_of_date,
         Account_Number__c = account_number,
         FinServ__FinancialAccountNumber__c = account_number,
         FinServ__MarketValue__c = MarketValue,
         Financial_Account_Name = account_name,
         Name = AssetName,
         FinServ__AssetClass__c = ClassName,
         FinServ__AssetCategory__c = sec_asset_type,
         Security_Type__c = IssueType,
         Sector = SegmentName,
         Subsector__c = Sector,
         Symbol_Text__c = DisplayCusip)

  

# 1 - run upsert for financail accounts & holdings
  
# salesforcer::sf_upsert(financial_account_data,
#                        object_name = "FinServ__FinancialAccount__c",
#                        external_id_fieldname = "FinServ__FinancialAccountNumber__c")
# salesforcer::sf_upsert(financial_holding_data,
#                        object_name = "FinServ__FinancialHolding__c",
#                        external_id_fieldname = "External_Id__c")

# 2 - run queries 

financial_account_query <- glue::glue("select Id,
                                              FinServ__CashBalance__c
                                       from FinServ__FinancialAccount__c 
                                       where Mins_Since_Modified__c > 120")

financial_accounts_to_zero_out <- sf_query(financial_account_query)


# response <- financial_accounts_to_zero_out %>%
#   mutate(FinServ__CashBalance__c = 0) %>%
#   sf_update(object_name = "FinServ__FinancialAccount__c")


financial_holding_query <- glue::glue("select Id
                                       from FinServ__FinancialHolding__c 
                                       where LastModifiedDate < TODAY")

financial_holdings_to_zero_out <- sf_query(financial_holding_query)

# salesforcer::sf_delete(financial_holdings_to_zero_out, object_name = "FinServ__FinancialHolding__c")
  
