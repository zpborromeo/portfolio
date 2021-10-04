
send_bd_accounts_not_in_sf <- function(bd_accounts_not_in_sf, send_list, from_email){
  
  sending_date <- paste0(format(Sys.time(), "%A, %B "),
                         format(Sys.time(), "%d") %>% as.numeric(),
                         ", ", format(Sys.time(), "%Y"))
  
  message_body <- glue::glue("Hi,

    The attached accounts are present in Black Diamond, but not Salesforce.
    
    Thank you,
    
    Maintenace")
  
  email <-
    blastula::compose_email(
      body = blastula::md(message_body),
      footer = blastula::md(c("sent via [Sowell Management Services](https://www.sowellmanagement.com/) on ", sending_date)),
      header = blastula::md(blastula::add_image(file = 'logo.png'))
    ) %>% 
    blastula::add_attachment('data/bd_accounts_not_in_sf.csv')
  
  email %>% 
    blastula::smtp_send(
      from = from_email,
      to = send_list,
      subject = stringr::str_c("Accounts Not in Salesforce - SECURE"),
      credentials = blastula::creds_file(file = "sowell_billing")
    )
  
  
}


create_aon_case <- function(bd_accounts_not_in_sf, csr_by_team){
  
  csr_ids <- csr_by_team %>% 
    select(primary_csr) %>% 
    filter(!str_detect(primary_csr, coll('-'))) %>% 
    distinct() %>% 
    pull(primary_csr) %>% 
    map_dfr(~{
      
      get_sf_user_by_name(.x)
      
    })
  
  new_account_case_types <- get_case_types_by_name("Submit New Account")

  aon_case_types <- get_case_types_by_name("Account Opened by Advisor Notification")
  
  new_account_cases <- get_cases_by_type(new_account_case_types$Id, 365)
  aon_cases <- get_cases_by_type(aon_case_types$Id, 365)
  
  
  csr_by_team_with_id <- csr_by_team %>% 
    left_join(csr_ids, by = c('primary_csr' = 'Name'))
  
  accounts_to_add <- bd_accounts_not_in_sf %>% 
    filter(!(FinServ__FinancialAccountNumber__c %in% new_account_cases$Account_Number_from_Custodian__c)) %>% 
    filter(!(FinServ__FinancialAccountNumber__c %in% aon_cases$Account_Number_from_Custodian__c)) %>% 
    select(FinServ__FinancialAccountNumber__c, FinServ__Description__c, 
           FinServ__FinancialAccountType__c, BD_Team_Name__c, Advisor_Description__c, 
           Custodian_Bank__c, Client_Fee__c, Model__c, BD_Client_Name__c) %>% 
    rowwise() %>% 
    mutate(Assign_to__c = csr_by_team_with_id$primary_csr[amatch(BD_Team_Name__c, csr_by_team_with_id$account_name,
                                                                          method = c('jw'))],
           Status = 'New',
           RecordTypeId = '0122M000001cxXnQAI',
           Client_Contact_Lookup__c = '0032M00003HZho1QAD',
           Sowell_Private_Comment__c = glue::glue("Team - {BD_Team_Name__c}", "Account Type - {FinServ__FinancialAccountType__c}",
                                                  "Client Contact - {BD_Client_Name__c}", "Financial Advisor - {Advisor_Description__c}", 
                                                  "Custodian - {Custodian_Bank__c}", "Model - {Model__c}", "Account Name - {FinServ__Description__c}",
                                                  "Client Fee - {Client_Fee__c}"
                                                  , .sep = '\n')) %>% 
    select(Assign_to__c, Status, Client_Contact_Lookup__c, 
           Sowell_Private_Comment__c, RecordTypeId) %>% 
    head(n = 1) %>% 
    salesforcer::sf_create("Case", api_type = "REST")
  
  
}
  

filter_accounts_for_sf <- function(bd_accounts, batch_date, teams_to_remove, 
                                   custodians_to_include, data_providers_to_remove){
  
  filtered_bd_accounts <- bd_accounts %>% 
    clean_names() %>% 
    unnest(team, keep_empty = TRUE) %>% 
    select(-c(AccountId, TeamId)) %>% 
    rename(team = Name, account_name = name) %>%
    unnest(teams_by_rep_code, keep_empty = TRUE) %>% 
    select(-c(AccountId, TeamId)) %>% 
    rename(team_by_rep_code = Name) %>% 
    mutate(team = if_else(is.na(team), team_by_rep_code, team)) %>% 
    filter(!(team %in% teams_to_remove)) %>% 
    filter(is.na(closed_date)) %>% 
    filter(as_of_date == str_c(batch_date, "T00:00:00")) %>% 
    filter(custodian %in% custodians_to_include) %>% 
    filter(!(data_provider %in% data_providers_to_remove)) %>% 
    rowwise() %>% 
    mutate(account_number = str_remove(long_number, "^[^_]+_"),
           payout_name = tags$Value[tags$Name == "Payout Name"],
           payout_name = str_squish(payout_name),
           payout_name = str_trim(payout_name)) %>% 
    filter(!(payout_name %in% teams_to_remove))
  
  filtered_bd_accounts
  
}

build_sf_financial_holdings <- function(filtered_bd_accounts, bd_account_numbers, sec_asset_types){
  
  financial_holding_data <- filtered_bd_accounts %>% 
    filter(account_number %in% bd_account_numbers) %>% 
    unnest(holdings, keep_empty = TRUE) %>% 
    filter(MarketValue > 1) %>% 
    filter(!(str_detect(AssetName, coll("^")) | str_detect(AssetName, coll("*")))) %>% 
    select(as_of_date, MarketValue, account_number,
           account_name, AssetName, Ticker, ClassName, 
           IssueType, SegmentName, Sector, AssetId) %>% 
    left_join(sec_asset_types, by = c("AssetId" = "asset_id")) %>% 
    select(-c(AssetId)) %>% 
    mutate(External_Id__c = str_c(account_number, Ticker)) %>% 
    rename(FinServ__LastUpdated__c = as_of_date,
           Account_Number__c = account_number,
           FinServ__MarketValue__c = MarketValue,
           Financial_Account_Name__c = account_name,
           Name = AssetName,
           FinServ__AssetClass__c = ClassName,
           FinServ__AssetCategory__c = sec_asset_type,
           Security_Type__c = IssueType,
           Sector__c = SegmentName,
           Subsector__c = Sector,
           Symbol_Text__c = Ticker) %>% 
    distinct()
  
}


build_sf_financial_accounts <- function(filtered_bd_accounts){
  
  financial_account_data <- filtered_bd_accounts %>% 
    unnest(style, keep_empty = TRUE) %>% 
    unnest(fee_schedules, keep_empty = TRUE) %>% 
    rename(account_fee_schedule = Name) %>% 
    rowwise() %>% 
    mutate(style = ifelse(is.null(style), "", style),
           client_city = if_else(is.null(address), "", address$City),
           client_zip = if_else(is.null(address), "", address$Zip),
           client_state = if_else(is.null(address), "", address$State),
           client_name = if_else(is.null(address), "", str_c(address$FirstName, " ", address$LastName)),
           client_address = if_else(is.null(address), "", str_c(address$Line1, " ", address$Line2, " ", address$Line3)),
           advisor_name = tags$Value[tags$Name == "Advisor Name"],
           billing_spec = tags$Value[tags$Name == "Billing Spec"],
           billing_cycle = tags$Value[tags$Name == "Billing Cycle"],
           billed_custodian = tags$Value[tags$Name == "Billed Custodian"],
           market_value = sum(holdings$MarketValue)) %>% 
    select(market_value, account_name, account_registration_type,
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
           FinServ__CloseDate__c = closed_date) %>% 
    distinct()
  
}