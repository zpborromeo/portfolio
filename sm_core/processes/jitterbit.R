# jitterbit mimic


setwd("C:\\sm-core\\sm_core")

source('utils.R')

batch_date <- get_latest_settlement_date(lubridate::today())

last_batch <- readr::read_csv('data/last_batch.csv') %>% 
  mutate(date = lubridate::ymd(date))

salesforce_update_status <- readr::read_file('data/salesforce_updated')

# csr_by_team <- salesforcer::sf_run_report('00O2M000009OFRVUA4') %>% 
#   clean_names() %>% 
#   select(-account_id) %>% 
#   distinct()



if(last_batch$date == batch_date & last_batch$status == 'complete' & salesforce_update_status == 'FALSE'){
  
  sec_asset_types <- readr::read_csv('data/security_master.csv') %>% 
    clean_names() %>% 
    select(asset_id, sec_asset_type) %>% 
    distinct()
  

  bd_accounts <- load_black_diamond_accounts(batch_date)
  
  # core pre-processing
  filtered_bd_accounts <- filter_accounts_for_sf(bd_accounts, batch_date, teams_to_remove, 
                                                 custodians_to_include, data_providers_to_remove)
  
  # build sf financial account data
  financial_account_data <- build_sf_financial_accounts(filtered_bd_accounts)
  
  # get accounts numbers in Black Diamond
  bd_account_numbers <- financial_account_data %>%
    pull(FinServ__FinancialAccountNumber__c)
  
  # build sf financial holding data 
  financial_holding_data <- build_sf_financial_holdings(filtered_bd_accounts, bd_account_numbers, sec_asset_types)
  
  # get sf financial account object Ids by BD account number
  sf_financial_accounts <- get_financial_account_by_account_number(bd_account_numbers)
  
  bd_accounts_not_in_sf <- financial_account_data %>% 
    filter(!(FinServ__FinancialAccountNumber__c %in% sf_financial_accounts$FinServ__FinancialAccountNumber__c)) %>% 
    write_csv('data/bd_accounts_not_in_sf.csv')
  
  
  # send_bd_accounts_not_in_sf(bd_accounts_not_in_sf,
  #                            c('noah.holtz@sowellmanagement.com', 'zach.borromeo@sowellmanagement.com'), from_email)
  # 

  
  # accounts -----------------------------------------------------------------------------------------------
  
  sf_financial_accounts_to_update <- financial_account_data %>% 
    left_join(sf_financial_accounts, by = c('FinServ__FinancialAccountNumber__c')) %>% 
    filter(!is.na(Id))
  
  update_responses <- sf_financial_accounts_to_update %>% 
    split(f = rep(1:ceiling(nrow(sf_financial_accounts_to_update) / 5000), each = 5000)[1:nrow(sf_financial_accounts_to_update)]) %>% 
    map_dfr(~{
      
      current_response <- salesforcer::sf_update(.x, api_type = "Bulk 2.0",
                                                 object_name = "FinServ__FinancialAccount__c") %>% 
        modify_at(c("sf__Id"), as.character)
      
      errors <- current_response %>%
        filter(!is.na(sf__Error)) %>% 
        select(-c(sf__Id, sf__Created, sf__Error))
      
      current_response <- current_response %>% 
        filter(is.na(sf__Error))
      
      count <- 0
      
      while (nrow(errors) > 0 & count < 5) {
        
        reprocess_response <- salesforcer::sf_update(errors, api_type = "Bulk 2.0",
                                                     object_name = "FinServ__FinancialAccount__c") %>% 
          modify_at(c("sf__Id"), as.character)
        
        errors <- reprocess_response %>% 
          filter(!is.na(sf__Error)) %>% 
          select(-c(sf__Id, sf__Created, sf__Error))
        
        successes <- reprocess_response %>% 
          filter(is.na(sf__Error))
        
        current_response <- current_response %>% 
          mutate(FinServ__FinancialAccountNumber__c, as.character(FinServ__FinancialAccountNumber__c)) %>% 
          bind_rows(successes)
        
        count <- count + 1
        
      }
      
      current_response <- current_response %>% 
        mutate(FinServ__FinancialAccountNumber__c = as.character(FinServ__FinancialAccountNumber__c))
      
      current_response
      
    })
  
  
  
  accounts_to_zero_out <- sf_query("select Id,
                                         FinServ__CashBalance__c
                                       from FinServ__FinancialAccount__c 
                                       where Mins_Since_Modified__c > 120") %>%
    filter(FinServ__CashBalance__c != 0) %>% 
    mutate(FinServ__CashBalance__c = 0)
  
  if(nrow(accounts_to_zero_out) > 0){
    
    zero_out_response <- accounts_to_zero_out %>% 
      salesforcer::sf_update(object_name = "FinServ__FinancialAccount__c",
                             api_type = "Bulk 2.0")
    
  }
  
  
  # holdings -----------------------------------------------------------------------------------------------
  
  sf_financial_holdings_to_upsert <- financial_holding_data %>% 
    left_join(sf_financial_accounts, by = c('Account_Number__c' = 'FinServ__FinancialAccountNumber__c')) %>% 
    filter(!is.na(Id)) %>% 
    filter(!is.na(External_Id__c)) %>% 
    rename('FinServ__FinancialAccount__c' = 'Id')
  
  
  upsert_response <- sf_financial_holdings_to_upsert %>% 
    split(f = rep(1:ceiling(nrow(sf_financial_holdings_to_upsert) / 5000), each = 5000)[1:nrow(sf_financial_holdings_to_upsert)]) %>% 
    map_dfr(~{
      
      current_response <- salesforcer::sf_upsert(.x, api_type = "Bulk 2.0",
                                                 object_name = "FinServ__FinancialHolding__c",
                                                 external_id_fieldname = "External_Id__c") %>% 
        modify_at(c('Account_Number__c', 'sf__Id'), as.character)
      
      
      errors <- current_response %>%
        filter(!is.na(sf__Error)) %>% 
        select(-c(sf__Id, sf__Created, sf__Error))
      
      current_response <- current_response %>% 
        filter(is.na(sf__Error))
      
      count <- 0
      
      while (nrow(errors) > 0 & count < 5) {
        
        reprocess_response <- salesforcer::sf_upsert(errors,  api_type = "Bulk 2.0",
                                                     object_name = "FinServ__FinancialHolding__c",
                                                     external_id_fieldname = "External_Id__c") %>% 
          modify_at(c('Account_Number__c', 'sf__Id'), as.character)
        
        errors <- reprocess_response %>% 
          filter(!is.na(sf__Error)) %>% 
          select(-c(sf__Id, sf__Created, sf__Error))
        
        successes <- reprocess_response %>% 
          filter(is.na(sf__Error))
        
        current_response <- current_response %>% 
          bind_rows(successes)
        
        count <- count + 1
        
      }
      
      current_response
      
    })
  
  
  holdings_to_delete <- sf_query("select Id
                                       from FinServ__FinancialHolding__c 
                                       where LastModifiedDate < TODAY") 
  if(nrow(holdings_to_delete) > 0){
    
    to_delete_response <- holdings_to_delete %>% 
      salesforcer::sf_delete(object_name = "FinServ__FinancialHolding__c",
                             api_type = "Bulk 2.0")
    
  }
  
  salesforce_update_status <- 'TRUE'
  
  readr::write_file(salesforce_update_status, file = 'data/salesforce_updated')
  
}


