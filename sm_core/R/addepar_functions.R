
get_addepar_attributes <- function(clean_accounts, custodian_by_account){
  
  custodian_by_account <- custodian_by_account %>% 
    rename(billing_account_custodian = custodian)
  
  attributes <- clean_accounts %>% 
    select(account_number, account_name,
           account_registration_type, manager,
           advisor_name, team, data_provider, style,
           custodian, billing_number, supervised, billable,
           external_billing_account_id, tax_status, payout_name, 
           aum_type, first_name, last_name, city, state, zip,
           line1, line2, line3, fee_schedule_name) %>% 
    mutate(tax_status = str_c(tax_status, "_status")) %>% 
    mutate(tax_status = if_else(tax_status == "1_status", "Taxable", tax_status)) %>% 
    mutate(tax_status = ifelse(tax_status == "2_status", "Non Taxable", tax_status)) %>% 
    mutate(tax_status = ifelse(tax_status == "3_status", "Tax Deferred", tax_status)) %>% 
    mutate(billing_account_number = ifelse(is.na(billing_number), account_number, billing_number)) %>% 
    left_join(custodian_by_account, by = c("external_billing_account_id" = "id")) %>% 
    mutate(billing_account_custodian = ifelse(is.na(billing_account_custodian), custodian, billing_account_custodian)) %>% 
    modify_at(c("line1", "line2", "line3", "first_name", "last_name", "city"), str_to_title) %>% 
    mutate(unsupervised = if_else(supervised == TRUE, "No", 'Yes')) %>% 
    mutate(nonbillable = if_else(billable == TRUE, "No", 'Yes')) %>% 
    mutate(sm_traded = if_else(str_sub(style, 1, 2) == "SM", "Yes", 'No')) %>% 
    mutate(advisor_choice = if_else(str_sub(style, 1, 2) == "AC", "Yes", 'No')) %>% 
    mutate(address = if_else(line2 == '' & line3 == '', line1, ""),
           address = if_else(line2 != '' & line3 != '' & line1 == '', str_c(line2, line3, sep = ", "), address),
           address = if_else(line2 != '' & line3 == '' & line1 == '', line2, address),
           address = if_else(line2 != '' & line3 == '' & line1 != '', str_c(line1, line2, sep = ", "), address),
           address = if_else(line2 != '' & line3 != '' & line1 != '', str_c(line1, line2, line3, sep = ", "), address)) %>% 
    select(-c(payout_name, billing_number, external_billing_account_id, line1, line2, line3, supervised, billable)) %>% 
    distinct() %>% 
    map_dfc(~{if_else(is.na(.x), "", .x)})
    
    
  
  attributes
  
}


get_custodian_by_account <- function(bd_accounts){
  
  custodian_by_account <- bd_accounts %>% 
    select(id, custodian) %>% 
    distinct()
  
  custodian_by_account
  
}


get_addepar_accounts <- function(addepar_api, addepar_header){
  
  
  raw_account_response <- httr::content(httr::GET(str_c(addepar_api$call_url, '/v1/entities?filter[entity_types]=FINANCIAL_ACCOUNT'), addepar_header, body = ''))
  
  account_response <- jsonlite::fromJSON(rawToChar(raw_account_response))
  
  accounts <- account_response$data
  
  
  pagination_link <- first(account_response$links)
  
  
  while (!is.null(pagination_link)) {
    
    raw <- httr::content(httr::GET(str_c(addepar_api$call_url, pagination_link, "&filter[entity_types]=FINANCIAL_ACCOUNT"), addepar_header, body = ''))
    
    current_response <- jsonlite::fromJSON(rawToChar(raw))
    
    current_accounts <- current_response$data
    pagination_link <- first(current_response$links)
    
    if(nrow(current_accounts) > 0){
      
      accounts <- accounts %>% 
        bind_rows(current_accounts)
    }
    
  }
  
  accounts
  
}

