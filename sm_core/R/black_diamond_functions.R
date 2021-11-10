

# get advisor by account
# need to use advisor name (tag) if present, otherwise use portfolio group
# combination of batch account data & API calls to get port group
get_advisor_by_account <- function(bd_accounts, auth_header, bd_api){
  
  
  
}


# get accounts with no fee schedule that should technically have a fee schedule since they're billable
get_billable_accounts_with_no_fee_schedule <- function(accounts_with_no_fee_schedule, non_billable_custodians, days_back){
  
  cut_off_date <- lubridate::today() - days_back
  
  billable_accounts <- accounts_with_no_fee_schedule %>% 
    filter(!(custodian %in% non_billable_custodians)) %>% 
    filter(start_date >= cut_off_date)
  
  billable_accounts
  
}

# get non-billable accounts to mark billable
get_non_billable_accounts_with_non_fee_schedule <- function(accounts_with_no_fee_schedule, non_billable_custodians, days_back){
  
  cut_off_date <- lubridate::today() - days_back
  
  non_billable_accounts <- accounts_with_no_fee_schedule %>% 
    filter((custodian %in% non_billable_custodians) & billable == TRUE) %>% 
    filter(start_date >= cut_off_date)
  
  non_billable_accounts
  
}

# get BD accounts with no fee schedule (NOT accounts with a fee schedule of 0% or 0% MCA)
get_accounts_with_no_fee_schedule <- function(bd_accounts, firms_to_remove){

  accounts_with_no_fee_schedule <- bd_accounts %>% 
    select(Id, AccountNumber, Name, Holdings, Custodian, FeeSchedules, Team, StartDate, Billable) %>% 
    rowwise() %>% 
    mutate(MarketValue = sum(as.numeric(Holdings$MarketValue, na.rm = TRUE)),
           Team = if_else(is.null(first(Team$Name)), "No Team", first(Team$Name))) %>% 
    filter((length(FeeSchedules) <= 0) & (MarketValue > 0) & !(Team %in% firms_to_remove)) %>% 
    select(-c(Holdings)) %>%
    mutate(StartDate = lubridate::ymd_hms(StartDate)) %>% 
    janitor::clean_names()
 
  accounts_with_no_fee_schedule
  
}


submit_black_diamond_batch_request <- function(auth_header, call_url, batch_date){
  
  batch_body <- paste0('{ "returnDate": ', "\"", batch_date, "\"", ',
                        "includeClosedAccounts": "false"}')
  
  batch_url <- str_c(call_url, '/v4/batch')
  
  batch_id <- httr::content(httr::POST(batch_url, auth_header, body = batch_body, encode='form'))
  
  batch_id
  
}



get_bd_portfolios <- function(call_url, auth_header){
  
  portfolios <- c()
  
  for (i in 1:10) {
    
    call <- str_c(call_url, '/v1/portfolio?page=', i, '&pageSize=max')
    
    response <- httr::content(httr::GET(call, auth_header, body = "", encode='form'))
    
    if(length(response) == 0){
      
      break
      
    }
    
    portfolios <- append(portfolios, response)
    
  }
  
  portfolios
  
}


get_bd_smartx_portfolios <- function(bd_portfolios){
  
  bd_smartx_portfolios <- enframe(bd_portfolios) %>% 
    hoist(value, "id", "name") %>% 
    select(-value) %>% 
    clean_names() %>% 
    filter(str_detect(name, coll("Reporting")))
  
  bd_smartx_portfolios
  
}


# get all black diamond accounts
get_bd_accounts <- function(call_url, auth_header){
  
  accounts <- c()
  
  for (i in 1:10) {
    
    call <- str_c(call_url, '/v1/account?page=', i, '&pageSize=max')
    
    response <- httr::content(httr::GET(call, auth_header, body = "", encode='form'))
    
    if(length(response) == 0){
      
      break
      
    }
    
    accounts <- append(accounts, response)
    
  }
  
  accounts
  
}



get_batch_data <- function(call_url, auth_header, batch_id){
  
  batch_get_url <- str_c(bd_api$call_url,'/v4/batch/', batch_id)
  
  binary_zip <- httr::content(httr::GET(batch_get_url, auth_header))
  
  binary_zip
}

