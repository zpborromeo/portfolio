
get_clean_accounts <- function(bd_accounts, portfolios,
                               portfolio_groups, portfolio_fee_schedules){
  
  clean_accounts <- bd_accounts %>% 
    clean_names() %>% 
    unnest(team, keep_empty = TRUE) %>% 
    rename(team = Name, account_name = name) %>% 
    select(-c(AccountId, TeamId)) %>% 
    unnest(teams_by_rep_code, keep_empty = TRUE) %>% 
    rename(team_by_rep_code = Name) %>% 
    mutate(team_by_rep_code = if_else(is.na(team_by_rep_code), team, team_by_rep_code),
           team = team_by_rep_code,
           team = if_else(team == '*Sowell', "Sowell Management", team)) %>% 
    unnest(style, keep_empty = TRUE) %>% 
    unnest(style, keep_empty = TRUE) %>% 
    unnest_wider(address) %>%
    select(-Name) %>% 
    unnest(fee_schedules, keep_empty = TRUE) %>% 
    rename(fee_schedule_name = Name) %>% 
    left_join(portfolio_fee_schedules, by = c("id" = "account_id")) %>% 
    left_join(portfolios, by = c("id" = "account_id")) %>% 
    left_join(portfolio_groups, by = c("id" = "account_id")) %>% 
    mutate(long_number = str_remove(long_number, coll("MngdPortfolio")),
           account_number = long_number,
           fee_schedule_name = if_else(is.na(fee_schedule_name), portfolio_fee_schedule_name, fee_schedule_name)) %>% 
    rowwise() %>% 
    mutate(aum_type = tags$Value[tags$Name == "AUM Type"],
           payout_name = str_trim(str_squish(tags$Value[tags$Name == "Payout Name"])),
           advisor_name = str_trim(str_squish(tags$Value[tags$Name == "Advisor Name"])),
           advisor_name = if_else(is.na(advisor_name) | advisor_name == '', portfolio_group, advisor_name),
           manager = str_trim(str_squish(if_else(is.null(manager), "", manager$LastName)))) %>% 
    ungroup() %>% 
    clean_names()
  
  clean_accounts
  
}



get_portfolio_groups <- function(bd_portfolios, bd_portfolio_groups){
  
  portfolio_groups <- bd_portfolios %>% 
    select(Id, AccountIds) %>% 
    unnest(AccountIds) %>% 
    left_join(bd_portfolio_groups, by = c("Id" = "PortfolioIds")) %>% 
    clean_names() %>% 
    select(account_ids, portfolio_group_name) %>% 
    rename(account_id = account_ids) %>% 
    distinct() %>% 
    filter(!is.na(portfolio_group_name)) %>% 
    group_by(account_id) %>% 
    summarise(portfolio_group = str_c(portfolio_group_name, collapse = " | "))
  
  portfolio_groups
  
}

get_portfolios <- function(bd_portfolios){
  
  portfolios <-  bd_portfolios %>% 
    select(AccountIds, Name) %>% 
    unnest(AccountIds) %>% 
    clean_names() %>% 
    rename(account_id = account_ids) %>% 
    distinct() %>% 
    group_by(account_id) %>% 
    summarise(portfolio = str_c(name, collapse = " | "))
  
  portfolios
  
}


get_portfolio_fee_schedules <- function(bd_portfolios){
  
  portfolio_fee_schedules <- bd_portfolios %>% 
    select(AccountIds, FeeSchedules) %>% 
    unnest(FeeSchedules) %>% 
    unnest(AccountIds) %>% 
    clean_names() %>% 
    rename(portfolio_fee_schedule_name = name, account_id = account_ids) %>% 
    select(account_id, portfolio_fee_schedule_name) %>% 
    mutate(portfolio_fee_schedule_name = str_c(portfolio_fee_schedule_name , " - Portfolio Breakpoint"))
  
  portfolio_fee_schedules
  
}



get_bd_batch_data <- function(batch_date){
  
  last_batch <- readr::read_csv('data/last_batch.csv') %>% 
    mutate(date = lubridate::ymd(date))
  
  if(last_batch$date == batch_date & last_batch$status == 'processing'){
    
    binary_zip <- get_batch_data(call_url, auth_header, last_batch$id)
    
    if(length(binary_zip) > 0){
      
      save_batch_data(batch_date, binary_zip)
      
      last_batch %>% 
        mutate(status = 'complete') %>% 
        readr::write_csv(file = 'data/last_batch.csv')
      
      salesforce_update_status <- 'FALSE'
      
      readr::write_file(salesforce_update_status, file = 'data/salesforce_updated')
      
    }
    
  }
  
  
  
  
}

get_msc_tasks <- function(tasks){
  
  msc_tasks <- tasks %>% 
    filter(str_detect(Subject, "MSC case Closed"))
  
  msc_tasks
  
}


build_style_update_accounts <- function(msc_cases, financial_accounts, style_key, bd_account_ids){
  
  accounts_to_update <- msc_cases %>% 
    left_join(financial_accounts, by = c("FinServ__FinancialAccount__c" = "Id")) %>% 
    mutate(match_name = if_else(Model_type__c == "ETF" & str_detect(New_model__c, "EBA"), str_c(New_model__c, " ", Model_type__c), New_model__c)) %>% 
    left_join(style_key, by = c('match_name' = 'salesforce_name')) %>% 
    distinct() %>% 
    filter(!is.na(black_diamond_name)) %>% 
    mutate(FinServ__FinancialAccountNumber__c = as.character(FinServ__FinancialAccountNumber__c)) %>% 
    left_join(bd_account_ids, by = c("FinServ__FinancialAccountNumber__c" = "AccountNumber"))
  
  accounts_to_update
  
}



# get OAuth toaken from BD, create aith header, return auth header to use in subsequent calls
auth_with_black_diamond <- function(bd_api){
  
  authentication_body <- list(grant_type = 'password',
                              client_id = bd_api$api_id,
                              client_secret = bd_api$api_secret,
                              username = bd_api$black_diamond_username,
                              password = bd_api$black_diamond_password)
  
  token <- httr::content(httr::POST(bd_api$authorization_url, 
                                    body = authentication_body, encode='form'))
  
  auth_header <- httr::add_headers('Authorization' = paste('bearer', token$access_token, sep = ' '),
                                   'Content-Type' = 'application/json')
  
  auth_header

}

# get the last trading day from today
# need to incorporate holidays
get_latest_settlement_date <- function(today){
  
  today_weekday <- lubridate::wday(today)
  
  latest_settlement_date <- case_when(
    today_weekday == 1 ~ today - 2,
    today_weekday == 2 ~ today - 3,
    TRUE ~ today - 1)
  
  latest_settlement_date
}


# save batch data from BD, unzip files, and delete zip file
save_batch_data <- function(batch_date, binary_zip){
  
  fs::dir_create(str_c("batch_data/", batch_date))
  writeBin(binary_zip, con = str_c("batch_data/", batch_date, "/daily_batch.zip"))
  zip::unzip(zipfile = str_c("batch_data/", batch_date, "/daily_batch.zip"), 
             exdir = str_c("batch_data/", batch_date))
  fs::file_delete(str_c("batch_data/", batch_date, "/daily_batch.zip"))
  
}


# load BD accounts from a given batch data folder
load_black_diamond_accounts <- function(batch_date){

  accounts <- dir_ls(str_c('batch_data/', batch_date), regexp = "accounts") %>% 
    map_dfr(~{
      
      current <- RcppSimdJson::fload(.x)
      
    })
  
  accounts
  
}

# load BD accounts w/out a portfolio from a given batch data folder
load_black_diamond_unassigned_accounts <- function(batch_date){
  
  portfolio_json <- jsonlite::fromJSON(first(dir_ls(str_c('batch_data/', batch_date), regexp = "portfolio.json")))
  
  unassigned_accounts <- portfolio_json$UnassignedAccountIds
  
  unassigned_accounts
  
}


# load BD portfolios from a given batch data folder
load_black_diamond_portfolios <- function(batch_date){
  
  portfolio_json <- jsonlite::fromJSON(first(dir_ls(str_c('batch_data/', batch_date), regexp = "portfolio.json")))
  
  portfolios <- portfolio_json$Portfolios
  
  portfolios
  
}


# load BD portfolios from a given batch data folder
load_black_diamond_portfolio_groups <- function(batch_date){
  
  portfolio_group_json <- jsonlite::fromJSON(first(dir_ls(str_c('batch_data/', batch_date), regexp = "portfoliogroup.json")))
  
  portfolio_groups <- portfolio_group_json %>% 
    unnest(PortfolioIds, keep_empty = TRUE)
  
  portfolio_groups
  
}


# load BD portfolios from a given batch data folder
load_black_diamond_client_relationships <- function(batch_date){
  
  relationship_json <- jsonlite::fromJSON(first(dir_ls(str_c('batch_data/', batch_date), regexp = "relationship.json")))
  
  relationships <- relationship_json$Relationship %>% 
    select(ID, Name)
  
  relationships_by_account <- relationship_json$ReferencedEntities %>% 
    unnest(Accounts) %>% 
    select(ExternalID, ExternalRelationshipID) %>% 
    left_join(relationships, by = c("ExternalRelationshipID" = "ID")) %>% 
    distinct()
  
  relationships_by_account
  
}


