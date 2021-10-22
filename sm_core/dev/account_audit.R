source('utils.R')


library(httr)
library(tidyverse)
library(janitor)
library(fs)
library(openxlsx)


# api creds
addepar_api <- list(
  
  api_id = 'f77e4420-9eaa-4654-8bc0-c2d2c55688a5',
  api_secret = 'irkFj1l7d24rLZleqFTDlcnZvjDYEfOV9melAwKv',
  call_url = 'https://sowell.addepar.com/api'
  
)

# function to call below
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


base_64_auth <- base64enc::base64encode(charToRaw(str_c(addepar_api$api_id, ':', addepar_api$api_secret)))

addepar_header <- httr::add_headers('Authorization' = str_c("Basic", base_64_auth, sep = ' '),
                                    'Content-Type' = 'application/vnd.api+json', 
                                    'Accept' = 'application/vnd.api+json', 'addepar-firm' = '717')


accounts <- get_addepar_accounts(addepar_api, addepar_header)

addepar_accounts <- accounts %>% 
  jsonlite::flatten() %>%
  select(id, attributes.account_number) %>% 
  rename(account_number = attributes.account_number)

batch_date <- get_latest_settlement_date(lubridate::today())

bd_accounts <- load_black_diamond_accounts(batch_date) 

clean_accounts <- bd_accounts %>% 
  clean_names()


bd_accounts_by_team <- clean_accounts %>% 
  select(account_number, teams_by_rep_code, custodian, tags) %>% 
  unnest(teams_by_rep_code, keep_empty = TRUE) %>% 
  select(-c(TeamId, AccountId, RepCodeId)) %>% 
  rename(team = Name) %>% 
  clean_names() %>% 
  distinct() %>% 
  filter(!is.na(team)) %>% 
  filter(str_detect(custodian, coll("TD Ameritrade")) |
           str_detect(custodian, coll("Schwab")) |
           str_detect(custodian, coll("Fidelity"))) %>% 
  filter(!str_detect(team, 'ZZZ')) %>% 
  filter((!(team %in% c("10x Wealth Advisors", "Evidence Based Advisors", "Lighthouse Wealth Strategies")))) %>% 
  rowwise() %>% 
  filter(!str_detect(str_trim(str_squish(tags$Value[tags$Name == "Payout Name"])), "EBA")) %>% 
  ungroup()


missing_summary <- bd_accounts_by_team %>% 
  filter(!(account_number %in% addepar_accounts$account_number)) %>% 
  group_by(team, custodian) %>% 
  summarise(number_missing_accounts = n(),
            missing_rep_codes = str_c(unique(rep_code_name), collapse = ", "))
  

missing_accounts <- bd_accounts_by_team %>% 
  filter(!(account_number %in% addepar_accounts$account_number))

wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "summary")
openxlsx::addWorksheet(wb, "accounts")

openxlsx::writeData(wb, "summary", missing_summary)
openxlsx::writeData(wb, "accounts", missing_accounts)

openxlsx::saveWorkbook(wb, file = "addepar_missing_act_rep_codes.xlsx", overwrite = TRUE)
