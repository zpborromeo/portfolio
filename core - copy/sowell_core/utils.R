# load packages, functions, global varibales, etc.



library(httr)
library(tidyverse)
library(salesforcer)
library(DBI)
library(janitor)
library(fs)
library(pool)
library(RPostgres)


for (file in list.files('R')) {
  
  source(stringr::str_c('R/', file))
  
}

# sf_auth()

bd_api <- config::get("bd_api")
core_db <- config::get("core_db")
core_db <- config::get("core_db")
salesforce <- config::get("salesforce")

billing_api_user_id <- salesforce$billing_api_user_id


pool <- pool::dbPool(
  drv = RPostgres::Postgres(),
  dbname = core_db$dbname,
  host = core_db$host,
  user = core_db$user,
  password = core_db$password,
  port = core_db$port
)


auth_header <- auth_with_black_diamond(bd_api)


firms_to_remove <- c("Riverwater Partners")

non_billable_custodians <- c("SMArtX", "Security Benefit Life", "SEI Wealth Advisor")

# bd filters params
data_providers_to_remove <- c("ByAllAccounts", "Quovo")

custodians_to_include <- c('Fidelity FILI', 'Fidelity FPRS', 'Fidelity IWS', 'Allianz',
                           'Charles Schwab', 'Hartford Life', 'Jackson', 'Jackson National Life Insurance Company',
                           'Pershing Advisory Solutions', 'Raymond James & Associates Inc.', 'Schwab',
                           'SEI Wealth Advisor', 'TCA', 'TD Ameritrade', 'Transamerica Life Insurance', 
                           'Nationwide Advisory Solutions', 'AIG Annuities-Variable & Index',
                           'American Funds', 'Fidelity Advisor Funds', 'John Hancock Freedom 529',
                           'Lincoln Financial Group', 'MFS Investment Management', 'Nationwide',
                           'Prudential Annuities', 'Union Bank and Trust')

teams_to_remove <- c('Ausdal Financial', 'Berthel Fischer', 'Cambridge Investments', 'Cambridge Investment',
                     'Centaurus', 'Cetera', 'Chanel Capital', 'Clarus Wealth Advisors',
                     'Corinthians Wealth', 'Decker Retirement Planning, Inc', 'Householder Group',
                     'Independent Financial Partners', 'Innovation Partners', 'Mack Investments',
                     'Madison Avenue', 'Magann Capital', 'Mid Atlantic Financial Mgmt',
                     'Peak Financial', 'Securities America', 'SEMA IV', 'test', 'Test Team',
                     'TLG', 'USA Financial', 'Veritas', 'Wigginton Assoc', 'World Equity Group',
                     'Worrell Advisory', 'ZZZ Closed Accounts', 'ZZZ Inactive Advisors',
                     'ZZZ Inactive Annuity', 'ZZZ Lutman', 'ZZZ Redwood', 'ZZZ Scott Christie',
                     'Element Wealth', 'Riverwater', '*Sowell', 'Riverwater Partners',
                     'ZZZ London House Capital', 'Gaia Capital Management Inc', 
                     'Lighthouse Wealth Strategies', 'ADV-Non-HNW, No Advisor, Schwab IIP')



# batch_date <- lubridate::ymd("2021-07-12")
# bd_accounts <- load_black_diamond_accounts(batch_date)
# 
# 
# 
# # move to separate call once working -------------------------------------------------------------------------------------------
# 
# # batch data for the day
# 
# # request batch
# 
# batch_date <- get_latest_settlement_date(lubridate::today())
# 
# batch_id <- submit_black_diamond_batch_request(auth_header, bd_api$call_url, batch_date)
# 
# 
# # download batch data
# binary_zip <- get_batch_data(call_url, auth_header, batch_id$id)
# 
# save_batch_data(batch_date, binary_zip)





