remove(list = ls())

library(httr)
library(tidyverse)
library(salesforcer)
library(DBI)


# salesforce get rep codes
sf_auth()


read_obj_result <- sf_read_metadata(metadata_type='CustomObject',
                                    object_names=c('Model_Selection__c'))

rep_code_query <- glue::glue("SELECT Name,
                           Financial_Advisor_Firm_Lookup__c,
                           Financial_Advisor_Lookup__c,
                           Rep_Code_Custodian__c,
                           Advisor_Type__c,
                           Custodian_Pricing__c,
                           Data_Provider__c,
                           Status__c
                    FROM Rep_Codes__c")


rep_codes <- sf_query(rep_code_query)



rep_code_query <- glue::glue("SELECT Model_Selection__c
                    FROM Case")


rep_codes <- sf_query(rep_code_query)





# Connect to a specific postgres database i.e. Heroku
con <- dbConnect(RPostgres::Postgres(),dbname = 'core', 
                 host = 'core-db-postgresql-do-user-9269326-0.b.db.ondigitalocean.com', # i.e. 'ec2-54-83-201-96.compute-1.amazonaws.com'
                 port = 25060, # or any other port specified by your DBA
                 user = 'doadmin',
                 password = 'eimpjxnf0cf5fujz')



# black diamond api variables
api_id <- '1959Sowell_Api'
api_secret <- '/PsEKEu+CzbRatL6Qd4mCmY5ySa8QBhkC2OkVxfeK6e8ZFUQLlmYBBgOxDNLoqMmi5ST23yrvI/VcvTSFczeBQ=='
black_diamond_username <- 'BWalden'
black_diamond_password <- 'Pluto18!'

# api urls
authorization_url <- 'https://api.bdreporting.com/issue/oauth2/token'

batch_url <- 'https://api.bdreporting.com/batch'

authentication_body <- list(grant_type = 'password',
                           client_id = api_id,
                           client_secret = api_secret,
                           username = black_diamond_username,
                           password = black_diamond_password)

token <- httr::content(httr::POST(authorization_url, body = authentication_body, encode='form'))

auth_header <- httr::add_headers('Authorization' = paste('bearer', token$access_token, sep = ' '),
                                 'Content-Type' = 'application/json')





# one off calls
accounts <- httr::content(httr::GET('https://api.bdreporting.com/v1/account?page=1&pageSize=max', auth_header, body = "", encode='form'))
account2 <- httr::content(httr::GET('https://api.bdreporting.com/v1/account?page=2&pageSize=max', auth_header, body = "", encode='form'))
account3 <- httr::content(httr::GET('https://api.bdreporting.com/v1/account?page=3&pageSize=max', auth_header, body = "", encode='form'))
account4 <- httr::content(httr::GET('https://api.bdreporting.com/v1/account?page=4&pageSize=max', auth_header, body = "", encode='form'))
account5 <- httr::content(httr::GET('https://api.bdreporting.com/v1/account?page=5&pageSize=max', auth_header, body = "", encode='form'))



styles_import <- enframe(styles) %>% 
  select(value) %>% 
  unnest_wider(value) %>% 
  select(id, name) %>% 
  rename(style_name = name, black_diamond_id = id)



dbWriteTable(con, "style", styles_import, append = TRUE)


report_test <- httr::content(httr::GET('https://api.bdreporting.com/v1/external/report/319054', auth_header, body = "", encode='form'))



test <- httr::content(httr::GET('https://api.bdreporting.com/v1/account/925853001', auth_header, body = "", encode='form'))



x <- as_tibble(unlist(test))


statuses <- 

status <- enframe(test) %>% 
  slice(n()) %>% 
  select(value) %>% 
  unnest_wider(value)
  
  unnest_wider(value) %>% 
  select(id, name) %>% 
  rename(style_name = name, black_diamond_id = id)




