# Author: Zach Borromeo
# Start Date: July 15, 2021
# Last Modified Date: July 15, 2021

# Functionality: To mass create a large number of client users based on list of
# data provided by financial advisors.

team_ID <- httr::content(httr::GET('https://api.bdreporting.com/v1/team', auth_header, body = "", encode='form'))

clean_team_ID <- team_ID %>% 
  enframe() %>% 
  select(value) %>% 
  unnest_wider(value)

profile_ID <- httr::content(httr::GET('https://api.bdreporting.com/v1/client/profiles', auth_header, body = "", encode='form'))

clean_profile_ID <- profile_ID %>% 
  enframe() %>% 
  select (value) %>% 
  unnest_wider(value)

source('utils.R')

#Pull Data from Excel Document

clientData <- readxl::read_excel("user_batch.xlsx")


bd_response <- clientData %>% 
  pmap(function(...) {
    
    current <- tibble(...)
    
    client_user <- glue::glue('{
    "userName": "<<current$userName>>",
    "firstName": "<<current$firstName>>",
    "lastName": "<<current$lastName>>",
    "email": "<<current$email>>",
    "teamId": "<<current$teamID>>",
    "relationshipIds": ["<<current$clientID>>"],
    "profileId": "<<current$profileID>>"}', .open = "<<", .close = ">>")
    
    print(client_user)
    
    httr::content(httr::POST(str_c(bd_api$call_url, '/v1/client/'), auth_header, body = client_user, encode='form'))
    
  })
