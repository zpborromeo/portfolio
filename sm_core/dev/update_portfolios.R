source('utils.R')

update_portfolios <- readxl::read_excel("Excel_Files/update_portfolios.xlsx") %>% 
  clean_names()

update_response <- update_portfolios %>% 
  pmap(function(...){
    
    current <- tibble(...)
    
    update_port_team <- glue::glue('{
            "name": "<<current$portfolio_name>>",
            "displayName": "<<current$portfolio_display_name>>",
            "teamID": "<<current$team_id>>"
            }', .open = "<<", .close = ">>")

    portfolio_response <- httr::content(httr::PUT(str_c(bd_api$call_url, '/v1/portfolio/', current$portfolio_id), auth_header, body = update_port_team, encode='form'))

  })