source('utils.R')

missing_ports <- readxl::read_excel("no_ports_test.xlsx")

ports_named <- missing_ports %>% 
  mutate(portfolio_display_name = str_to_title(str_c(first_name, last_name, sep = ' ')),
  portfolio_name = str_to_title(portfolio_name))
       

portfolio_groups <- httr::content(httr::GET(str_c(bd_api$call_url, '/v1/portfoliogroup'), auth_header, body = '', encode='form')) %>% 
  enframe() %>% 
  select(value) %>% 
  unnest_wider(value)

rep_codes_1 <- salesforcer::sf_run_report('00O2M0000099kPnUAI') %>% 
  distinct() %>% 
  clean_names()

rep_codes_new <- rep_codes_1 %>% 
  rename(rep_codes_id = rep_codes_rep_codes_id)

rep_codes <- salesforcer::sf_run_report('00O2M0000099eO0UAI') %>% 
  distinct() %>% 
  clean_names()

mutated_rep_codes <- merge(rep_codes, rep_codes_new, by = "rep_codes_id")
  

teams_list <- httr::content(httr::GET(str_c(bd_api$call_url, '/v1/team'), auth_header, body = '', encode='form')) %>% 
  enframe() %>% 
  select(value) %>% 
  unnest_wider(value)

bd_response <- ports_named %>% 
  pmap(function(...) {

  current <- tibble(...)
  
  cur_rep_code <- current$rep_code_name
  
  portfolio_team <- rep_codes_new %>% 
    filter(rep_codes_id %in% cur_rep_code) %>% 
    select(financial_advisor_firm_lookup_account_name) %>% 
    distinct() %>% 
    pull(financial_advisor_firm_lookup_account_name)
  
  bd_team_id <- teams_list %>% 
    filter(name %in% portfolio_team) %>% 
    select(id, name) %>% 
    distinct() %>% 
    pull(id)
  
  portfolio_body <- glue::glue('{
            "name": "<<current$portfolio_name>>",
            "displayName": "<<current$portfolio_display_name>>",
          }', .open = "<<", .close = ">>")

  portfolio_response <- httr::content(httr::POST(str_c(bd_api$call_url, '/v1/portfolio/'),
                           auth_header, body = portfolio_body, encode='form'))

  portfolio_id <- first(portfolio_response$id)

  update_port_team <- glue::glue('{
            "name": "<<current$portfolio_name>>",
            "displayName": "<<current$portfolio_display_name>>",
            "teamID": "<<bd_team_id>>"
            }', .open = "<<", .close = ">>")

  portfolio_response <- httr::content(httr::PUT(str_c(bd_api$call_url, '/v1/portfolio/', portfolio_id),
                                               auth_header, body = update_port_team, encode='form'))

   relationship_body <- glue::glue('{
            "name": "<<current$portfolio_name>>",
            "createMasterPortfolio": false,
            "masterPortfolioID": "<<portfolio_id>>",
            "masterPortfolioName": "<<current$portfolio_display_name>>",
            "teamIDs": ["<<bd_team_id>>"]
            }', .open = "<<", .close = ">>")

    relationship_response <- httr::content(httr::POST(str_c(bd_api$call_url, '/v1/relationship/'),
                          auth_header, body = relationship_body, encode='form'))

    # portfolio_group1 <- mutated_rep_codes %>%
    #   filter(rep_codes_id %in% cur_rep_code) %>%
    #   select(financial_advisor_lookup_full_name) %>%
    #   distinct() %>%
    #   pull(financial_advisor_lookup_full_name)
    # 
    # portfolio_group2 <-  mutated_rep_codes %>%
    #   filter(rep_codes_id %in% cur_rep_code) %>%
    #   select(financial_advisor_firm_lookup_account_name) %>%
    #   distinct() %>%
    #   pull(financial_advisor_firm_lookup_account_name)
    # 
    # portfolio_group_id1 <- first(portfolio_groups$id[amatch(portfolio_group1, portfolio_groups$name, method = c('jw'))])
    # portfolio_group_id2 <- first(portfolio_groups$id[amatch(portfolio_group2, portfolio_groups$name, method = c('jw'))])
    # 
    # if (!is.na(portfolio_group_id1)){
    #   portfolio_group_response <- httr::content(httr::PUT(str_c(bd_api$call_url, '/v1/portfoliogroup/', portfolio_group_id1, '/portfolio/', portfolio_id),
    #                                                        auth_header, body = '', encode='form'))
    # }else if (!is.na(portfolio_group_id2)){
    #   portfolio_group_response <- httr::content(httr::PUT(str_c(bd_api$call_url, '/v1/portfoliogroup/', portfolio_group_id2, '/portfolio/', portfolio_id),
    #                                                       auth_header, body = '', encode='form'))
    # }
  
  })
