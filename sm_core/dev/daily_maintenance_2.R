
source('utils.R')


batch_date <- get_latest_settlement_date(lubridate::today())

bd_accounts <- load_black_diamond_accounts(batch_date)

bd_portfolios <- load_black_diamond_portfolios(batch_date)

unassigned_accounts <- load_black_diamond_unassigned_accounts(batch_date)

words_to_remove <- c('llc', 'revocable', 'trust', 'cust',
                     'for', 'rev', 'co', 'tt', 'sr', 'jr', 'and', 'estate',
                     'ttee', 'ira', 'inh', 'char', 'rem', 'unitrust', 'family',
                     'the', 'tr', '401K')

portfolio_groups <- httr::content(httr::GET(str_c(bd_api$call_url, '/v1/portfoliogroup'), auth_header, body = '', encode='form')) %>% 
  enframe() %>% 
  select(value) %>% 
  unnest_wider(value)


rep_codes <- salesforcer::sf_run_report('00O2M0000099eO0UAI') %>% 
  distinct() %>% 
  clean_names()

rep_codes_1 <- salesforcer::sf_run_report('00O2M0000099kPnUAI') %>%
  distinct() %>%
  clean_names()

rep_codes_new <- rep_codes_1 %>%
  rename(rep_codes_id = rep_codes_rep_codes_id)

mutated_rep_codes <- merge(rep_codes, rep_codes_new, by = "rep_codes_id")

teams_list <- httr::content(httr::GET(str_c(bd_api$call_url, '/v1/team'), auth_header, body = '', encode='form')) %>%
  enframe() %>%
  select(value) %>%
  unnest_wider(value)


accounts <- bd_accounts %>% 
  clean_names() %>% 
  filter(id %in% unassigned_accounts) %>% 
  select(id, account_number, custodial_account_name, account_registration_type, custodian, teams_by_rep_code) %>% 
  unnest(teams_by_rep_code, keep_empty = TRUE) %>% 
  rename(team_name = Name, bd_account_id = id) %>% 
  clean_names() %>%
  filter(custodian == "Fidelity IWS" | custodian == "Schwab" | custodian == "TD Ameritrade") %>% 
  select(-c(account_id, rep_code_id, team_name)) %>% 
  modify_at(c("custodial_account_name"), str_to_lower) %>% 
  modify_at(c("custodial_account_name"), tm::removeWords, words_to_remove) %>% 
  modify_at(c("portfolio_name"), str_replace_all, coll(' ,'), ',') %>% 
  modify_at(c("custodial_account_name"), str_remove_all, coll(',')) %>% 
  modify_at(c("custodial_account_name"), str_remove_all, coll('&')) %>% 
  modify_at(c("custodial_account_name"), str_trim) %>% 
  modify_at(c("custodial_account_name"), str_squish) %>% 
  modify_at(c("custodial_account_name"), str_to_lower) %>% 
  rowwise() %>% 
  mutate(last_name = last(first(str_split(custodial_account_name, ' '))),
         first_name = first(first(str_split(custodial_account_name, ' '))),
         portfolio_name = str_c(last_name, ", ", first_name),
         display_name = if_else(!is.na(account_registration_type), str_c(str_to_title(first_name), str_to_title(last_name), account_registration_type, sep = ' '), 
                 str_to_title(str_c(first_name, last_name, sep = ' ')))) %>% 
  filter(!is.na(team_id))


portfolios <- bd_portfolios %>% 
  clean_names() %>% 
  select(id, name, teams) %>% 
  rename(portfolio_name = name, bd_portfolio_id = id) %>% 
  unnest(teams) %>% 
  rename(team_name = Name) %>% 
  clean_names() %>% 
  select(-c(account_id, team_name)) %>% 
  filter(team_id %in% accounts$team_id) %>% 
  modify_at(c("portfolio_name"), str_to_lower) %>% 
  modify_at(c("portfolio_name"), tm::removeWords, words_to_remove) %>% 
  modify_at(c("portfolio_name"), str_replace_all, coll(' ,'), ',') %>% 
  modify_at(c("custodial_account_name"), str_remove_all, coll('&')) %>% 
  modify_at(c("portfolio_name"), str_trim) %>% 
  modify_at(c("portfolio_name"), str_squish)


# search for portfolios
account_match <- accounts %>% 
  group_by(team_id) %>% 
  group_split() %>% 
  map_dfr(~{
    
    possible_match_portfolios <- portfolios %>% 
      filter(team_id == first(.x$team_id)) %>% 
      select(-team_id)
    
    accounts_with_portfolio <- .x %>% 
      mutate(match_portfolio = possible_match_portfolios$portfolio_name[amatch(portfolio_name, possible_match_portfolios$portfolio_name,
                                                                               method = c('jw'))],
             bd_portfolio_id = possible_match_portfolios$bd_portfolio_id[amatch(portfolio_name, possible_match_portfolios$portfolio_name,
                                                                                method = c('jw'))])
    
    accounts_with_portfolio
    
  })


# update display name
account_bd_response <- accounts %>% 
  pmap(function(...) {
    
    current <- tibble(...)
    
    account_body <- glue::glue('{
          "displayName": "<<current$display_name>>"
        }', .open = "<<", .close = ">>")
    
    httr::PUT(str_c(bd_api$call_url, '/v1/account/', current$bd_account_id, '/displayName'), 
              auth_header, body = account_body, encode='form')
    
  })


# add accounts with portfolio match to given portfolios
accounts_with_portfolio <- account_match %>% 
  filter(!is.na(bd_portfolio_id)) %>% 
  group_by(bd_portfolio_id) %>% 
  group_split() %>% 
  map(~{
    
    portfolio_body <- glue::glue('{
          "accountIds": <<jsonlite::toJSON(.x$bd_account_id)>>
        }', .open = "<<", .close = ">>")
    
    httr::PUT(str_c(bd_api$call_url, '/v1/portfolio/', first(.x$bd_portfolio_id), '/account'), 
              auth_header, body = portfolio_body, encode='form')
    
  })


rep_codes <- salesforcer::sf_run_report('00O2M0000099eO0UAI') %>% 
  distinct() %>% 
  clean_names()

rep_codes_1 <- salesforcer::sf_run_report('00O2M0000099kPnUAI') %>%
  distinct() %>%
  clean_names()

rep_codes_new <- rep_codes_1 %>%
  rename(rep_codes_id = rep_codes_rep_codes_id)

mutated_rep_codes <- merge(rep_codes, rep_codes_new, by = "rep_codes_id")

teams_list <- httr::content(httr::GET(str_c(bd_api$call_url, '/v1/team'), auth_header, body = '', encode='form')) %>%
  enframe() %>%
  select(value) %>%
  unnest_wider(value)

# create portfolio & relationship for accounts without a portfolio match
accounts_without_portfolio <- account_match %>% 
  filter(is.na(bd_portfolio_id)) %>% 
  mutate(portfolio_display_name = str_to_title(str_c(first_name, last_name, sep = ' ')),
         portfolio_name = str_to_title(portfolio_name)) %>% 
  group_by(portfolio_name) %>% 
  group_split() %>% 
  map(~{
    
    current <- tibble(...)
    
    cur_rep_code <- current$rep_code_name
    
    print(cur_rep_code)

    portfolio_team <- rep_codes_new %>%
      filter(rep_codes_id %in% cur_rep_code) %>%
      select(financial_advisor_firm_lookup_account_name) %>%
      distinct() %>%
      pull(financial_advisor_firm_lookup_account_name)

    print(portfolio_team)

    bd_team_id <- teams_list %>%
      filter(description %in% portfolio_team) %>%
      select(id, name) %>%
      distinct() %>%
      pull(id)

    print(bd_team_id)

    # name <- first(.x$portfolio_name)
    # display_name <- first(.x$portfolio_display_name)
    # 
    # print(display_name)
    # print(cur_rep_code)
    # 
    # portfolio_body <- glue::glue('{
    #       "name": "<<name>>",
    #       "displayName": "<<display_name>>",
    #       "accountIds": <<jsonlite::toJSON(.x$bd_account_id)>>
    #     }', .open = "<<", .close = ">>")
    # 
    # portfolio_response <- httr::content(httr::POST(str_c(bd_api$call_url, '/v1/portfolio/'),
    #                                                auth_header, body = portfolio_body, encode='form'))
    # 
    # portfolio_id <- first(portfolio_response$id)
    # 
    # update_port_team <- glue::glue('{
    #         "name": "<<current$portfolio_name>>",
    #         "displayName": "<<current$portfolio_display_name>>",
    #         "teamID": "<<bd_team_id>>"
    #         }', .open = "<<", .close = ">>")
    # 
    # portfolio_response <- httr::content(httr::PUT(str_c(bd_api$call_url, '/v1/portfolio/', portfolio_id),
    #                                               auth_header, body = update_port_team, encode='form'))
    # 
    # relationship_body <- glue::glue('{
    #       "name": "<<name>>",
    #       "displayName": "<<display_name>>",
    #       "createMasterPortfolio": true,
    #       "masterPortfolioID": "<<portfolio_id>>",
    #       "masterPortfolioName": "<<display_name>>",
    #     }', .open = "<<", .close = ">>")
    # 
    # relationship_response <- httr::content(httr::POST(str_c(bd_api$call_url, '/v1/relationship/'),
    #                                                   auth_header, body = relationship_body, encode='form'))

    # update_cr_team <- glue::glue('{
    #       "name": "<<name>>",
    #       "teamIDs": "<<bd_team_id>>",
    #     }', .open = "<<", .close = ">>")
    #
    # relationship_response <- httr::content(httr::PUT(str_c(bd_api$call_url, '/v1/relationship/'),
    #                                                   auth_header, body = update_cr_team, encode='form'))

})


