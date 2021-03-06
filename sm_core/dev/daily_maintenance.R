
source('utils.R')


batch_date <- get_latest_settlement_date(lubridate::today())

bd_accounts <- load_black_diamond_accounts(batch_date)

bd_portfolios <- load_black_diamond_portfolios(batch_date)

unassigned_accounts <- load_black_diamond_unassigned_accounts(batch_date)

words_to_remove <- c('llc', 'revocable', 'trust', 'cust',
                     'for', 'rev', 'co', 'tt', 'sr', 'jr', 'and')

portfolio_groups <- httr::content(httr::GET(str_c(bd_api$call_url, '/v1/portfoliogroup'), auth_header, body = '', encode='form')) %>% 
  enframe() %>% 
  select(value) %>% 
  unnest_wider(value)


rep_codes <- salesforcer::sf_run_report('00O2M0000099eO0UAI') %>% 
  distinct() %>% 
  clean_names()


accounts <- bd_accounts %>% 
  clean_names() %>% 
  filter(id %in% unassigned_accounts) %>% 
  select(id, account_number, custodial_account_name, account_registration_type, custodian, teams_by_rep_code) %>% 
  unnest(teams_by_rep_code, keep_empty = TRUE) %>% 
  rename(team_name = Name, bd_account_id = id) %>% 
  clean_names() %>% 
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
         display_name = str_to_title(str_c(first_name, last_name, account_registration_type, sep = ' '))) %>% 
  filter(!is.na(account_registration_type)) %>% 
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


# create portfolio & relationship for accounts without a portfolio match
accounts_without_portfolio <- account_match %>% 
  filter(is.na(bd_portfolio_id)) %>% 
  mutate(portfolio_display_name = str_to_title(str_c(first_name, last_name, sep = ' ')),
         portfolio_name = str_to_title(portfolio_name)) %>% 
  group_by(portfolio_name) %>% 
  group_split() %>% 
  map(~{
    
    name <- first(.x$portfolio_name)
    display_name <- first(.x$portfolio_display_name)
    
    portfolio_body <- glue::glue('{
          "name": "<<name>>",
          "displayName": "display_name",
          "accountIds": <<jsonlite::toJSON(.x$bd_account_id)>>
        }', .open = "<<", .close = ">>")
    
    portfolio_response <- httr::content(httr::POST(str_c(bd_api$call_url, '/v1/portfolio/'),
                                                   auth_header, body = portfolio_body, encode='form'))

    portfolio_id <- first(portfolio_response$id)

    relationship_body <- glue::glue('{
          "name": "<<name>>",
          "displayName": "<<display_name>>",
          "createMasterPortfolio": true,
          "masterPortfolioID": "<<portfolio_id>>",
          "masterPortfolioName": "<<display_name>>",
        }', .open = "<<", .close = ">>")

    relationship_response <- httr::content(httr::POST(str_c(bd_api$call_url, '/v1/relationship/'),
                                                      auth_header, body = relationship_body, encode='form'))


    # portfolio_body <- glue::glue('{
    #       "portfolioIds": <<jsonlite::toJSON(portfolio_id)>>
    #     }', .open = "<<", .close = ">>")
    # 
    # portfolio_rep_codes <- unique(.x$rep_code_name)
    # 
    # portfolio_group <- rep_codes %>%
    #   filter(rep_codes_id %in% portfolio_rep_codes) %>%
    #   select(financial_advisor_lookup_full_name) %>%
    #   distinct() %>%
    #   pull(financial_advisor_lookup_full_name)
    # 
    # portfolio_group_id <- first(portfolio_groups$id[amatch(portfolio_group, portfolio_groups$name, method = c('jw'))])
    # 
    # if(!is.na(portfolio_group_id)){
    # 
    #   portfolio_group_response <- httr::content(httr::POST(str_c(bd_api$call_url, '/v1/portfoliogroup/', portfolio_group_id, '/portfolio'),
    #                                                        auth_header, body = portfolio_body, encode='form'))
    # 
    # }
    
  })


