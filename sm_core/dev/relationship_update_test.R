source('utils.R')

relationship_body <- c('{
          "name": "Relationship, Test", 
          "displayName": "Test Relationship",
          "createMasterPortfolio": false,
          "masterPortfolioID": "z058088711",
          "masterPortfolioName": "Lindsay Demo"
        }')

relationship_response <- httr::content(httr::POST(str_c(bd_api$call_url, '/v1/relationship/'),
                                                  auth_header, body = relationship_body, encode='form'))

portfolio_group_response <- httr::content(httr::PUT(str_c(bd_api$call_url, '/v1/portfoliogroup/z05274181/portfolio/z058088711'),
                                                    auth_header, body = '', encode='form'))