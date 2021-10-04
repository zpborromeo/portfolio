# Test Portfolio Groups

source('utils.R')

portfolio_group_response <- httr::content(httr::PUT(str_c(bd_api$call_url, '/v1/portfoliogroup/359961/portfolio/z057594421'),
                                                    auth_header, body = '', encode='form'))
