
source('utils.R')

# salesforce calls
style_case_types <- get_style_case_types()

style_cases <- get_style_cases(style_case_types)

financial_accounts <- get_financial_accounts(style_cases)

adhesion_sf_accounts <- get_sf_adhesion_accounts(style_cases, financial_accounts)

batch_date <- "6-18-2021"

bd_accounts <- load_black_diamond_accounts(batch_date)


# assigns Adhesion to any Adhesion account listed in SF
adhesion_account_ids <- bd_accounts %>% 
  select(Id, AccountNumber, Manager) %>% 
  filter((AccountNumber %in% adhesion_sf_accounts) & (Manager$LastName != "Adhesion")) %>% 
  pull(Id) %>% 
  walk(~{
    
    httr::content(httr::PUT(str_c(bd_api$call_url, '/account/', .x, '/manager'), auth_header, body = adhesion_body, encode='form'))
    
  })




