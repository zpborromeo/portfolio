# Institutional investment managers must submit Form 13F to the SEC on a quarterly basis. 
# Per the U.S. Securities and Exchange Commission ( www.sec.gov): “In general, an institutional investment manager is: 
# (1) an entity that invests in, or buys and sells, securities for its own account; or 
# (2) a natural person or an entity that exercises investment discretion over the account of any other natural person or entity. 
# Institutional investment managers can include investment advisers, banks, insurance companies, broker-dealers, pension funds, and corporation.”
# 
# The 13F report will contain all qualified discretionary securities under management with a total of at least $100 million in value. 
# “The securities that institutional investment managers must report on Form 13F are ‘section 13(f) securities.’ 
# Section 13(f) securities generally include equity securities that trade on an exchange, certain equity options and warrants, 
# shares of closed-end investment companies, and certain convertible debt securities.” 
# Mutual Funds, individual bonds, and money market funds are not Section 13(f) securities. Along with that, the SEC excludes minimum positions, 
# which are any positions that are fewer than 10,000 shares and less than $200,000 in total market value.
# 
# The SEC EDGAR database is updated quarterly with eligible 13F securities and that list can be found on the SEC website. 
# A complete 13F report needs to be filed within 45 calendar days of a quarter-end.



source('utils.R')




sec_13f_current <- SEC_13F_list() %>% 
  pull(CUSIP)


sec_13F <- bd_accounts %>% 
  rename(account_name = Name) %>% 
  unnest(Team, keep_empty = TRUE) %>% 
  rename(team = Name) %>% 
  select(team, Custodian, Holdings) %>% 
  unnest(Holdings) %>% 
  filter(AsOfDate == '2021-06-30T00:00:00' & Discretionary == TRUE) %>% 
  filter(!(team %in% rias)) %>% 
  filter(!(team == "Lighthouse Wealth Strategies" & Custodian == 'TD Ameritrade')) %>% 
  clean_names() %>% 
  select(asset_name, provider_issue_type, display_cusip, investment_discretion, units, market_value) %>% 
  group_by(asset_name, provider_issue_type) %>% 
  summarise(cusip = first(display_cusip),
            shares = round(sum(units), digits = 0),
            value = round(sum(market_value) / 1000, digits = 0),
            sole = shares) %>% 
  mutate(options_put_call = ifelse(provider_issue_type  == "Call", "Call", NA),
         options_put_call = ifelse(provider_issue_type  == "Put", "Put", options_put_call),
         provider_issue_type = ifelse(provider_issue_type  %in% c("Put", "Call"), NA, provider_issue_type),
         sh_prn = "SH",
         other_managers = NA,
         shared = 0,
         none = 0,
         investment_discretion = 'SOLE',
         asset_name = str_replace(asset_name, coll("+"), "-"),
         provider_issue_type = ifelse(str_detect(asset_name, coll("COM")), "Stock", provider_issue_type)) %>% 
  select(asset_name, provider_issue_type, cusip,
         value, shares, sh_prn, options_put_call, investment_discretion,
         other_managers, sole, shared, none) %>% 
  filter(shares >= 10000 & value >= 200) %>% 
  filter(cusip %in% sec_13f_current) %>% 
  rename("Name of Issuer" = asset_name, "Title of Class" = provider_issue_type,
         "CUSIP No" = cusip, "Value (x$1000)" = value, "Shares or Principal Amount" = shares,
         "Shares/Principal" = sh_prn, "Put/Call" = options_put_call, 
         "Investment Discretion" = investment_discretion, "Other Managers" = other_managers,
         Sole = sole, Shared = shared, None = none)
  
openxlsx::write.xlsx(sec_13F, file = "sowell_13f_q2_2021.xlsx", overwrite = TRUE)

