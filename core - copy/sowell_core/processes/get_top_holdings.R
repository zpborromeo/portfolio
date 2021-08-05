source('utils.R')

scales::dollar_format()
batch_date <- "6-18-2021"

accounts <- load_black_diamond_accounts(batch_date)

holdings <- bind_rows(accounts$Holdings, .id = "column_label")

sec_13F <- holdings %>% 
  filter(ProviderIssueType %in% c("ADR", "CEF", "Stock", "ETF", "REIT", "Convertible", "Call", "Put")) %>% 
  select(AssetName, ProviderIssueType, Cusip, InvestmentDiscretion, Units, MarketValue) %>% 
  group_by(AssetName, ProviderIssueType, Cusip) %>% 
  summarise(InvestmentDiscretion = first(InvestmentDiscretion),
            shares = sum(Units),
            value = sum(MarketValue) / 1000) %>% 
  mutate(firm = "Sowell Management Services",
         options_put_call = if_else(ProviderIssueType  == "Call", "Call", ""),
         options_put_call = if_else(ProviderIssueType  == "Put", "Put", options_put_call),
         ProviderIssueType = if_else(ProviderIssueType  %in% c("Put", "Call"), "", ProviderIssueType),
         sh_prn = "",
         other_managers = "",
         sole = 1,
         shared = 0,
         none = 0) %>% 
  select(firm, AssetName, ProviderIssueType, Cusip,
         value, shares, options_put_call, InvestmentDiscretion,
         sh_prn, other_managers, sole, shared, none)





top_holdings <- holdings %>% 
  select(AssetNameShort, ClassName, SegmentName, IssueType, MarketValue) %>% 
  filter(str_detect(IssueType, "Mutual|Exchange Traded Fund")) %>% 
  group_by(AssetNameShort, IssueType) %>% 
  summarise(TotalMarketValue = sum(MarketValue)) %>% 
  arrange(desc(TotalMarketValue)) %>% 
  head(20) %>% 
  ungroup(AssetNameShort, IssueType) %>% 
  mutate(TotalMarketValue = formattable::currency(TotalMarketValue)) %>% 
  mutate(parent = if_else(str_detect(AssetNameShort, "VANGUARD"), "Vanguard", ""),
         parent = if_else(str_detect(AssetNameShort, "ISHARES"), "BlackRock", parent),
         parent = if_else(str_detect(AssetNameShort, "INVESCO"), "Invesco", parent),
         parent = if_else(str_detect(AssetNameShort, "SPDR"), "State Street", parent),
         parent = if_else(str_detect(AssetNameShort, "PARNASSUS"), "Parnassus Funds", parent),
         
         parent = if_else(str_detect(AssetNameShort, "PROSHARES"), "ProFunds Group", parent),
         parent = if_else(str_detect(AssetNameShort, "DFA"), "Dimensional Fund Advisors", parent)) %>% 
  select(parent, AssetNameShort, IssueType, TotalMarketValue) %>% 
  rename(`Fund Parent` = parent, Fund = AssetNameShort, `Fund Type` = IssueType,
         `Market Value` = TotalMarketValue)
  

write_csv(top_holdings, file = 'sowell_top_funds.csv')