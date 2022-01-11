source('utils.R')

addepar_sm <- readxl::read_excel("Excel_Files/addepar-sm.xlsx") %>% 
  clean_names() %>% 
  rename("sm_sec_asset_type" = sec_asset_type) 
  # rename("addepar_cusip" = cusip) %>% 
  # rename("addepar_ticker" = ticker)

black_diamond_sm <- readxl::read_excel("Excel_Files/black-diamond-sm.xlsx") %>% 
  clean_names()

ticker_match <- addepar_sm %>% 
  left_join(black_diamond_sm, by="ticker") %>% 
  select(ticker, security, entity_id, sm_class, sm_segment, sm_sector_segment, sm_issue_type, sm_sec_asset_type, issue_type,
        class, segment, dual_segment, sec_asset_type) %>% 
  filter(ticker != "-") %>%
  mutate(class_match = if_else(sm_class == class, TRUE, FALSE)) %>%
  mutate(segment_match = if_else(sm_segment == segment, TRUE, FALSE)) %>%
  mutate(sector_segment_match = if_else(sm_sector_segment == dual_segment, TRUE, FALSE)) %>%
  mutate(issue_type_match = if_else(sm_issue_type == issue_type, TRUE, FALSE)) %>% 
  mutate(sec_asset_type_match = if_else(sm_sec_asset_type == sec_asset_type, TRUE, FALSE)) %>% 
  mutate(full_match = if_else(class_match == TRUE && segment_match == TRUE && sector_segment_match == TRUE && issue_type_match == TRUE && sec_asset_type_match == TRUE, "Fully Matched Information", "Mismatched Information"))

cusip_match <- addepar_sm %>% 
  left_join(black_diamond_sm, by="cusip") %>% 
  select(cusip, security, entity_id, sm_class, sm_segment, sm_sector_segment, sm_issue_type, sm_sec_asset_type, issue_type,
         class, segment, dual_segment, sec_asset_type) %>% 
  filter(cusip != "-")  %>%
  mutate(class_match = if_else(sm_class == class, TRUE, FALSE)) %>%
  mutate(segment_match = if_else(sm_segment == segment, TRUE, FALSE)) %>%
  mutate(sector_segment_match = if_else(sm_sector_segment == dual_segment, TRUE, FALSE)) %>%
  mutate(issue_type_match = if_else(sm_issue_type == issue_type, TRUE, FALSE)) %>% 
  mutate(sec_asset_type_match = if_else(sm_sec_asset_type == sec_asset_type, TRUE, FALSE)) %>% 
  mutate(full_match = if_else(class_match == TRUE && segment_match == TRUE && sector_segment_match == TRUE && issue_type_match == TRUE && sec_asset_type_match == TRUE, "Fully Matched Information", "Mismatched Information"))

save_document <- write.csv(ticker_match, "C:/sm-core/sm_core/Excel_Files/ticker_match.csv")

save_document <- write.csv(cusip_match, "C:/sm-core/sm_core/Excel_Files/cusip_match.csv")

ticker_class_missing <- ticker_match %>% 
  filter(sm_class == "-")

ticker_asset_type_missing <- ticker_match %>% 
  filter(is.na(sm_sec_asset_type))

ticker_issue_type_missing <- ticker_match %>% 
  filter(sm_issue_type == "-")
