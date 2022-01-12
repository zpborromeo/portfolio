source('utils.R')

addepar_sm <- readxl::read_excel("Excel_Files/addepar-sm.xlsx") %>% 
  clean_names() %>% 
  rename("sm_sec_asset_type" = sec_asset_type)

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

ticker_upload_table <- ticker_match %>% 
  select(ticker, entity_id, security, sm_class, sm_segment, sm_sector_segment, sm_issue_type, sm_sec_asset_type, issue_type,
         class, segment, dual_segment, sec_asset_type, class_match, segment_match, sector_segment_match, issue_type_match, 
         sec_asset_type_match, full_match) %>% 
  mutate(new_class = if_else(class_match == TRUE, sm_class, class)) %>% 
  mutate(new_class = if_else(is.na(class), sm_class, class)) %>% 
  mutate(new_segment = if_else(segment_match == TRUE, sm_segment, segment)) %>%
  mutate(new_segment = if_else(is.na(segment), sm_segment, segment)) %>% 
  mutate(new_sector_segment = if_else(sector_segment_match == TRUE, sm_sector_segment, dual_segment)) %>%
  mutate(new_sector_segment = if_else(is.na(dual_segment), sm_sector_segment, dual_segment)) %>% 
  mutate(new_issue_type = if_else(issue_type_match == TRUE, sm_issue_type, issue_type)) %>%
  mutate(new_issue_type = if_else(is.na(issue_type), sm_issue_type, issue_type)) %>% 
  mutate(new_sec_asset_type = sec_asset_type) %>% 
  select(ticker, entity_id, security, new_class, new_segment, new_sector_segment, new_issue_type, new_sec_asset_type)


cusip_upload_table <- cusip_match %>% 
  select(cusip, entity_id, security, sm_class, sm_segment, sm_sector_segment, sm_issue_type, sm_sec_asset_type, issue_type,
         class, segment, dual_segment, sec_asset_type, class_match, segment_match, sector_segment_match, issue_type_match, 
         sec_asset_type_match, full_match) %>% 
  mutate(new_class = if_else(class_match == TRUE, sm_class, class)) %>% 
  mutate(new_class = if_else(is.na(class), sm_class, class)) %>% 
  mutate(new_segment = if_else(segment_match == TRUE, sm_segment, segment)) %>%
  mutate(new_segment = if_else(is.na(segment), sm_segment, segment)) %>% 
  mutate(new_sector_segment = if_else(sector_segment_match == TRUE, sm_sector_segment, dual_segment)) %>%
  mutate(new_sector_segment = if_else(is.na(dual_segment), sm_sector_segment, dual_segment)) %>% 
  mutate(new_issue_type = if_else(issue_type_match == TRUE, sm_issue_type, issue_type)) %>%
  mutate(new_issue_type = if_else(is.na(issue_type), sm_issue_type, issue_type)) %>% 
  mutate(new_sec_asset_type = sec_asset_type) %>% 
  select(cusip, entity_id, security, new_class, new_segment, new_sector_segment, new_issue_type, new_sec_asset_type)

save_document <- write.csv(ticker_upload_table, "C:/sm-core/sm_core/Excel_Files/ticker_upload.csv")

save_document <- write.csv(cusip_upload_table, "C:/sm-core/sm_core/Excel_Files/cusip_upload.csv")


