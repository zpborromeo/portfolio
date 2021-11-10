source('utils.R')

# update_accounts <- readxl::read_excel("Excel_Files/update_sna_cases.xlsx")

# update_accounts <- readxl::read_excel("Excel_Files/aon_case_list.xlsx")

# update_accounts <- readxl::read_excel("Excel_Files/create_sna_aon_cases.xlsx")

update_accounts <- readxl::read_excel("Excel_Files/update_cases_assigned_to.xlsx")

response <- update_accounts %>% 
  split(f = rep(1:ceiling(nrow(update_accounts) / 10), each = 10)[1:nrow(update_accounts)]) %>% 
  map_dfr(~{
    
    # sf_create(.x, object_name = "Case")
     sf_update(.x, object_name = "Case")
    
    
  })

response <- sf_update(update_accounts, api_type = "Bulk 2.0", object_name = "Case")

query1 <- sprintf("select Id
    from Case
    WHERE Id in ('%s')",
    paste0(unique(response$id[is.na(response$id)]), collapse = "','"))

case_ID <- sf_query(query1)


failed_accts <- update_accounts %>%
  filter(!(Id %in% case_ID$Id))

