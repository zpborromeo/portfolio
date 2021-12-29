source('utils.R')

contact_update <- readxl::read_xlsx("Excel_Files/update_contacts.xlsx")

response <- contact_update %>% 
  split(f = rep(1:ceiling(nrow(contact_update) / 10), each = 10)[1:nrow(contact_update)]) %>% 
  map_dfr(~{
    
    # sf_create(.x, object_name = "Case")
    sf_update(.x, object_name = "Contact")
    
    
  })

