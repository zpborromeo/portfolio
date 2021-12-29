source('utils.R')

update_contacts <- readxl::read_excel("Excel_Files/update_sf_contacts.xlsx") 

update_households <- readxl::read_excel("Excel_Files/update_sf_households.xlsx")


response_contacts <- update_contacts %>% 
  split(f = rep(1:ceiling(nrow(update_contacts) / 10), each = 10)[1:nrow(update_contacts)]) %>% 
  map_dfr(~{
    
    # sf_create(.x, object_name = "Case")
    sf_update(.x, object_name = "Contact")
    
    
  })


response_households <- update_households %>% 
  split(f = rep(1:ceiling(nrow(update_households) / 10), each = 10)[1:nrow(update_households)]) %>% 
  map_dfr(~{
    
    # sf_create(.x, object_name = "Case")
    sf_update(.x, object_name = "Account")
    
    
  })
