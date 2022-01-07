source('utils.R')


create_households <- readxl::read_excel("Excel_Files/create_sf_households.xlsx")

# delete_households <- readxl::read_excel("Excel_Files/delete_sf_households.xlsx") 

response_households <- create_households %>%
  split(f = rep(1:ceiling(nrow(create_households) / 10), each = 10)[1:nrow(create_households)]) %>%
  map_dfr(~{

    sf_create(.x, object_name = "Account")
    # sf_delete(.x, object_name = "Account")

  })

save_document <- write.csv(response_households, "Excel_Files/households_made.csv")


househould_type_query1 <- sprintf("select Id,
    Name,
    RecordTypeId
    from Account", 
                               paste0(response$id, collapse = "','"))

household_ID <- sf_query(househould_type_query1)

write_csv(household_ids.csv")