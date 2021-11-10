source('utils.R')

contact_type_query <- glue::glue("select Id,
    Name,
    RecordTypeId
    from Contact")

contact_ID <- sf_query(contact_type_query)

contact_upload <- readxl::read_xlsx("Excel_Files/contact_auto_gen.xlsx")

response <- sf_create(contact_upload, api_type = "REST", object_name = "Contact")


contact_type_query1 <- sprintf("select Id,
    Name,
    RecordTypeId
    from Contact
    WHERE Id in ('%s')", 
    paste0(response$id, collapse = "','"))

contact_ID <- sf_query(contact_type_query1)

write_csv(contact_ID, "contact_ids.csv")
