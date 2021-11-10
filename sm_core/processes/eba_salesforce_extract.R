
library(httr)
library(tidyverse)
library(salesforcer)
library(janitor)
library(fs)


sf_auth()

eba_new_account_cases <- sf_query("select Id,
                           CaseNumber,
                           Financial_Advisor__r.Name,
                           Financial_Advisor__r.Account.Name,
                           Account_Number_from_Custodian__c,
                           RecordType.Name
                    from Case
                    where RecordType.Name like '%EBA - Submit New Account%'")


cases_to_download <- eba_new_account_cases %>% 
  clean_names()




case_ids <- cases_to_download$id

content_links <- case_ids %>% 
  split(f = rep(1:ceiling(length(case_ids) / 500), each = 500)[1:length(case_ids)]) %>% 
  map_dfr(~{
    
    
    link_query <- sprintf("SELECT
                           ContentDocumentId,
                           LinkedEntityId
                    FROM ContentDocumentLink 
                    where LinkedEntityId in ('%s') and LinkedEntity.Type='Case'", 
                                  paste0(.x, collapse = "','"))
    
    links <- sf_query(link_query)
    
    links
    
  }) %>% 
  distinct()





content_ids <- content_links$ContentDocumentId

content_urls <- content_ids %>% 
  split(f = rep(1:ceiling(length(content_ids) / 500), each = 500)[1:length(content_ids)]) %>% 
  map_dfr(~{
    
    
    content_query <- sprintf("SELECT
                           ContentDocumentId,
                           VersionData,
                           FileExtension,
                           FileType,
                           IsLatest,
                           Title
                    FROM ContentVersion 
                    where ContentDocumentId in ('%s')", 
                             paste0(.x, collapse = "','"))
    
    
    content <- sf_query(content_query)
    
    content
    
  }) %>% 
  distinct()



case_file_urls <- content_links %>% 
  left_join(content_urls, by = c("ContentDocumentId")) %>% 
  left_join(cases_to_download, by = c("LinkedEntityId" = "id")) %>% 
  select(financial_advisor_r_account_name, financial_advisor_r_name, account_number_from_custodian_c, 
         case_number, FileExtension, Title, VersionData) %>% 
  clean_names() %>% 
  modify_at(c("financial_advisor_r_account_name", "financial_advisor_r_name", "title"), str_replace, fixed('/'), replacement = '-') %>% 
  mutate(financial_advisor_r_account_name = if_else(is.na(financial_advisor_r_account_name), "Missing Firm", financial_advisor_r_account_name),
         financial_advisor_r_name = if_else(is.na(financial_advisor_r_name), "Missing Advisor", financial_advisor_r_name),
         account_number_from_custodian_c = if_else(is.na(account_number_from_custodian_c), "Missing Account", account_number_from_custodian_c),
         file_extension = if_else(title == 'Tiffany Nguyen Drivers License', "jpg", file_extension),
         file_extension = if_else(title == 'Client Agreement D Petesch', "pdf", file_extension)) %>% 
  pwalk(function(...) {
    
    current <- tibble(...)
    
    write_dir <- fs::dir_create(str_c('salesforce_eba_export', current$financial_advisor_r_account_name, 
                       current$financial_advisor_r_name, current$account_number_from_custodian_c, current$case_number, sep = '/'))

    file <- salesforcer::rGET(str_c("https://sowellmanagement.my.salesforce.com", current$version_data))

    writeBin(file$content, con = str_c(write_dir, str_c(current$title, current$file_extension, sep = '.'), sep = '/'))
    
  })

