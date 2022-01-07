
library(httr)
library(tidyverse)
library(salesforcer)
library(janitor)
library(fs)


sf_auth()

eba_new_account_cases <- sf_query("select Id,
                                  Name,
                                  RecordType.Name
                    from Contact
                    where OwnerId='0052M000008sO57QAE'")

cases_to_download <- eba_new_account_cases %>% 
  clean_names()

case_ids <- cases_to_download$id

#contacts
content_links <- case_ids %>% 
  split(f = rep(1:ceiling(length(case_ids) / 500), each = 500)[1:length(case_ids)]) %>% 
  map_dfr(~{
    
    
    link_query <- sprintf("SELECT
                           ContentDocumentId,
                           LinkedEntityId
                    FROM ContentDocumentLink 
                    where LinkedEntityId in ('%s') and LinkedEntity.Type='Contact'", 
                          paste0(.x, collapse = "','"))
    
    links <- sf_query(link_query)
    
    links
    
  }) %>% 
  distinct()

#break

content_ids <- content_links$ContentDocumentId

#contacts
content_links <- case_ids %>% 
  split(f = rep(1:ceiling(length(case_ids) / 500), each = 500)[1:length(case_ids)]) %>% 
  map_dfr(~{
    
    
    link_query <- sprintf("SELECT
                           ContentDocumentId,
                           LinkedEntityId
                    FROM ContentDocumentLink 
                    where LinkedEntityId in ('%s') and LinkedEntity.Type='Contact'", 
                          paste0(.x, collapse = "','"))
    
    links <- sf_query(link_query)
    
    links
    
  }) %>% 
  distinct()

#break

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


#contacts
case_file_urls <- content_links %>% 
  left_join(content_urls, by = c("ContentDocumentId")) %>% 
  left_join(cases_to_download, by = c("LinkedEntityId" = "id")) %>% 
  select(name, FileExtension, Title, VersionData) %>% 
  clean_names() %>% 
  modify_at(c("title"), str_replace, fixed('/'), replacement = '-') %>% 
  pwalk(function(...) {
    
    current <- tibble(...)
    
    write_dir <- fs::dir_create(str_c('salesforce_eba_export', 'DS Financial Strategies',
                                      current$name,  sep = '/'))
    
    file <- salesforcer::rGET(str_c("https://sowellmanagement.my.salesforce.com", current$version_data))
    
    file_name <- str_replace_all(str_c(strtrim(current$title, 64), current$file_extension, sep = '.'), "[[:punct:]]", "-")
    
    # print(str_c(current$title, current$file_extension, sep = '.'))
    
    writeBin(file$content, con = str_c(write_dir, file_name, sep = '/'))
    
    # print((con = str_c(write_dir, file_name, sep = '/')))

    
  })

