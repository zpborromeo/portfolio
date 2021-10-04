
setwd("C:\\sm-core\\sm_core")

source('utils.R')

batch_date <- get_latest_settlement_date(lubridate::today())

last_batch <- readr::read_csv('data/last_batch.csv') %>% 
  mutate(date = lubridate::ymd(date))


if(last_batch$date == batch_date & last_batch$status == 'processing'){
  
  binary_zip <- get_batch_data(call_url, auth_header, last_batch$id)
  
  if(length(binary_zip) > 0){
    
    # fs::dir_ls('batch_data', recurse = TRUE) %>% 
    #   fs::file_delete()
    
    save_batch_data(batch_date, binary_zip)
    
    last_batch %>% 
      mutate(status = 'complete') %>% 
      readr::write_csv(file = 'data/last_batch.csv')
    
    salesforce_update_status <- 'FALSE'
    
    readr::write_file(salesforce_update_status, file = 'data/salesforce_updated')
    
    
  }

}




