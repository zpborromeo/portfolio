
setwd("C:\\sm-core\\sm_core")

source('utils.R')

batch_date <- get_latest_settlement_date(lubridate::today())

last_batch <- readr::read_csv('data/last_batch.csv') %>% 
  mutate(date = lubridate::ymd(date))

if(last_batch$date != batch_date){
  
  recon <- httr::content(httr::GET(str_c(bd_api$call_url, '/v1/firm/status'), auth_header, body = '', encode='form'))
  
  recon_status <- recon$status
  recon_date <- lubridate::ymd(recon$asOfDate)
  
  if(stringr::str_detect(recon_status, "Complete") & batch_date == recon_date){
    
    batch_id <- submit_black_diamond_batch_request(auth_header, bd_api$call_url, batch_date)
    
    batch_id %>% 
      as_tibble() %>% 
      mutate(date = batch_date,
             status = 'processing') %>% 
      readr::write_csv(file = 'data/last_batch.csv')
    
  }
  
}
