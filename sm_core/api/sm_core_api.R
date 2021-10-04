
library(plumber)

#* @apiTitle sm_core

#* Signal code base that black diamond batch data is ready for download
#* @post /batch_ready
function() {
 
  setwd("/home/sm_core/R/sm_core")
  
  # source('utils.R')
  
  readr::write_file("data is ready", file = 'data/batch_status')
  
  # print(req$body)
  # 
  # test <- jsonlite::toJSON(req$body)
  # readr::write_file(test, file = 'data/batch_status_2')
  # 
  
  # batch_date <- get_latest_settlement_date(lubridate::today())
  # 
  # # get batch data
  # get_bd_batch_data(batch_date)
  
}
