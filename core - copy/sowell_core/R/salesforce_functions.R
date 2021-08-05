complete_sf_task <- function(task){
  
  response <- task %>%
    select(Id, Status) %>% 
    mutate(Status = "Complete") %>% 
    sf_update(object_name = "Task")
  
  response
}

get_open_sf_tasks <- function(user_id){
  
  open_task_query <- sprintf("select Id,
                           Subject,
                           Status,
                           WhatId,
                           OwnerId
                    from Task
                    where Status = 'Open' and OwnerId in ('%s')", 
                             paste0(user_id, collapse = "','"))
  
  open_tasks <- sf_query(open_task_query)
  
  open_tasks
  
}


get_sf_cases_by_id <- function(case_ids){

  case_query <- sprintf("SELECT Id,
                           CaseNumber,
                           FinServ__FinancialAccount__c,
                           Current_model__c,
                           New_model__c,
                           Model_type__c,
                           Custom_Blend_Information__c,
                           Will_acct_be_SMS_traded__c,
                           Account_Number_from_Custodian__c
                    FROM Case 
                    WHERE Id in ('%s')", 
                            paste0(case_ids, collapse = "','"))
  
  cases <- sf_query(case_query)
  
  cases
  
}


# get all case types in Salesforce that could influence styles
get_case_types_by_name <- function(name){
  
  case_type_query <- glue::glue("select Id,
                           Name
                    from RecordType
                    where Name like '%{name}%'")
  
  case_types <- sf_query(case_type_query)
  
  case_types
  
}

# get all cases in Salesforce that could influence styles
get_cases_by_type <- function(case_type_ids, days_back){
  
  cut_off_date <- str_c(lubridate::today() - days_back, "T12:00:00.000Z")
  
  ids <- glue::glue_collapse(case_type_ids, sep = "','")
  
  case_query <- glue::glue("SELECT Id,
                           CaseNumber,
                           Account_Number_from_Custodian__c,
                           RecordTypeId,
                           CreatedDate,
                           FinServ__FinancialAccount__c,
                           Will_acct_be_SMS_traded__c,
                           Client_Fee__c
                    FROM Case 
                    WHERE RecordTypeId in ('{ids}') and CreatedDate > {cut_off_date}")
  
  
  cases <- sf_query(case_query)
  
  cases
  
}


# get Salesforce financial account objects based on IDs
get_financial_account_by_case <- function(case){
  
  financial_account_id <- case %>%
    filter(!is.na(FinServ__FinancialAccount__c)) %>% 
    pull(FinServ__FinancialAccount__c)
  
  financial_account_query <- sprintf("SELECT Id,
                           FinServ__FinancialAccountNumber__c
                    FROM FinServ__FinancialAccount__c
                    WHERE Id in ('%s')", 
                                     paste0(financial_account_id, collapse = "','"))
  
  financial_account <- sf_query(financial_account_query)
  
  financial_account
  
}



get_new_account_cases_by_account_number <- function(account_number){
  
  case_query <- sprintf("SELECT Id,
                           CaseNumber,
                           FinServ__FinancialAccount__c,
                           Current_model__c,
                           New_model__c,
                           Model_type__c,
                           Custom_Blend_Information__c,
                           Will_acct_be_SMS_traded__c,
                           Account_Number_from_Custodian__c
                    FROM Case 
                    WHERE Id in ('%s')", 
                        paste0(account_number, collapse = "','"))
  
  cases <- sf_query(case_query)
  
  cases
}






