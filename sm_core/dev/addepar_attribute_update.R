source('utils.R')


base_64_auth <- base64enc::base64encode(charToRaw(str_c(addepar_api$api_id, ':', addepar_api$api_secret)))

addepar_header <- httr::add_headers('Authorization' = str_c("Basic", base_64_auth, sep = ' '),
                                    'Content-Type' = 'application/vnd.api+json', 
                                    'Accept' = 'application/vnd.api+json', 'addepar-firm' = '717')



batch_date <- get_latest_settlement_date(lubridate::today())

bd_accounts <- load_black_diamond_accounts(batch_date) 


clean_accounts <- bd_accounts %>% 
  clean_names() %>% 
  unnest(team, keep_empty = TRUE) %>% 
  rename(team = Name, account_name = name) %>% 
  unnest(style, keep_empty = TRUE) %>% 
  unnest(style, keep_empty = TRUE) %>% 
  unnest_wider(address) %>% 
  rowwise() %>% 
  mutate(account_number = str_remove(long_number, "^[^_]+_"),
         payout_name = str_trim(str_squish(tags$Value[tags$Name == "Payout Name"])),
         aum_type = tags$Value[tags$Name == "AUM Type"],
         primary_rep_code = tags$Value[tags$Name == "Primary Rep Code"],
         advisor_name = str_trim(str_squish(tags$Value[tags$Name == "Advisor Name"])),
         manager = str_trim(str_squish(if_else(is.null(manager), "", manager$LastName)))) %>% 
  clean_names() %>% 
  unnest(fee_schedules, keep_empty = TRUE) %>% 
  rename(fee_schedule_name = Name)
  

custodian_by_account <- get_custodian_by_account(clean_accounts)

attributes <- get_addepar_attributes(clean_accounts, custodian_by_account)

accounts <- get_addepar_accounts(addepar_api, addepar_header)

addepar_account_ids <- accounts %>% 
  jsonlite::flatten() %>%
  select(id, attributes.account_number) %>% 
  rename(account_number = attributes.account_number)
  

attribute_patch <- attributes %>% 
  filter(account_number %in% accounts$attributes$account_number) %>% 
  left_join(addepar_account_ids, by = c("account_number")) %>% 
  map_dfc(~str_replace_na(.x, replacement = '')) %>% 
  map_dfc(~if_else(.x == "", "Not Assigned", .x)) %>% 
  map_dfc(~str_replace_all(.x, coll("&"), 'and'))

glue_test <- glue::glue('{
          "id": "<<attribute_patch$id>>",
          "type": "entities",
          "attributes": 
            {
              
              "_custom_account_name_614051": [
                {
                  "value": "<<attribute_patch$account_name>>",
                  "date": null,
                  "weight": 1.0
                }
              ],
              "_custom_style_614133": [
                {
                  "value": "<<attribute_patch$style>>",
                  "date": null,
                  "weight": 1.0
                }
              ],
              "_custom_fee_schedule_name_838010": [
                {
                  "value": "<<attribute_patch$fee_schedule_name>>",
                  "date": null,
                  "weight": 1.0
                }
              ],
              "_custom_manager_616027": [
                {
                  "value": "<<attribute_patch$manager>>",
                  "date": null,
                  "weight": 1.0
                }
              ],
              "_custom_team_610116": [
                {
                  "value": "<<attribute_patch$team>>",
                  "date": null,
                  "weight": 1.0
                }
              ],
              "_custom_advisor_name_614083": [
                {
                  "value": "<<attribute_patch$advisor_name>>",
                  "date": null,
                  "weight": 1.0
                }
              ],
              "_custom_account_registration_type_610112": [
                {
                  "value": "<<attribute_patch$account_registration_type>>",
                  "date": null,
                  "weight": 1.0
                }
              ],
              "_custom_data_provider_610113": [
                {
                  "value": "<<attribute_patch$data_provider>>",
                  "date": null,
                  "weight": 1.0
                }
              ],
              "_custom_aum_type_616025": [
                {
                  "value": "<<attribute_patch$aum_type>>",
                  "date": null,
                  "weight": 1.0
                }
              ],
              "_custom_billing_account_number_833147": [
                {
                  "value": "<<attribute_patch$billing_account_number>>",
                  "date": null,
                  "weight": 1.0
                }
              ],
              "_custom_billing_account_custodian_833148": [
                {
                  "value": "<<attribute_patch$billing_account_custodian>>",
                  "date": null,
                  "weight": 1.0
                }
              ],
              "_custom_unsupervised_661127": [
                {
                  "value": "<<attribute_patch$unsupervised>>",
                  "date": null,
                  "weight": 1.0
                }
              ],
              "_custom_nonbillable_661141": [
                {
                  "value": "<<attribute_patch$nonbillable>>",
                  "date": null,
                  "weight": 1.0
                }
              ],
              "_custom_tax_status_834079": [
                {
                  "value": "<<attribute_patch$tax_status>>",
                  "date": null,
                  "weight": 1.0
                }
              ],
              "_custom_address_637788": [
                {
                  "value": "<<attribute_patch$address>>",
                  "date": null,
                  "weight": 1.0
                }
              ],
              "_custom_first_name_637605": [
                {
                  "value": "<<attribute_patch$first_name>>",
                  "date": null,
                  "weight": 1.0
                }
              ],
              "_custom_last_name_637604": [
                {
                  "value": "<<attribute_patch$last_name>>",
                  "date": null,
                  "weight": 1.0
                }
              ],
              "_custom_zip_669225": [
                {
                  "value": "<<attribute_patch$zip>>",
                  "date": null,
                  "weight": 1.0
                }
              ],
              "_custom_state_669224": [
                {
                  "value": "<<attribute_patch$state>>",
                  "date": null,
                  "weight": 1.0
                }
              ],
              "_custom_city_669223": [
                {
                  "value": "<<attribute_patch$city>>",
                  "date": null,
                  "weight": 1.0
                }
              ],
              "_custom_custodian_764300": [
                {
                  "value": "<<attribute_patch$custodian>>",
                  "date": null,
                  "weight": 1.0
                }
              ],
              "_custom_internal_style_834324": [
                {
                  "value": "<<attribute_patch$sm_traded>>",
                  "date": null,
                  "weight": 1.0
                }
              ],
              "_custom_advisor_choice_834323": [
                {
                  "value": "<<attribute_patch$advisor_choice>>",
                  "date": null,
                  "weight": 1.0
                }
              ]
            }
        }', .open = "<<", .close = ">>")







patch_response <- glue_test %>% 
  split(f = rep(1:ceiling(length(glue_test) / 1000), each = 1000)[1:length(glue_test)]) %>% 
  map(~{
    
    payload <- glue::glue('{ "data": [ <<glue::glue_collapse(.x,  sep = ",")>> ] }', .open = "<<", .close = ">>")
    
    
    patch_response <- httr::PATCH(str_c(addepar_api$call_url, '/v1/entities'), addepar_header, body = payload, encode = 'form')
    
  })


# testing 
x <- rawToChar(patch_response[[6]]$content)
x




  

