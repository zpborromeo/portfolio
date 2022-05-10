
source('utils.R')


batch_date <- get_latest_settlement_date(lubridate::today())
bd_accounts <- load_black_diamond_accounts(batch_date)
bd_portfolios <- load_black_diamond_portfolios(batch_date)
bd_portfolio_groups <- load_black_diamond_portfolio_groups(batch_date)

portfolio_groups <- get_portfolio_groups(bd_portfolios, bd_portfolio_groups)

portfolios <-  get_portfolios(bd_portfolios)

portfolio_fee_schedules <- get_portfolio_fee_schedules(bd_portfolios)

clean_accounts <- get_clean_accounts(bd_accounts, portfolios,
                                     portfolio_groups, portfolio_fee_schedules)

attributes <- get_addepar_attributes(clean_accounts)


accounts <- get_addepar_accounts(addepar_api, addepar_header)


addepar_account_ids <- accounts %>% 
  jsonlite::flatten() %>%
  select(id, attributes.account_number) %>% 
  rename(account_number = attributes.account_number)
  

attribute_patch <- attributes %>% 
  filter(account_number %in% addepar_account_ids$account_number) %>% 
  left_join(addepar_account_ids, by = c("account_number")) %>% 
  map_dfc(~str_replace_na(.x, replacement = '-')) %>% 
  map_dfc(~if_else(.x == "", "-", .x))


patch_payload <- glue::glue('{
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
              "_custom_portfolio_name_854507": [
                {
                  "value": "<<attribute_patch$portfolio>>",
                  "date": null,
                  "weight": 1.0
                }
              ]
            }
        }', .open = "<<", .close = ">>")



patch_response <- patch_payload %>% 
  split(f = rep(1:ceiling(length(patch_payload) / 1000), each = 1000)[1:length(patch_payload)]) %>% 
  map(~{
    
    payload <- glue::glue('{ "data": [ <<glue::glue_collapse(.x,  sep = ",")>> ] }', .open = "<<", .close = ">>")
    print(payload)
    patch_response <- httr::PATCH(str_c(addepar_api$call_url, '/v1/entities'), addepar_header, 
                                  body = payload, 
                                  encode = 'form')
  })


patch_failues <- patch_response %>% 
  keep(~ .x$status != 200)
  


