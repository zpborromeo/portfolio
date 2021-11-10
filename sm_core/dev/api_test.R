


# one off calls
accounts <- httr::content(httr::GET('https://api.bdreporting.com/v1/account?page=1&pageSize=max', auth_header, body = "", encode='form'))
account2 <- httr::content(httr::GET('https://api.bdreporting.com/v1/account?page=2&pageSize=max', auth_header, body = "", encode='form'))
account3 <- httr::content(httr::GET('https://api.bdreporting.com/v1/account?page=3&pageSize=max', auth_header, body = "", encode='form'))
account4 <- httr::content(httr::GET('https://api.bdreporting.com/v1/account?page=4&pageSize=max', auth_header, body = "", encode='form'))
account5 <- httr::content(httr::GET('https://api.bdreporting.com/v1/account?page=5&pageSize=max', auth_header, body = "", encode='form'))



styles_import <- enframe(styles) %>% 
  select(value) %>% 
  unnest_wider(value) %>% 
  select(id, name) %>% 
  rename(style_name = name, black_diamond_id = id)

