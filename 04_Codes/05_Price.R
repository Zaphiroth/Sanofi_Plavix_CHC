# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  Sanofi CMAX
# Purpose:      Price
# programmer:   Zhe Liu
# date:         2020-08-14
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##---- Origin Price ----
price.market <- raw.total %>% 
  filter(city != "上海") %>% 
  bind_rows(sh.imp)

price.origin <- price.market %>% 
  filter(units > 0) %>% 
  group_by(packid, year, quarter, province, city) %>% 
  summarise(sales = sum(sales, na.rm = TRUE),
            units = sum(units, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(price = sales / units) %>% 
  select(-sales, -units)


##---- Mean price by year ----
price.year <- price.market %>% 
  filter(units > 0) %>% 
  group_by(packid, year, province, city) %>% 
  summarise(sales = sum(sales, na.rm = TRUE),
            units = sum(units, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(price_year = sales / units) %>% 
  select(-sales, -units)


##---- Mean price by city ----
price.city <- price.market %>% 
  filter(units > 0) %>% 
  group_by(packid, province, city) %>% 
  summarise(sales = sum(sales, na.rm = TRUE),
            units = sum(units, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(price_city = sales / units) %>% 
  select(-sales, -units)


##---- Mean price by province ----
price.province <- price.market %>% 
  filter(units > 0) %>% 
  group_by(packid, province) %>% 
  summarise(sales = sum(sales, na.rm = TRUE),
            units = sum(units, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(price_prov = sales / units) %>% 
  select(-sales, -units)


##---- Mean price by pack ID ----
price.pack <- price.market %>% 
  filter(units > 0) %>% 
  group_by(packid) %>% 
  summarise(sales = sum(sales, na.rm = TRUE),
            units = sum(units, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(price_pack = sales / units) %>% 
  select(-sales, -units)


##---- Add new price ----
proj.price <- proj.universe %>% 
  left_join(price.origin, by = c("province", "city", "year", "quarter", "packid")) %>% 
  left_join(price.year, by = c("province", "city", "year", "packid")) %>% 
  left_join(price.city, by = c("province", "city", "packid")) %>% 
  left_join(price.province, by = c("province", "packid")) %>% 
  left_join(price.pack, by = c("packid")) %>% 
  mutate(price = ifelse(is.na(price), price_year, price),
         price = ifelse(is.na(price), price_city, price),
         price = ifelse(is.na(price), price_prov, price),
         price = ifelse(is.na(price), price_pack, price)) %>% 
  mutate(units = sales / price) %>% 
  filter(units > 0, sales > 0, price > 0) %>% 
  select(year, month, quarter, province, city, atc4, nfc, 
         molecule, product, packid, units, sales, price)

write.xlsx(proj.price, "03_Outputs/05_Projection_with_Price.xlsx")





