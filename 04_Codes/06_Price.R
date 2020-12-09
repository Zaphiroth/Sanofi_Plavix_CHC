# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  Sanofi CMAX
# Purpose:      Price
# programmer:   Zhe Liu
# date:         2020-12-09
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##---- Origin Price ----
price.origin <- imp.total %>% 
  group_by(packid, quarter, province, city) %>% 
  summarise(sales = sum(sales, na.rm = TRUE),
            units = sum(units, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(price = sales / units) %>% 
  select(-sales, -units)


##---- Mean price by city year ----
price.city <- imp.total %>% 
  group_by(packid, year, province, city) %>% 
  summarise(sales = sum(sales, na.rm = TRUE), 
            units = sum(units, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(price_city = sales / units) %>% 
  select(-sales, -units)


##---- Mean price by province quarter ----
price.province <- imp.total %>% 
  group_by(packid, quarter, province) %>% 
  summarise(sales = sum(sales, na.rm = TRUE),
            units = sum(units, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(price_prov = sales / units) %>% 
  select(-sales, -units)


##---- Mean price by province year ----
price.year <- imp.total %>% 
  group_by(packid, year, province) %>% 
  summarise(sales = sum(sales, na.rm = TRUE), 
            units = sum(units, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(price_year = sales / units) %>% 
  select(-sales, -units)


##---- Mean price by pack quarter ----
price.pack <- imp.total %>% 
  group_by(packid, quarter) %>% 
  summarise(sales = sum(sales, na.rm = TRUE),
            units = sum(units, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(price_pack = sales / units) %>% 
  select(-sales, -units)


##---- Mean price by pack year ----
price.pack.year <- imp.total %>% 
  group_by(packid, year) %>% 
  summarise(sales = sum(sales, na.rm = TRUE),
            units = sum(units, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(price_pack_year = sales / units) %>% 
  select(-sales, -units)


##---- Add new price ----
proj.price <- proj.universe %>% 
  mutate(city_price = if_else(city == '上海' & quarter != '2018Q1', 
                              '北京', 
                              city)) %>% 
  left_join(price.origin, by = c('province', 'city_price' = 'city', 'quarter', 'packid')) %>% 
  left_join(price.city, by = c('province', 'city_price' = 'city', 'year', 'packid')) %>% 
  left_join(price.province, by = c('province', 'quarter', 'packid')) %>% 
  left_join(price.year, by = c('province', 'year', 'packid')) %>% 
  left_join(price.pack, by = c('quarter', 'packid')) %>% 
  left_join(price.pack.year, by = c('year', 'packid')) %>% 
  mutate(price = if_else(is.na(price), price_city, price), 
         price = if_else(is.na(price), price_prov, price), 
         price = if_else(is.na(price), price_year, price), 
         price = if_else(is.na(price), price_pack, price), 
         price = if_else(is.na(price), price_pack_year, price)) %>% 
  mutate(units = sales / price) %>% 
  filter(units > 0, sales > 0, price > 0) %>% 
  select(year, date, quarter, province, city, atc4, nfc, molecule, product, 
         packid, price, units, sales)

write.xlsx(proj.price, "03_Outputs/06_Sanofi_Plavix_CHC_Proj_with_Price.xlsx")
