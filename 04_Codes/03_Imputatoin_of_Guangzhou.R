# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  Sanofi CMAX
# Purpose:      Imputation of Guangzhou
# programmer:   Zhe Liu
# Date:         2020-12-08
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##---- Model ----
## model set
gz.model.data <- raw.total %>% 
  mutate(flag = if_else(province == '广东', 1, 0))

gz.model.set <- gz.model.data %>% 
  filter(year %in% c('2018', '2019', '2020'), !(quarter %in% c('2020Q4'))) %>% 
  group_by(province, city, district, pchc, date, flag) %>% 
  summarise(sales = sum(sales, na.rm = TRUE)) %>% 
  ungroup() %>% 
  pivot_wider(id_cols = c(province, city, district, pchc, flag), 
              names_from = date, 
              values_from = sales, 
              values_fill = 0)

## model
gz.train.set <- gz.model.set[gz.model.set$flag == 0, ]
gz.test.set <- gz.model.set[gz.model.set$flag == 1, ]

gz.knn.model <- kknn(flag ~ ., 
                     train = gz.train.set[, -(1:4)], 
                     test = gz.test.set[, -(1:4)], 
                     k = 3, 
                     scale = TRUE)

## model weightage
gz.model.indice <- as.data.frame(gz.knn.model$C) %>% 
  lapply(function(x) {
    gz.train.set$pchc[x]
  }) %>% 
  as.data.frame(col.names = c('pchc_1', 'pchc_2', 'pchc_3')) %>% 
  bind_cols(gz.test.set[, 1:4]) %>% 
  pivot_longer(cols = starts_with('pchc_'), 
               names_to = 'knn_level', 
               values_to = 'knn_pchc')

gz.model.weight <- as.data.frame(gz.knn.model$D) %>% 
  lapply(function(x) {
    1 / (x + 1)
  }) %>% 
  as.data.frame(col.names = c('pchc_1', 'pchc_2', 'pchc_3')) %>% 
  mutate(pchc_1 = pchc_1 / (pchc_1 + pchc_2 + pchc_3),
         pchc_2 = pchc_2 / (pchc_1 + pchc_2 + pchc_3),
         pchc_3 = pchc_3 / (pchc_1 + pchc_2 + pchc_3)) %>% 
  bind_cols(gz.test.set[, 1:4]) %>% 
  pivot_longer(cols = starts_with('pchc_'), 
               names_to = 'knn_level', 
               values_to = 'knn_weight')


##---- Growth ----
gz.model.growth <- gz.model.data %>% 
  filter(flag == 0) %>% 
  group_by(knn_pchc = pchc, molecule, year, date, quarter) %>% 
  summarise(sales = sum(sales, na.rm = TRUE)) %>% 
  ungroup() %>% 
  inner_join(gz.model.indice, by = 'knn_pchc') %>% 
  left_join(gz.model.weight, 
            by = c('province', 'city', 'district', 'pchc', 'knn_level')) %>% 
  group_by(pchc, molecule, year, date, quarter) %>% 
  summarise(sales = sum(sales * knn_weight, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(month = stri_sub(date, 5, 6)) %>% 
  pivot_wider(id_cols = c(pchc, molecule, month), 
              names_from = year, 
              values_from = sales, 
              values_fill = 0) %>% 
  mutate(growth_1920 = `2020` / `2019`) %>% 
  select(pchc, molecule, month, `2019` = growth_1920) %>% 
  pivot_longer(cols = starts_with('20'), 
               names_to = 'year', 
               values_to = 'growth') %>% 
  filter(!is.na(growth), !is.infinite(growth)) %>% 
  mutate(date = stri_paste(year, month)) %>% 
  select(year, date, pchc, molecule, growth)


##---- Prediction ----
## predict 2020
imp.gz <- gz.model.data %>% 
  filter(quarter %in% c('2019Q3'), flag == 1) %>% 
  left_join(gz.model.growth, by = c('pchc', 'molecule', 'year', 'date')) %>% 
  mutate(growth = if_else(is.na(growth), 1, growth),
         growth = if_else(growth > 3, 3, growth),
         growth = if_else(growth > quantile(growth, 0.9),
                          mean(growth[growth >= quantile(growth, 0.25) & 
                                        growth <= quantile(growth, 0.75)]),
                          growth)) %>% 
  mutate(units_imp = units * growth,
         sales_imp = sales * growth,
         date = gsub('2019', '2020', date),
         quarter = gsub('2019', '2020', quarter),
         year = '2020',
         flag = 1) %>% 
  group_by(year, date, quarter, province, city, district, pchc, atc4, nfc, 
           molecule, product, packid, flag) %>% 
  summarise(units = sum(units_imp, na.rm = TRUE),
            sales = sum(sales_imp, na.rm = TRUE)) %>% 
  ungroup() %>% 
  filter(units > 0, sales > 0) %>% 
  select(year, date, quarter, province, city, district, pchc, atc4, nfc, 
         molecule, product, packid, units, sales, flag)


##---- Result ----
imp.total <- bind_rows(raw.total, imp.sh, imp.gz)

write.xlsx(imp.total, '03_Outputs/03_Sanofi_Plavix_CHC_Imp.xlsx')
