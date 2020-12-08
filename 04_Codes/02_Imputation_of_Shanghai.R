# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  Sanofi CMAX
# Purpose:      Imputation of Shanghai
# programmer:   Zhe Liu
# Date:         2020-12-08
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##---- Sample data ----
## Shanghai
history.sh1 <- read.xlsx('02_Inputs/data/上海/上海_2017.xlsx')
history.sh2 <- read.xlsx('02_Inputs/data/上海/上海_2018.xlsx')

history.sh <- bind_rows(history.sh1, history.sh2) %>% 
  mutate(quarter_m = stri_sub(Date, 5, 6)) %>% 
  distinct(year = stri_sub(Date, 1, 4), 
           quarter = ifelse(quarter_m %in% c('01', '02', '03'), 
                            stri_paste(year, 'Q1'), 
                            ifelse(quarter_m %in% c('04', '05', '06'), 
                                   stri_paste(year, 'Q2'), 
                                   ifelse(quarter_m %in% c('07', '08', '09'), 
                                          stri_paste(year, 'Q3'), 
                                          ifelse(quarter_m %in% c('10', '11', '12'), 
                                                 stri_paste(year, 'Q4'), 
                                                 year)))), 
           date = as.character(Date), 
           province = '上海', 
           city = '上海', 
           pchc = PCHC, 
           packid = stri_pad_left(pfc, 7, 0), 
           price = value / unit, 
           units = unit, 
           sales = value) %>% 
  mutate(pchc = case_when(pchc == 'PCHC06729' ~ 'PCHC06728', 
                          pchc == 'PCHC06622' ~ 'PCHC06620', 
                          pchc == 'PCHC06645' ~ 'PCHC06644', 
                          pchc == 'PCHC06722' ~ 'PCHC06721', 
                          pchc == 'PCHC06840' ~ 'PCHC06839', 
                          TRUE ~ pchc)) %>% 
  left_join(pchc.mapping4, by = c('province', 'city', 'pchc')) %>% 
  left_join(ims.mol, by = 'packid') %>% 
  filter(pchc != '#N/A', units > 0, sales > 0) %>% 
  mutate(packid = if_else(stri_sub(packid, 1, 5) == '47775', 
                          stri_paste('58906', stri_sub(packid, 6, 7)), 
                          packid), 
         packid = if_else(stri_sub(packid, 1, 5) == '06470', 
                          stri_paste('64895', stri_sub(packid, 6, 7)), 
                          packid)) %>% 
  select(year, date, quarter, province, city, district, pchc, atc4 = ATC4_Code, 
         nfc = NFC123_Code, molecule = Molecule_Desc, product = Prd_desc, 
         packid, units, sales)

## Beijing
history.bj <- read.xlsx('02_Inputs/data/上海/Plavix_北京市_2017_packid_moleinfo.xlsx') %>% 
  distinct(year = as.character(Year), 
           quarter = Quarter, 
           date = as.character(Month), 
           province = '北京', 
           city = '北京', 
           district = County, 
           hospital = Hospital_Name, 
           packid = stri_pad_left(packcode, 7, 0), 
           units = if_else(is.na(Volume), Value / Price, Volume), 
           sales = Value) %>% 
  left_join(pchc.mapping3, by = c('province', 'city', 'district', 'hospital')) %>% 
  filter(!is.na(pchc), !is.na(packid)) %>% 
  mutate(packid = if_else(stri_sub(packid, 1, 5) == '47775', 
                          stri_paste('58906', stri_sub(packid, 6, 7)), 
                          packid), 
         packid = if_else(stri_sub(packid, 1, 5) == '06470', 
                          stri_paste('64895', stri_sub(packid, 6, 7)), 
                          packid)) %>% 
  left_join(ims.mol, by = 'packid') %>% 
  filter(units > 0, sales > 0) %>% 
  select(year, date, quarter, province, city, district, pchc, atc4 = ATC4_Code, 
         nfc = NFC123_Code, molecule = Molecule_Desc, product = Prd_desc, 
         packid, units, sales)


##---- Model ----
## model set
sh.model.data <- bind_rows(history.sh, history.bj, raw.total) %>% 
  filter(province %in% c('上海', '北京'), 
         packid %in% market.def$Pack_ID) %>% 
  mutate(flag = if_else(province == '上海', 1, 0))

sh.model.set <- sh.model.data %>% 
  filter(quarter %in% c('2017Q1', '2017Q2', '2017Q3', '2017Q4', '2018Q1')) %>% 
  group_by(province, city, district, pchc, date, flag) %>% 
  summarise(sales = sum(sales, na.rm = TRUE)) %>% 
  ungroup() %>% 
  pivot_wider(id_cols = c(province, city, district, pchc, flag), 
              names_from = date, 
              values_from = sales, 
              values_fill = 0)

## model
sh.train.set <- sh.model.set[sh.model.set$flag == 0, ]
sh.test.set <- sh.model.set[sh.model.set$flag == 1, ]

sh.knn.model <- kknn(flag ~ ., 
                     train = sh.train.set[, -(1:4)], 
                     test = sh.test.set[, -(1:4)], 
                     k = 3, 
                     scale = TRUE)

## model weightage
sh.model.indice <- as.data.frame(sh.knn.model$C) %>% 
  lapply(function(x) {
    sh.train.set$pchc[x]
  }) %>% 
  as.data.frame(col.names = c('pchc_1', 'pchc_2', 'pchc_3')) %>% 
  bind_cols(sh.test.set[, 1:4]) %>% 
  pivot_longer(cols = starts_with('pchc_'), 
               names_to = 'knn_level', 
               values_to = 'knn_pchc')

sh.model.weight <- as.data.frame(sh.knn.model$D) %>% 
  lapply(function(x) {
    1 / (x + 1)
  }) %>% 
  as.data.frame(col.names = c('pchc_1', 'pchc_2', 'pchc_3')) %>% 
  mutate(pchc_1 = pchc_1 / (pchc_1 + pchc_2 + pchc_3),
         pchc_2 = pchc_2 / (pchc_1 + pchc_2 + pchc_3),
         pchc_3 = pchc_3 / (pchc_1 + pchc_2 + pchc_3)) %>% 
  bind_cols(sh.test.set[, 1:4]) %>% 
  pivot_longer(cols = starts_with('pchc_'), 
               names_to = 'knn_level', 
               values_to = 'knn_weight')


##---- Growth ----
sh.model.growth <- sh.model.data %>% 
  filter(flag == 0) %>% 
  group_by(knn_pchc = pchc, molecule, year, date, quarter) %>% 
  summarise(sales = sum(sales, na.rm = TRUE)) %>% 
  ungroup() %>% 
  inner_join(sh.model.indice, by = 'knn_pchc') %>% 
  left_join(sh.model.weight, 
            by = c('province', 'city', 'district', 'pchc', 'knn_level')) %>% 
  group_by(pchc, molecule, year, date, quarter) %>% 
  summarise(sales = sum(sales * knn_weight, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(month = stri_sub(date, 5, 6)) %>% 
  pivot_wider(id_cols = c(pchc, molecule, month), 
              names_from = year, 
              values_from = sales, 
              values_fill = 0) %>% 
  mutate(growth_1718 = `2018` / `2017`, 
         growth_1819 = `2019` / `2018`, 
         growth_1920 = `2020` / `2019`) %>% 
  select(pchc, molecule, month, `2017` = growth_1718, `2018` = growth_1819, 
         `2019` = growth_1920) %>% 
  pivot_longer(cols = starts_with('20'), 
               names_to = 'year', 
               values_to = 'growth') %>% 
  filter(!is.na(growth), !is.infinite(growth)) %>% 
  mutate(date = stri_paste(year, month)) %>% 
  select(year, date, pchc, molecule, growth)


##---- Prediction ----
## predict 2018
sh.predict.sales.18 <- sh.model.data %>% 
  filter(quarter %in% c('2017Q2', '2017Q3', '2017Q4'), flag == 1) %>% 
  left_join(sh.model.growth, by = c('pchc', 'molecule', 'year', 'date')) %>% 
  mutate(growth = if_else(is.na(growth), 1, growth),
         growth = if_else(growth > 3, 3, growth),
         growth = if_else(growth > quantile(growth, 0.9),
                          mean(growth[growth >= quantile(growth, 0.25) & 
                                        growth <= quantile(growth, 0.75)]),
                          growth)) %>% 
  mutate(units_imp = units * growth,
         sales_imp = sales * growth,
         date = gsub('2017', '2018', date),
         quarter = gsub('2017', '2018', quarter),
         year = '2018',
         flag = 1) %>% 
  select(year, date, quarter, province, city, district, pchc, atc4, nfc, molecule, 
         product, packid, units = units_imp, sales = sales_imp, flag)

## predict 2019
sh.predict.sales.19 <- sh.model.data %>% 
  filter(quarter %in% c('2018Q1'), flag == 1) %>% 
  bind_rows(sh.predict.sales.18) %>% 
  left_join(sh.model.growth, by = c('pchc', 'molecule', 'year', 'date')) %>% 
  mutate(growth = if_else(is.na(growth), 1, growth),
         growth = if_else(growth > 3, 3, growth),
         growth = if_else(growth > quantile(growth, 0.9),
                          mean(growth[growth >= quantile(growth, 0.25) & 
                                        growth <= quantile(growth, 0.75)]),
                          growth)) %>% 
  mutate(units_imp = units * growth,
         sales_imp = sales * growth,
         date = gsub('2018', '2019', date),
         quarter = gsub('2018', '2019', quarter),
         year = '2019',
         flag = 1) %>% 
  select(year, date, quarter, province, city, district, pchc, atc4, nfc, molecule, 
         product, packid, units = units_imp, sales = sales_imp, flag)

## predict 2020
sh.predict.sales.20 <- sh.predict.sales.19 %>% 
  filter(quarter %in% c('2019Q1', '2019Q2', '2019Q3')) %>% 
  left_join(sh.model.growth, by = c('pchc', 'molecule', 'year', 'date')) %>% 
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
  select(year, date, quarter, province, city, district, pchc, atc4, nfc, molecule, 
         product, packid, units = units_imp, sales = sales_imp, flag)


##---- Result ----
imp.sh <- sh.model.data %>% 
  filter(quarter %in% c('2018Q1'), flag == 1) %>% 
  mutate(flag = 0) %>% 
  bind_rows(sh.predict.sales.18, sh.predict.sales.19, sh.predict.sales.20) %>% 
  group_by(year, date, quarter, province, city, district, pchc, atc4, nfc, 
           molecule, product, packid, flag) %>% 
  summarise(units = sum(units, na.rm = TRUE),
            sales = sum(sales, na.rm = TRUE)) %>% 
  ungroup() %>% 
  filter(units > 0, sales > 0) %>% 
  select(year, date, quarter, province, city, district, pchc, atc4, nfc, 
         molecule, product, packid, units, sales, flag)

write.xlsx(imp.sh, '03_Outputs/02_Sanofi_Plavix_CHC_Imp_Shanghai.xlsx')
