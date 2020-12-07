# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  Sanofi CMAX
# Purpose:      Imputation of Shanghai
# programmer:   Zhe Liu
# Date:         2020-08-14
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##---- Sample data ----
# Shanghai
history.sh1 <- read.xlsx("02_Inputs/data/上海_2017.xlsx")
history.sh2 <- read.xlsx("02_Inputs/data/上海_2018.xlsx")

history.sh <- bind_rows(history.sh1, history.sh2) %>% 
  mutate(quarter_m = stri_sub(Date, 5, 6)) %>% 
  distinct(year = stri_sub(Date, 1, 4), 
           quarter = ifelse(quarter_m %in% c("01", "02", "03"), 
                            stri_paste(year, "Q1"), 
                            ifelse(quarter_m %in% c("04", "05", "06"), 
                                   stri_paste(year, "Q2"), 
                                   ifelse(quarter_m %in% c("07", "08", "09"), 
                                          stri_paste(year, "Q3"), 
                                          ifelse(quarter_m %in% c("10", "11", "12"), 
                                                 stri_paste(year, "Q4"), 
                                                 year)))), 
           month = as.character(Date), 
           province = "上海", 
           city = "上海", 
           pchc = PCHC, 
           packid = pfc, 
           price = value / unit, 
           units = unit, 
           sales = value) %>% 
  left_join(pchc.mapping4, by = c('province', 'city', 'pchc')) %>% 
  left_join(ims.mol, by = "packid") %>% 
  filter(!is.na(pchc), pchc != '#N/A', !is.na(atc4), units > 0, sales > 0) %>% 
  select(year, quarter, month, province, city, district, pchc, atc4, nfc, 
         molecule, product, packid, price, units, sales)

# Beijing
history.total <- read.csv("02_Inputs/data/all_raw_data_packid_171819_CHC_fj19.csv")

history.bj <- history.total %>% 
  mutate(year = as.character(Year),
         month = as.character(Month),
         quarter = `季度`,
         province = `省份`,
         city = if_else(`城市` == "市辖区", "北京", gsub("市", "", `城市`)),
         district = `区县`,
         hospital = `医院名称`,
         packid = stri_pad_left(packcode_m, 7, 0)) %>% 
  filter(city == '北京') %>% 
  distinct() %>% 
  left_join(pchc.mapping3, by = c("province", "city", "district", "hospital")) %>% 
  filter(!is.na(pchc)) %>% 
  select(year, month, quarter, province, city, district, pchc, atc4 = ATC4_Code, 
         nfc = NFC123_Code, molecule = Molecule_Desc, product = Prd_desc, packid, 
         price = `采购价`, units = `采购数量`, sales = `采购金额`)


##---- Model ----
# model set
sh.model.data <- bind_rows(history.sh, history.bj, raw.total) %>% 
  filter(province %in% c("上海", "北京"), 
         stri_sub(atc4, 1, 4) %in% c('A10C', 'A10D', 'A10S')) %>% 
  mutate(flag = if_else(province == "上海", 1, 0))

sh.model.set <- sh.model.data %>% 
  filter(quarter %in% c("2017Q1", "2017Q2", "2017Q3", "2017Q4", "2018Q1")) %>% 
  group_by(province, city, district, pchc, month, flag) %>% 
  summarise(sales = sum(sales, na.rm = TRUE)) %>% 
  ungroup() %>% 
  setDT() %>% 
  dcast(province + city + district + pchc + flag ~ month, value.var = "sales", fill = 0)

# model
sh.train.set <- sh.model.set[sh.model.set$flag == 0, ]
sh.test.set <- sh.model.set[sh.model.set$flag == 1, ]

sh.model.train <- sh.train.set[, -c("province", "city", 'district', "pchc")]
sh.model.test <- sh.test.set[, -c("province", "city", 'district', "pchc")]

sh.knn.model <- kknn(flag ~ ., train = sh.model.train, test = sh.model.test, k = 3, scale = TRUE)

# model weightage
sh.model.indice <- as.data.frame(sh.knn.model$C) %>%
  lapply(function(x) {
    sh.train.set$pchc[x]
  }) %>%
  as.data.frame(col.names = c("pchc_1", "pchc_2", "pchc_3")) %>%
  bind_cols(sh.test.set[, c("province", "city", 'district', "pchc")]) %>%
  setDT() %>%
  melt(id.vars = c("province", "city", 'district', "pchc"), 
       variable.name = "knn_level", value.name = "knn_pchc")

sh.model.weight <- as.data.frame(sh.knn.model$D) %>%
  lapply(function(x) {
    1 / (x + 1)
  }) %>%
  as.data.frame(col.names = c("pchc_1", "pchc_2", "pchc_3")) %>%
  mutate(weight_sum = pchc_1 + pchc_2 + pchc_3,
         pchc_1 = pchc_1 / weight_sum,
         pchc_2 = pchc_2 / weight_sum,
         pchc_3 = pchc_3 / weight_sum) %>%
  bind_cols(sh.test.set[, c("province", "city", 'district', "pchc")]) %>%
  select(-weight_sum) %>%
  setDT() %>%
  melt(id.vars = c("province", "city", 'district', "pchc"), 
       variable.name = "knn_level", value.name = "knn_weight")


##---- Growth ----
sh.model.growth <- sh.model.data %>% 
  filter(flag == 0) %>% 
  group_by(knn_pchc = pchc, molecule, year, month, quarter) %>% 
  summarise(sales = sum(sales, na.rm = TRUE)) %>% 
  ungroup() %>% 
  inner_join(sh.model.indice, by = "knn_pchc") %>% 
  left_join(sh.model.weight, 
            by = c("province", "city", 'district', "pchc", "knn_level")) %>% 
  group_by(pchc, molecule, year, month, quarter) %>% 
  summarise(sales = sum(sales * knn_weight, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(month = stri_sub(month, 5, 6)) %>% 
  setDT() %>% 
  dcast(pchc + molecule + month ~ year, value.var = "sales", fill = 0) %>% 
  mutate(growth_1718 = `2018` / `2017`, 
         growth_1819 = `2019` / `2018`, 
         growth_1920 = `2020` / `2019`) %>% 
  select(pchc, molecule, month, `2017` = growth_1718, `2018` = growth_1819, 
         `2019` = growth_1920) %>% 
  setDT() %>% 
  melt(id.vars = c("pchc", "molecule", "month"), 
       measure.vars = c("2017", "2018", "2019"), 
       variable.name = "year", value.name = "growth", 
       variable.factor = FALSE) %>% 
  filter(!is.na(growth), !is.infinite(growth)) %>% 
  mutate(month = stri_paste(year, month))


##---- Prediction ----
sh.predict.sales.18 <- sh.model.data %>% 
  filter(quarter %in% c("2017Q2", "2017Q3", "2017Q4"), flag == 1) %>% 
  left_join(sh.model.growth, by = c("pchc", "molecule", "year", "month")) %>% 
  mutate(growth = if_else(is.na(growth), 1, growth),
         growth = if_else(growth > 3, 3, growth),
         growth = if_else(growth > quantile(growth, 0.9),
                          mean(growth[growth >= quantile(growth, 0.25) & 
                                        growth <= quantile(growth, 0.75)]),
                          growth)) %>% 
  mutate(units_imp = units * growth,
         sales_imp = sales * growth,
         month = gsub("2017", "2018", month),
         quarter = gsub("2017", "2018", quarter),
         year = "2018",
         flag = 1) %>% 
  select(year, month, quarter, province, city, district, pchc, atc4, nfc, molecule, 
         product, packid, price, units = units_imp, sales = sales_imp, flag)

sh.predict.sales.19 <- sh.model.data %>% 
  filter(quarter %in% c("2018Q1"), flag == 1) %>% 
  bind_rows(sh.predict.sales.18) %>% 
  left_join(sh.model.growth, by = c("pchc", "molecule", "year", "month")) %>% 
  mutate(growth = if_else(is.na(growth), 1, growth),
         growth = if_else(growth > 3, 3, growth),
         growth = if_else(growth > quantile(growth, 0.9),
                          mean(growth[growth >= quantile(growth, 0.25) & 
                                        growth <= quantile(growth, 0.75)]),
                          growth)) %>% 
  mutate(units_imp = units * growth,
         sales_imp = sales * growth,
         month = gsub("2018", "2019", month),
         quarter = gsub("2018", "2019", quarter),
         year = "2019",
         flag = 1) %>% 
  select(year, month, quarter, province, city, district, pchc, atc4, nfc, molecule, 
         product, packid, price, units = units_imp, sales = sales_imp, flag)

sh.predict.sales.20 <- sh.predict.sales.19 %>% 
  filter(quarter %in% c('2019Q1', '2019Q2'), flag == 1) %>% 
  left_join(sh.model.growth, by = c("pchc", "molecule", "year", "month")) %>% 
  mutate(growth = if_else(is.na(growth), 1, growth),
         growth = if_else(growth > 3, 3, growth),
         growth = if_else(growth > quantile(growth, 0.9),
                          mean(growth[growth >= quantile(growth, 0.25) & 
                                        growth <= quantile(growth, 0.75)]),
                          growth)) %>% 
  mutate(units_imp = units * growth,
         sales_imp = sales * growth,
         month = gsub("2019", "2020", month),
         quarter = gsub("2019", "2020", quarter),
         year = "2020",
         flag = 1) %>% 
  select(year, month, quarter, province, city, district, pchc, atc4, nfc, molecule, 
         product, packid, price, units = units_imp, sales = sales_imp, flag)


##---- Result ----
sh.imp <- sh.predict.sales.20 %>% 
  filter(stri_sub(atc4, 1, 4) %in% c('A10C', 'A10D', 'A10S')) %>% 
  mutate(flag = ifelse(is.na(flag), 0, flag)) %>% 
  group_by(year, month, quarter, pchc, atc4, nfc, 
           molecule, product, packid, flag) %>% 
  summarise(province = first(na.omit(province)), 
            city = first(na.omit(city)), 
            district = first(na.omit(district)), 
            units = sum(units, na.rm = TRUE),
            sales = sum(sales, na.rm = TRUE)) %>% 
  ungroup() %>% 
  filter(units > 0, sales > 0) %>% 
  mutate(price = sales / units) %>% 
  select(year, month, quarter, province, city, district, pchc, atc4, nfc, 
         molecule, product, packid, price, units, sales, flag)

write.xlsx(sh.imp, "03_Outputs/02_Imputation_of_Shanghai.xlsx")


