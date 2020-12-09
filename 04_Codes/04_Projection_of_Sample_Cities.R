# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  Sanofi CMAX
# Purpose:      Projection of Sample Cities
# programmer:   Zhe Liu
# Date:         2020-12-09
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##---- Universe info ----
## PCHC
pchc.universe <- read.xlsx('02_Inputs/Universe_PCHCCode_20201208.xlsx', sheet = 'PCHC')

hospital.universe <- pchc.universe %>% 
  group_by(pchc = PCHC_Code) %>% 
  summarise(province = first(na.omit(`省`)),
            city = first(na.omit(`地级市`)),
            district = first(na.omit(`区[县/县级市】`)),
            pop = first(na.omit(`人口`)),
            est = first(na.omit(`其中：西药药品收入（千元）`))) %>% 
  ungroup() %>% 
  filter(!is.na(est), !is.na(pop))

## segment
proj.segment <- read.xlsx('02_Inputs/seg_45cities.xlsx') %>% 
  mutate(seg_city = if_else(city == '上海', stri_paste(city, district), city)) %>% 
  select(seg_city, segment = seg_up)

## sampel PCHC
sample.pchc.list <- unique(imp.total$pchc)

## universe PCHC
universe.pchc <- bind_rows(imp.total, hospital.universe) %>% 
  group_by(pchc) %>% 
  summarise(province = first(na.omit(province)), 
            city = first(na.omit(city)), 
            district = first(na.omit(district))) %>% 
  ungroup() %>% 
  select(province, city, district, pchc)


##---- Projection ----
## universe PCHC set
universe.set <- imp.total %>% 
  distinct(year, quarter, date, province, city, atc4, nfc, molecule, product, packid) %>% 
  left_join(universe.pchc, by = c('province', 'city')) %>% 
  mutate(seg_city = if_else(city == '上海', stri_paste(city, district), city)) %>% 
  left_join(proj.segment, by = 'seg_city') %>% 
  left_join(imp.total, by = c('year', 'date', 'quarter', 'province', 'city', 
                              'district', 'pchc', 'atc4', 'nfc', 'molecule', 
                              'product', 'packid')) %>% 
  left_join(hospital.universe[, c('pchc', 'est')], by = 'pchc') %>% 
  mutate(segment = if_else(is.na(segment), 1, segment),
         sales = if_else(is.na(sales) & pchc %in% sample.pchc.list, 0, sales))

## projection parameter
proj.parm <- data.table(universe.set)[, {
  ux <- mean(est, na.rm = TRUE)
  uy <- mean(sales, na.rm = TRUE)
  slope <- uy / ux
  intercept <- 0
  predict_sales = est * slope
  spearman_cor <- cor(sales, predict_sales, method = 'spearman')
  list(slope = slope, intercept = intercept, spearman_cor = spearman_cor)
}, by = list(date, segment, packid)]

## projection result
proj.sample <- universe.set %>% 
  left_join(proj.parm, by = c('date', 'segment', 'packid')) %>% 
  mutate(predict_sales = est * slope,
         predict_sales = if_else(predict_sales < 0, 0, predict_sales),
         final_sales = if_else(is.na(sales), predict_sales, sales)) %>% 
  filter(final_sales > 0) %>% 
  group_by(year, date, quarter, province, city, district, atc4, nfc, molecule, 
           product, packid) %>% 
  summarise(sales = sum(final_sales, na.rm = TRUE)) %>% 
  ungroup() %>% 
  select(year, date, quarter, province, city, district, atc4, nfc, molecule, 
         product, packid, sales)

write.xlsx(proj.sample, '03_Outputs/04_Sanofi_Plavix_CHC_Proj_Sample.xlsx')
