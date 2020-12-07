# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  Sanofi CMAX
# Purpose:      Review
# programmer:   Zhe Liu
# Date:         2020-10-28
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##---- Projection rate ----
raw.2019 <- read_feather('02_Inputs/review/01_Total_Market_Data.feather')
proj.2019 <- read_feather('02_Inputs/review/05_Projection_with_Price.feather')

raw.city <- raw.2019 %>% 
  select(-market) %>% 
  distinct() %>% 
  bind_rows(raw.total) %>% 
  arrange(quarter, city) %>% 
  group_by(quarter, province, city, product, packid) %>% 
  summarise(units_raw = sum(units, na.rm = TRUE), 
            sales_raw = sum(sales, na.rm = TRUE)) %>% 
  ungroup()

proj.city <- proj.2019 %>% 
  select(-market) %>% 
  distinct() %>% 
  bind_rows(proj.price) %>% 
  arrange(quarter, city) %>% 
  group_by(quarter, province, city, product, packid) %>% 
  summarise(units_proj = sum(units, na.rm = TRUE), 
            sales_proj = sum(sales, na.rm = TRUE)) %>% 
  ungroup()

market.def <- total.market %>% 
  distinct(packid, market)

proj.rate <- full_join(raw.city, proj.city, 
                       by = c("quarter", "province", "city", "product", "packid")) %>% 
  filter(stri_sub(quarter, 1, 4) >= '2018', packid %in% lantus.chc$PACK) %>% 
  group_by(quarter, province, city, product) %>% 
  summarise(sales_raw = sum(sales_raw, na.rm = TRUE), 
            sales_proj = sum(sales_proj, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(sales_rate = sales_proj / sales_raw) %>% 
  arrange(quarter, province, city, product)

write.xlsx(proj.rate, '05_Internal_Review/Lantus_CHC_Projection_Rate.xlsx')



