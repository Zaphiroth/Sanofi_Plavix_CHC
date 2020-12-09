# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  Sanofi CMAX
# Purpose:      Review
# programmer:   Zhe Liu
# Date:         2020-10-28
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##---- Check SOP -----
## CHPA
chpa.format <- read.xlsx('05_Internal_Review/ims_chpa_to20Q3_format.xlsx')

market.pack <- market.def %>% 
  distinct(Pack_ID, Market)

plavix.chpa <- chpa.format %>% 
  pivot_longer(cols = c(ends_with('UNIT'), ends_with('RENMINBI')), 
               names_to = 'quarter', 
               values_to = 'value') %>% 
  separate(quarter, c('quarter', 'measure'), sep = '_') %>% 
  pivot_wider(id_cols = c(Pack_ID, ATC4_Code, Molecule_Desc, Prd_desc, 
                          Pck_Desc, Corp_Desc, quarter), 
              names_from = measure, 
              values_from = value) %>% 
  left_join(market.pack, by = 'Pack_ID') %>% 
  filter(!is.na(Market), 
         stri_sub(quarter, 1, 4) %in% c('2018', '2019', '2020')) %>% 
  select(Pack_ID, YQ = quarter, `ATC Code IV` = ATC4_Code, Market, 
         Mole_Ename = Molecule_Desc, Prod_Ename = Prd_desc, Pack_DESC = Pck_Desc, 
         Corp_EName = Corp_Desc, `Total Unit` = UNIT, `Value (RMB)` = RENMINBI)

write.xlsx(plavix.chpa, '05_Internal_Review/Plavix_CHPA_2018Q1_2020Q3.xlsx')

## Update
chpa.info <- plavix.chpa %>% 
  distinct(Pack_ID, `ATC Code IV`, Mole_Ename, Prod_Ename, Pack_DESC, Corp_EName)

delivery.update <- plavix.chc %>% 
  distinct(Pack_ID = PACK, YQ = YM, Market, Measurement, Unit) %>% 
  pivot_wider(names_from = Measurement, 
              values_from = Unit) %>% 
  left_join(chpa.info, by = 'Pack_ID') %>% 
  filter(!is.na(`ATC Code IV`), stri_sub(YQ, 1, 4) %in% c('2018', '2019', '2020'))

write.xlsx(delivery.update, '05_Internal_Review/Plavix_Delivery_Updated.xlsx')





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



