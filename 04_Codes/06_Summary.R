# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  Sanofi CMAX
# Purpose:      Summary
# programmer:   Zhe Liu
# Date:         2020-08-14
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##---- Readin ----
# product info
product.info.raw <- read_csv("02_Inputs/2020 FULL DATABASE MTHLY CHPA-202003(全字段).CSV")

product.info <- product.info.raw %>% 
  filter(!is.na(PACK)) %>% 
  mutate(PACK = stri_pad_left(PACK, 7, 0)) %>% 
  distinct(PACK, `CORPORATE DESC`, `MANUF.TYPE DESC`, `MANUFACT. DESC`, 
           `APP FORM 1 SHORT DESC`, `APP FORM 1 DESC`, `TC I SHORT DESC`, 
           `TC I DESC`, `TC II SHORT DESC`, `TC II DESC`, `TC III SHORT DESC`, 
           `TC III DESC`, `TC IV SHORT DESC`, `ATC IV DESC`, `COMPS ABBR`, 
           `COMPS DESC`, `PRODUCT SHORT DESC`, `PRODUCT DESC`, `PACK SHORT DESC`, 
           `PACK DESC`)

# pack info
pack.info <- read.xlsx("02_Inputs/Product standardization master data-A-S-0313.xlsx") %>% 
  filter(nchar(PACK_ID) == 7) %>% 
  distinct(PACK_ID, PACK_SIZE = PACK)

# dosage info
dosage.info <- read.xlsx("02_Inputs/Master dosage workbook 20M06.xlsx", 
                         check.names = TRUE) %>% 
  distinct(PACK_CODE, DOSAGE = `Dosage..Dossage.unit.`) %>% 
  mutate(PACK_CODE = stri_pad_left(PACK_CODE, 7, 0))


##---- Market ----
market1 <- proj.price %>% 
  filter(molecule %in% c("INSULIN GLARGINE", 
                         "INSULIN ASPART+INSULIN ASPART PROTAMINE CRYSTALLINE", 
                         "INSULIN LISPRO+INSULIN LISPRO PROTAMINE", 
                         "INSULIN DETEMIR", 
                         "INSULIN DEGLUDEC"), 
         stri_sub(nfc, 1, 1) == "G") %>% 
  mutate(market = "Basal Analog + Premix Analog")

market2 <- proj.price %>% 
  filter((stri_sub(atc4, 1, 4) == "A10S") | 
           (molecule %in% c("INSULIN GLARGINE", 
                            "INSULIN ASPART+INSULIN ASPART PROTAMINE CRYSTALLINE", 
                            "INSULIN LISPRO+INSULIN LISPRO PROTAMINE", 
                            "INSULIN DETEMIR", 
                            "INSULIN DEGLUDEC") & 
              stri_sub(nfc, 1, 1) == "G")) %>% 
  mutate(market = "Basal analog+premix analog+GLP-1")

market3 <- proj.price %>% 
  filter(atc4 %in% c("A10C5", "A10C2")) %>% 
  mutate(market = "Basal market")

market4 <- proj.price %>% 
  filter(product %in% c("LANTUS             AVS", "CHANG XIU LIN      B7G")) %>% 
  mutate(market = "Glargine Market")

market5 <- proj.price %>% 
  filter(stri_sub(atc4, 1, 4) == "A10S") %>% 
  mutate(market = "GLP-1")

market6 <- proj.price %>% 
  filter(stri_sub(atc4, 1, 4) %in% c("A10C") | atc4 %in% c("A10D0")) %>% 
  mutate(market = "Insulin Market")

market7 <- proj.price %>% 
  filter(stri_sub(atc4, 1, 4) %in% c("A10C") | atc4 %in% c("A10D0")) %>% 
  mutate(market = "Insulin Type")

market8 <- proj.price %>% 
  filter(atc4 == "A10C3") %>% 
  mutate(market = "Premix")

market9 <- proj.price %>% 
  filter(atc4 == "A10C1") %>% 
  mutate(market = "Rapid (Apidra)")

market10 <- proj.price %>% 
  filter(atc4 %in% c('A10C2', 'A10C3', 'A10C5')) %>% 
  mutate(market = 'Basal+Premix')

total.market <- bind_rows(market1, market2, market3, market4, market5, 
                          market6, market7, market8, market9, market10) %>% 
  select(year, quarter, month, province, city, market, atc4, nfc, 
         molecule, product, packid, price, units, sales)


##---- History ----
history.raw <- read.xlsx('02_Inputs/lantus_chc_2018Q1_2020Q1_0706.xlsx', 
                                check.names = FALSE)
colnames(history.raw) <- 
  c('Channel', 'Market', 'YM', 'CORPORATE DESC', 'MANUF.TYPE DESC', 
    'MANUFACT. DESC', 'APP FORM 1 SHORT DESC', 'APP FORM 1 DESC', 
    'TC I SHORT DESC', 'TC I DESC', 'TC II SHORT DESC', 'TC II DESC', 
    'TC III SHORT DESC', 'TC III DESC', 'TC IV SHORT DESC', 'ATC IV DESC', 
    'COMPS ABBR', 'COMPS DESC', 'PRODUCT SHORT DESC', 'PRODUCT DESC', 
    'PACK SHORT DESC', 'PACK DESC', 'PACK', 'Measurement', 'Unit')

# update market
history.lantus.chc <- history.raw %>% 
  filter(Market %in% c('Basal market', 'Premix')) %>% 
  mutate(Market = 'Basal+Premix') %>% 
  bind_rows(history.raw)


##---- Result ----
# update 2020Q1, 2020Q2
lantus.chc <- total.market %>% 
  group_by(YM = quarter, Market = market, PACK = packid) %>% 
  summarise(`Value(RMB)` = sum(sales, na.rm = TRUE),
            units = sum(units, na.rm = TRUE)) %>% 
  ungroup() %>% 
  left_join(product.info, by = "PACK") %>% 
  left_join(pack.info, by = c("PACK" = "PACK_ID")) %>% 
  left_join(dosage.info, by = c("PACK" = "PACK_CODE")) %>% 
  mutate(Channel = "CHC",
         `Value(RMB)` = round(`Value(RMB)`, 2),
         `Volume(Dosage unit)` = units * PACK_SIZE,
         `T day` = round(`Volume(Dosage unit)` / DOSAGE), 
         `Volume(Dosage unit)` = round(`Volume(Dosage unit)`)) %>% 
  pivot_longer(cols = c(`Value(RMB)`, `Volume(Dosage unit)`, `T day`), 
               names_to = 'Measurement', 
               values_to = 'Unit', 
               values_drop_na = FALSE) %>% 
  select(Channel, Market, YM, `CORPORATE DESC`, `MANUF.TYPE DESC`, `MANUFACT. DESC`, 
         `APP FORM 1 SHORT DESC`, `APP FORM 1 DESC`, `TC I SHORT DESC`, 
         `TC I DESC`, `TC II SHORT DESC`, `TC II DESC`, `TC III SHORT DESC`, 
         `TC III DESC`, `TC IV SHORT DESC`, `ATC IV DESC`, `COMPS ABBR`, 
         `COMPS DESC`, `PRODUCT SHORT DESC`, `PRODUCT DESC`, `PACK SHORT DESC`, 
         `PACK DESC`, PACK, Measurement, Unit) %>% 
  bind_rows(history.lantus.chc[history.lantus.chc$YM != '2020Q1', ]) %>% 
  pivot_wider(names_from = 'Measurement', values_from = 'Unit') %>% 
  left_join(dosage.info, by = c("PACK" = "PACK_CODE")) %>% 
  mutate(`T day` = round(`Volume(Dosage unit)` / DOSAGE), 
         `Volume(Dosage unit)` = round(`Volume(Dosage unit)`)) %>% 
  pivot_longer(cols = c(`Value(RMB)`, `Volume(Dosage unit)`, `T day`), 
               names_to = 'Measurement', 
               values_to = 'Unit', 
               values_drop_na = FALSE) %>% 
  mutate(Measurement = factor(Measurement, 
                              levels = c('Value(RMB)', 'Volume(Dosage unit)', 'T day')), 
         Category = case_when(
           `TC IV SHORT DESC` %in% c('A10C1') ~ 'Rapid', 
           `TC IV SHORT DESC` %in% c('A10C2', 'A10C4', 'A10C5') ~ 'Basal', 
           `TC IV SHORT DESC` %in% c('A10C3') ~ 'Premix', 
           `TC IV SHORT DESC` %in% c('A10D0') ~ 'Animal', 
           `TC IV SHORT DESC` %in% c('A10S0') ~ 'GLP-1', 
           TRUE ~ NA_character_
         )) %>% 
  select(Channel, Market, Category, YM, `CORPORATE DESC`, `MANUF.TYPE DESC`, 
         `MANUFACT. DESC`, `APP FORM 1 SHORT DESC`, `APP FORM 1 DESC`, 
         `TC I SHORT DESC`, `TC I DESC`, `TC II SHORT DESC`, `TC II DESC`, 
         `TC III SHORT DESC`, `TC III DESC`, `TC IV SHORT DESC`, `ATC IV DESC`, 
         `COMPS ABBR`, `COMPS DESC`, `PRODUCT SHORT DESC`, `PRODUCT DESC`, 
         `PACK SHORT DESC`, `PACK DESC`, PACK, Measurement, Unit) %>% 
  arrange(YM, Market, PACK, Measurement)

write.xlsx(lantus.chc, "03_Outputs/06_Sanofi_Lantus_CHC_Projection_2017Q1_2020Q2_v3.xlsx")

# update 2020Q2
lantus.chc1 <- total.market %>% 
  filter(quarter %in% c('2020Q2')) %>% 
  group_by(YM = quarter, Market = market, PACK = packid) %>% 
  summarise(`Value(RMB)` = sum(sales, na.rm = TRUE),
            units = sum(units, na.rm = TRUE)) %>% 
  ungroup() %>% 
  left_join(product.info, by = "PACK") %>% 
  left_join(pack.info, by = c("PACK" = "PACK_ID")) %>% 
  left_join(dosage.info, by = c("PACK" = "PACK_CODE")) %>% 
  mutate(Channel = "CHC",
         `Value(RMB)` = round(`Value(RMB)`, 2),
         `Volume(Dosage unit)` = units * PACK_SIZE,
         `T day` = round(`Volume(Dosage unit)` / DOSAGE), 
         `Volume(Dosage unit)` = round(`Volume(Dosage unit)`)) %>% 
  pivot_longer(cols = c(`Value(RMB)`, `Volume(Dosage unit)`, `T day`), 
               names_to = 'Measurement', 
               values_to = 'Unit', 
               values_drop_na = FALSE) %>% 
  select(Channel, Market, YM, `CORPORATE DESC`, `MANUF.TYPE DESC`, `MANUFACT. DESC`, 
         `APP FORM 1 SHORT DESC`, `APP FORM 1 DESC`, `TC I SHORT DESC`, 
         `TC I DESC`, `TC II SHORT DESC`, `TC II DESC`, `TC III SHORT DESC`, 
         `TC III DESC`, `TC IV SHORT DESC`, `ATC IV DESC`, `COMPS ABBR`, 
         `COMPS DESC`, `PRODUCT SHORT DESC`, `PRODUCT DESC`, `PACK SHORT DESC`, 
         `PACK DESC`, PACK, Measurement, Unit) %>% 
  bind_rows(history.lantus.chc) %>% 
  pivot_wider(names_from = 'Measurement', values_from = 'Unit') %>% 
  left_join(dosage.info, by = c("PACK" = "PACK_CODE")) %>% 
  mutate(`T day` = round(`Volume(Dosage unit)` / DOSAGE), 
         `Volume(Dosage unit)` = round(`Volume(Dosage unit)`)) %>% 
  pivot_longer(cols = c(`Value(RMB)`, `Volume(Dosage unit)`, `T day`), 
               names_to = 'Measurement', 
               values_to = 'Unit', 
               values_drop_na = FALSE) %>% 
  mutate(Measurement = factor(Measurement, 
                              levels = c('Value(RMB)', 'Volume(Dosage unit)', 'T day')), 
         Category = case_when(
           `TC IV SHORT DESC` %in% c('A10C1') ~ 'Rapid', 
           `TC IV SHORT DESC` %in% c('A10C2', 'A10C4', 'A10C5') ~ 'Basal', 
           `TC IV SHORT DESC` %in% c('A10C3') ~ 'Premix', 
           `TC IV SHORT DESC` %in% c('A10D0') ~ 'Animal', 
           `TC IV SHORT DESC` %in% c('A10S0') ~ 'GLP-1', 
           TRUE ~ NA_character_
         )) %>% 
  select(Channel, Market, Category, YM, `CORPORATE DESC`, `MANUF.TYPE DESC`, 
         `MANUFACT. DESC`, `APP FORM 1 SHORT DESC`, `APP FORM 1 DESC`, 
         `TC I SHORT DESC`, `TC I DESC`, `TC II SHORT DESC`, `TC II DESC`, 
         `TC III SHORT DESC`, `TC III DESC`, `TC IV SHORT DESC`, `ATC IV DESC`, 
         `COMPS ABBR`, `COMPS DESC`, `PRODUCT SHORT DESC`, `PRODUCT DESC`, 
         `PACK SHORT DESC`, `PACK DESC`, PACK, Measurement, Unit) %>% 
  arrange(YM, Market, PACK, Measurement)

write.xlsx(lantus.chc1, "03_Outputs/06_Sanofi_Lantus_CHC_Projection_2017Q1_2020Q2_m_v3.xlsx")


# lantus.chc %>% group_by(YM) %>% summarise(unit = sum(Unit))


lantus.chc.city <- total.market %>% 
  group_by(YM = quarter, Market = market, province, city, PACK = packid) %>% 
  summarise(`Value(RMB)` = sum(sales, na.rm = TRUE),
            units = sum(units, na.rm = TRUE)) %>% 
  ungroup() %>% 
  left_join(product.info, by = "PACK") %>% 
  left_join(pack.info, by = c("PACK" = "PACK_ID")) %>% 
  left_join(dosage.info, by = c("PACK" = "PACK_CODE")) %>% 
  mutate(Channel = "CHC",
         `Value(RMB)` = round(`Value(RMB)`, 2),
         `Volume(Dosage unit)` = units * PACK_SIZE,
         `T day` = round(`Volume(Dosage unit)` / DOSAGE), 
         `Volume(Dosage unit)` = round(`Volume(Dosage unit)`)) %>% 
  pivot_longer(cols = c(`Value(RMB)`, `Volume(Dosage unit)`, `T day`), 
               names_to = 'Measurement', 
               values_to = 'Unit', 
               values_drop_na = FALSE) %>% 
  select(Channel, Market, YM, province, city, `CORPORATE DESC`, `MANUF.TYPE DESC`, `MANUFACT. DESC`, 
         `APP FORM 1 SHORT DESC`, `APP FORM 1 DESC`, `TC I SHORT DESC`, 
         `TC I DESC`, `TC II SHORT DESC`, `TC II DESC`, `TC III SHORT DESC`, 
         `TC III DESC`, `TC IV SHORT DESC`, `ATC IV DESC`, `COMPS ABBR`, 
         `COMPS DESC`, `PRODUCT SHORT DESC`, `PRODUCT DESC`, `PACK SHORT DESC`, 
         `PACK DESC`, PACK, Measurement, Unit) %>% 
  pivot_wider(names_from = 'Measurement', values_from = 'Unit') %>% 
  left_join(dosage.info, by = c("PACK" = "PACK_CODE")) %>% 
  mutate(`T day` = round(`Volume(Dosage unit)` / DOSAGE), 
         `Volume(Dosage unit)` = round(`Volume(Dosage unit)`)) %>% 
  pivot_longer(cols = c(`Value(RMB)`, `Volume(Dosage unit)`, `T day`), 
               names_to = 'Measurement', 
               values_to = 'Unit', 
               values_drop_na = FALSE) %>% 
  mutate(Measurement = factor(Measurement, 
                              levels = c('Value(RMB)', 'Volume(Dosage unit)', 'T day')), 
         Category = case_when(
           `TC IV SHORT DESC` %in% c('A10C1') ~ 'Rapid', 
           `TC IV SHORT DESC` %in% c('A10C2', 'A10C4', 'A10C5') ~ 'Basal', 
           `TC IV SHORT DESC` %in% c('A10C3') ~ 'Premix', 
           `TC IV SHORT DESC` %in% c('A10D0') ~ 'Animal', 
           `TC IV SHORT DESC` %in% c('A10S0') ~ 'GLP-1', 
           TRUE ~ NA_character_
         )) %>% 
  select(Channel, Market, Category, YM, province, city, `CORPORATE DESC`, `MANUF.TYPE DESC`, 
         `MANUFACT. DESC`, `APP FORM 1 SHORT DESC`, `APP FORM 1 DESC`, 
         `TC I SHORT DESC`, `TC I DESC`, `TC II SHORT DESC`, `TC II DESC`, 
         `TC III SHORT DESC`, `TC III DESC`, `TC IV SHORT DESC`, `ATC IV DESC`, 
         `COMPS ABBR`, `COMPS DESC`, `PRODUCT SHORT DESC`, `PRODUCT DESC`, 
         `PACK SHORT DESC`, `PACK DESC`, PACK, Measurement, Unit) %>% 
  arrange(YM, Market, PACK, Measurement)

write.xlsx(lantus.chc.city, "03_Outputs/Sanofi_Lantus_CHC_Province.xlsx")







