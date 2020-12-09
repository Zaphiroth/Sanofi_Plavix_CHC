# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  Sanofi CMAX
# Purpose:      Summary
# programmer:   Zhe Liu
# Date:         2020-08-14
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##---- Readin ----
## product info
product.info <- read_csv("02_Inputs/2020 FULL DATABASE MTHLY CHPA-202009(全字段).CSV") %>% 
  filter(!is.na(PACK)) %>% 
  mutate(PACK = stri_pad_left(PACK, 7, 0)) %>% 
  distinct(PACK, `CORPORATE DESC`, `MANUF.TYPE DESC`, `MANUFACT. DESC`, 
           `APP FORM 1 SHORT DESC`, `APP FORM 1 DESC`, `TC I SHORT DESC`, 
           `TC I DESC`, `TC II SHORT DESC`, `TC II DESC`, `TC III SHORT DESC`, 
           `TC III DESC`, `TC IV SHORT DESC`, `ATC IV DESC`, `COMPS ABBR`, 
           `COMPS DESC`, `PRODUCT SHORT DESC`, `PRODUCT DESC`, `PACK SHORT DESC`, 
           `PACK DESC`)

## pack info
pack.info <- read.xlsx("02_Inputs/Product standardization master data-A-S-1021.xlsx") %>% 
  filter(nchar(PACK_ID) == 7) %>% 
  distinct(PACK_ID, PACK_SIZE = PACK)

## dosage info
dosage.info <- read.xlsx("02_Inputs/Master dosage workbook 20M09.xlsx", 
                         check.names = TRUE) %>% 
  distinct(PACK_CODE, DOSAGE = `Dosage..Dossage.unit.`) %>% 
  mutate(PACK_CODE = stri_pad_left(PACK_CODE, 7, 0))


##---- Market ----
proj.market <- market.def %>% 
  distinct(Market, Pack_ID) %>% 
  inner_join(proj.price, by = c('Pack_ID' = 'packid')) %>% 
  select(year, quarter, date, province, city, Market, atc4, nfc, 
         molecule, product, Pack_ID, price, units, sales)


##---- Result ----
## nation
plavix.chc <- proj.market %>% 
  group_by(YM = quarter, Market, PACK = Pack_ID) %>% 
  summarise(`Value(RMB)` = sum(sales, na.rm = TRUE),
            units = sum(units, na.rm = TRUE)) %>% 
  ungroup() %>% 
  left_join(product.info, by = "PACK") %>% 
  left_join(pack.info, by = c("PACK" = "PACK_ID")) %>% 
  left_join(dosage.info, by = c("PACK" = "PACK_CODE")) %>% 
  filter(!is.na(`CORPORATE DESC`)) %>% 
  mutate(Channel = "CHC",
         `Value(RMB)` = round(`Value(RMB)`, 2),
         `Volume(Dosage unit)` = units * PACK_SIZE,
         `T day` = round(`Volume(Dosage unit)` / DOSAGE), 
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

write.xlsx(plavix.chc, "03_Outputs/07_Sanofi_Plavix_CHC_2018Q1_2020Q3.xlsx")

## city
plavix.chc.city <- proj.market %>% 
  group_by(Province = province, City = city, YM = quarter, Market, PACK = Pack_ID) %>% 
  summarise(`Value(RMB)` = sum(sales, na.rm = TRUE),
            units = sum(units, na.rm = TRUE)) %>% 
  ungroup() %>% 
  left_join(product.info, by = "PACK") %>% 
  left_join(pack.info, by = c("PACK" = "PACK_ID")) %>% 
  left_join(dosage.info, by = c("PACK" = "PACK_CODE")) %>% 
  filter(!is.na(`CORPORATE DESC`)) %>% 
  mutate(Channel = "CHC",
         `Value(RMB)` = round(`Value(RMB)`, 2),
         `Volume(Dosage unit)` = units * PACK_SIZE,
         `T day` = round(`Volume(Dosage unit)` / DOSAGE), 
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
  select(Channel, Market, Province, City, Category, YM, `CORPORATE DESC`, 
         `MANUF.TYPE DESC`, `MANUFACT. DESC`, `APP FORM 1 SHORT DESC`, `APP FORM 1 DESC`, 
         `TC I SHORT DESC`, `TC I DESC`, `TC II SHORT DESC`, `TC II DESC`, 
         `TC III SHORT DESC`, `TC III DESC`, `TC IV SHORT DESC`, `ATC IV DESC`, 
         `COMPS ABBR`, `COMPS DESC`, `PRODUCT SHORT DESC`, `PRODUCT DESC`, 
         `PACK SHORT DESC`, `PACK DESC`, PACK, Measurement, Unit) %>% 
  arrange(YM, Market, Province, City, PACK, Measurement)

write.xlsx(plavix.chc.city, "03_Outputs/07_Sanofi_Plavix_CHC_City.xlsx")
