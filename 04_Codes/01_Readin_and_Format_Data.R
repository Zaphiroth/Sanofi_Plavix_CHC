# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  Sanofi CMAX
# Purpose:      Readin Raw Data
# programmer:   Zhe Liu
# Date:         2020-12-07
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##---- Mapping table ----
## PCHC code
pchc.universe <- read.xlsx("02_Inputs/Universe_PCHCCode_20201201.xlsx", sheet = "PCHC")

pchc.mapping1 <- pchc.universe %>% 
  filter(!is.na(`单位名称`), !is.na(PCHC_Code)) %>% 
  group_by(province = `省`, city = `地级市`, district = `区[县/县级市】`, hospital = `单位名称`) %>% 
  summarise(pchc = first(PCHC_Code)) %>% 
  ungroup()

pchc.mapping2 <- pchc.universe %>% 
  filter(!is.na(ZS_Servier.name), !is.na(PCHC_Code)) %>% 
  group_by(province = `省`, city = `地级市`, district = `区[县/县级市】`, hospital = `ZS_Servier.name`) %>% 
  summarise(pchc = first(PCHC_Code)) %>% 
  ungroup()

pchc.mapping3 <- bind_rows(pchc.mapping1, pchc.mapping2) %>% 
  distinct(province, city, district, hospital, pchc)

pchc.mapping4 <- pchc.mapping3 %>% 
  group_by(pchc) %>% 
  summarise(province = first(na.omit(province)),
            city = first(na.omit(city)),
            district = first(na.omit(district))) %>% 
  ungroup()

## IMS info
ims_prod_ref <- fread("02_Inputs/cn_prod_ref_201912_1.txt") %>% 
  setDF() %>% 
  mutate(Pack_Id = str_pad(Pack_Id, 7, "left", pad = "0")) %>% 
  select(Pack_Id, NFC123_Code)

ims.mol.raw <- read.xlsx("02_Inputs/ims_chpa_to20Q3.xlsx", startRow = 3, cols = 1:21)

ims.mol1 <- ims.mol.raw[, 1:21] %>% 
  distinct() %>% 
  filter(!is.na(Pack_Id)) %>% 
  left_join(ims_prod_ref, by = "Pack_Id") %>% 
  select(packid = Pack_ID, Corp_ID, Corp_Desc, MNF_TYPE, MnfType_Desc, 
         Mnf_Desc, ATC4_Code, NFC123_Code, Prd_desc, Pck_Desc, 
         Molecule_Desc)

ims_prod_ref <- fread("02_Inputs/cn_prod_ref_201912_1.txt") %>% 
  setDF() %>% 
  mutate(Pack_Id = str_pad(Pack_Id, 7, "left", pad = "0"))

ims_mol_lkp_ref <- fread("02_Inputs/cn_mol_lkp_201912_1.txt") %>%
  setDF() %>%
  arrange(Pack_ID, Molecule_ID) %>%
  mutate(Pack_ID  = str_pad(Pack_ID , 7, "left", pad = "0"))

ims_mol_ref <- fread("02_Inputs/cn_mol_ref_201912_1.txt")

ims_corp_ref <- fread("02_Inputs/cn_corp_ref_201912_1.txt")

ims.mol2 <- ims_mol_lkp_ref %>%
  left_join(ims_mol_ref, by = c("Molecule_ID" = "Molecule_Id")) %>%
  arrange(Pack_ID, Molecule_Desc) %>%
  group_by(Pack_ID) %>%
  summarise(Molecule_Desc = paste(Molecule_Desc, collapse = "+")) %>%
  ungroup() %>%
  left_join(ims_prod_ref, by = c("Pack_ID" = "Pack_Id")) %>%
  left_join(ims_corp_ref, by = "Corp_ID") %>% 
  select(packid = Pack_ID, Corp_ID, Corp_Desc, ATC4_Code, NFC123_Code,
         Prd_desc, Pck_Desc, Molecule_Desc)

ims.mol <- ims.mol2 %>% 
  filter(!(packid %in% ims.mol1$packid)) %>% 
  mutate(Corp_ID = stri_pad_left(Corp_ID, 4, 0)) %>% 
  bind_rows(ims.mol1)

## market definition
market.def <- read.xlsx('02_Inputs/Market_Plavix.xlsx')


##---- Formatting raw data ----
## Anhui, Beijing, Fujian, Jiangsu, Shandong, Zhejiang
raw.list <- map(list.files('02_Inputs/data', pattern = '*.xlsx', full.names = TRUE), 
                function(x) {
                  read.xlsx(x) %>% 
                    mutate(Year = as.character(Year), 
                           Month = as.character(Month), 
                           Prd_desc_ZB = as.character(Prd_desc_ZB))
                })

raw.data <- bind_rows(raw.list) %>% 
  distinct(year = as.character(Year), 
           quarter = Quarter, 
           date = as.character(Month), 
           province = gsub('省|市', '', Province), 
           city = if_else(City == "市辖区", "北京", gsub("市", "", City)), 
           district = County, 
           hospital = Hospital_Name, 
           packid = stri_pad_left(packcode, 7, 0), 
           units = if_else(is.na(Volume), Value / Price, Volume), 
           sales = Value) %>% 
  left_join(pchc.mapping3, by = c('province', 'city', 'district', 'hospital')) %>% 
  filter(!is.na(pchc), !is.na(packid), 
         year %in% c('2018', '2019', '2020'), !(quarter %in% c('2020Q4'))) %>% 
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

## Guangzhou
raw.gz1 <- read.csv('02_Inputs/data/广州/gzs 20q1_date.csv')
raw.gz2 <- read.xlsx('02_Inputs/data/广州/2jd_date.xlsx')

raw.gz <- raw.gz1 %>% 
  select(-period) %>% 
  mutate(date = as.character(date)) %>% 
  bind_rows(raw.gz2) %>% 
  mutate(quarter_m = stri_sub(date, 5, 6)) %>% 
  distinct(year = stri_sub(date, 1, 4), 
           quarter = ifelse(quarter_m %in% c("01", "02", "03"), 
                            stri_paste(year, "Q1"), 
                            ifelse(quarter_m %in% c("04", "05", "06"), 
                                   stri_paste(year, "Q2"), 
                                   ifelse(quarter_m %in% c("07", "08", "09"), 
                                          stri_paste(year, "Q3"), 
                                          ifelse(quarter_m %in% c("10", "11", "12"), 
                                                 stri_paste(year, "Q4"), 
                                                 year)))), 
           date = stri_sub(date, 1, 6), 
           province = '广东', 
           city = '广州', 
           hospital = name, 
           packid = stri_pad_left(pfc, 7, 0), 
           price = value / unit, 
           units = unit, 
           sales = value) %>% 
  left_join(pchc.mapping3, by = c('province', 'city', 'hospital')) %>% 
  filter(!is.na(pchc), !is.na(packid), 
         year %in% c('2018', '2019', '2020'), !(quarter %in% c('2020Q4'))) %>% 
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

## Shanghai

# Guangzhou


# servier
raw.ah1 <- read_xlsx('02_Inputs/data/Servier_ah_CHC_202006.xlsx')
raw.bj1 <- read_xlsx('02_Inputs/data/Servier_bj_CHC_2020Q2.xlsx')
raw.js1 <- read_xlsx('02_Inputs/data/Servier_js_CHC_2020Q1Q2.xlsx')
raw.zj1 <- read_xlsx('02_Inputs/data/Servier_zj_CHC_2020Q1Q2.xlsx')
raw.fjsd1 <- read_xlsx('02_Inputs/data/Servier_fjsd_CHC_2020Q1Q2(predicted by all_raw_data_packid_Servier_171819_CHC_m_v4).xlsx')

raw.servier <- bind_rows(raw.ah1, raw.bj1, raw.js1, raw.zj1) %>% 
  mutate(Year = as.character(Year), 
         Month = as.character(Month)) %>% 
  bind_rows(raw.fjsd1) %>% 
  distinct(year = as.character(Year), 
           quarter = Quarter, 
           month = as.character(Month), 
           province = gsub('省|市', '', Province), 
           city = if_else(City == "市辖区", "北京", gsub("市", "", City)), 
           district = County, 
           hospital = Hospital_Name, 
           atc4 = ATC4_Code, 
           nfc = NFC123_Code, 
           molecule = Molecule_Desc, 
           product = Prd_desc, 
           packid = stri_pad_left(packcode, 7, 0), 
           price = Price, 
           units = if_else(is.na(Volume), Value / Price, Volume), 
           sales = Value) %>% 
  left_join(pchc.mapping3, by = c('province', 'city', 'district', 'hospital')) %>% 
  filter(!is.na(pchc), pchc != '#N/A', units > 0, sales > 0) %>% 
  bind_rows(raw.gz) %>% 
  filter(stri_sub(atc4, 1, 4) %in% c('A10S')) %>% 
  group_by(year, quarter, month, pchc, atc4, nfc, molecule, product, packid) %>% 
  summarise(province = first(na.omit(province)), 
            city = first(na.omit(city)), 
            district = first(na.omit(district)), 
            units = sum(units, na.rm = TRUE), 
            sales = sum(sales, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(price = sales / units)

# total
raw.ah <- read.xlsx('02_Inputs/data/胰岛素_ah_CHC_202006.xlsx')
raw.bj <- read.xlsx('02_Inputs/data/胰岛素_bj_CHC_2020Q2.xlsx')
raw.js <- read.xlsx('02_Inputs/data/胰岛素_js_CHC_2020Q1Q2.xlsx')
raw.zj <- read.xlsx('02_Inputs/data/胰岛素_zj_CHC_2020Q1Q2.xlsx')
raw.fjsd <- read.xlsx('02_Inputs/data/胰岛素_fjsd_CHC_2020Q1Q2(predicted by yidaosu171819_m2_packid_moleinfo).xlsx')

raw.total <- bind_rows(raw.ah, raw.bj, raw.js, raw.zj) %>% 
  mutate(Year = as.character(Year), 
         Month = as.character(Month)) %>% 
  bind_rows(raw.fjsd) %>% 
  distinct(year = as.character(Year), 
           quarter = Quarter, 
           month = as.character(Month), 
           province = gsub('省|市', '', Province), 
           city = if_else(City == "市辖区", "北京", gsub("市", "", City)), 
           district = County, 
           hospital = Hospital_Name, 
           atc4 = ATC4_Code, 
           nfc = NFC123_Code, 
           molecule = Molecule_Desc, 
           product = Prd_desc, 
           packid = stri_pad_left(packcode, 7, 0), 
           price = Price, 
           units = if_else(is.na(Volume), Value / Price, Volume), 
           sales = Value) %>% 
  left_join(pchc.mapping3, by = c('province', 'city', 'district', 'hospital')) %>% 
  filter(!is.na(pchc), pchc != '#N/A', units > 0, sales > 0) %>% 
  bind_rows(raw.gz) %>% 
  filter(stri_sub(atc4, 1, 4) %in% c('A10C', 'A10D')) %>% 
  group_by(year, quarter, month, pchc, atc4, nfc, molecule, product, packid) %>% 
  summarise(province = first(na.omit(province)), 
            city = first(na.omit(city)), 
            district = first(na.omit(district)), 
            units = sum(units, na.rm = TRUE), 
            sales = sum(sales, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(price = sales / units) %>% 
  bind_rows(raw.servier)

write.xlsx(raw.total, '03_Outputs/01_Sanofi_Lantus_2020Q1Q2_Raw.xlsx')

# chk <- raw.servier %>%
#   filter(is.na(pchc)) %>%
#   distinct(province, city, district, hospital)
# 
# write.xlsx(chk, '05_Internal_Review/pchc_check.xlsx')








