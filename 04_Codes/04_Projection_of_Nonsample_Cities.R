# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  Sanofi CMAX
# Purpose:      Projection of Non-sample Cities
# programmer:   Zhe Liu
# Date:         2020-08-14
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##---- Universe info ----
# city tier
city.tier <- read.xlsx("02_Inputs/pchc_city_tier.xlsx") %>% 
  group_by(city) %>% 
  mutate(tier = ifelse(is.na(city_tier), first(na.omit(city_tier)), city_tier)) %>% 
  ungroup() %>% 
  mutate(tier = ifelse(is.na(tier), 5, tier)) %>% 
  distinct(city, tier)

# universe PCHC
universe.city <- pchc.universe %>% 
  group_by(pchc = PCHC_Code) %>% 
  summarise(province = first(na.omit(`省`)),
            city = first(na.omit(`地级市`)),
            district = first(na.omit(`区[县/县级市】`)),
            pop = first(na.omit(`人口`)),
            est = first(na.omit(`其中：西药药品收入（千元）`))) %>% 
  ungroup() %>% 
  filter(!is.na(est), !is.na(pop)) %>% 
  left_join(city.tier, by = "city") %>% 
  mutate(tier = ifelse(is.na(tier), 1, tier)) %>% 
  group_by(province, city, tier) %>% 
  summarise(pop = sum(pop, na.rm = TRUE),
            est = sum(est, na.rm = TRUE)) %>% 
  ungroup()

# universe district
proj.market.nation <- proj.sample.m %>% 
  left_join(city.tier, by = "city") %>% 
  mutate(tier = ifelse(is.na(tier), 1, tier)) %>% 
  group_by(year, month, quarter, province, city, tier, atc4, nfc, molecule, 
           product, packid) %>% 
  summarise(sales = sum(sales, na.rm = TRUE)) %>% 
  ungroup() %>% 
  inner_join(universe.city, by = c("province", "city", "tier"))


#---- Projection ----
proj.region.list <- vector("list", length = nrow(universe.city))

pb <- txtProgressBar(min = 1, max = nrow(universe.city), initial = 1) 

for (i in 1:nrow(universe.city)) {
  setTxtProgressBar(pb, i)
  
  proj.region.list[[i]] <- universe.city[i, ] %>% 
    left_join(proj.market.nation, by = c("tier")) %>% 
    mutate(est_gap = abs(est.x - est.y)) %>% 
    filter(est_gap <= min(est_gap)) %>% 
    mutate(slope = ifelse(is.infinite(est.x / est.y) | is.na(est.x / est.y), 
                          1, 
                          est.x / est.y)) %>% 
    select(month, province = province.x, city = city.x, tier, atc4, nfc, 
           molecule, product, packid, sales, est.x, est.y, slope)
}

proj.universe <- proj.region.list %>% 
  bind_rows() %>% 
  mutate(slope = ifelse(slope > quantile(slope, 0.9), quantile(slope, 0.9), slope),
         final_sales = sales * slope, 
         year = stri_sub(month, 1, 4), 
         quarter_m = stri_sub(month, 5, 6), 
         quarter = ifelse(quarter_m %in% c("01", "02", "03"), 
                          stri_paste(year, "Q1"), 
                          ifelse(quarter_m %in% c("04", "05", "06"), 
                                 stri_paste(year, "Q2"), 
                                 ifelse(quarter_m %in% c("07", "08", "09"), 
                                        stri_paste(year, "Q3"), 
                                        ifelse(quarter_m %in% c("10", "11", "12"), 
                                               stri_paste(year, "Q4"), 
                                               year))))) %>% 
  filter(final_sales > 0, !(city %in% unique(proj.market.nation$city))) %>% 
  mutate(flag = 1) %>% 
  select(year, month, quarter, province, city, atc4, nfc, molecule, 
         product, packid, sales = final_sales, flag) %>% 
  bind_rows(proj.sample.m)

write.xlsx(proj.universe, "03_Outputs/04_Universe_Projection.xlsx")






