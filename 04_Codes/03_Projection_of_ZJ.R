# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  Sanofi CMAX
# Purpose:      Projection of Zhejiang
# programmer:   Zhe Liu
# Date:         2020-09-22
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##---- Projection of Zhejiang ----
# district
district.mapping <- universe.pchc %>% 
  filter(province == '浙江') %>% 
  left_join(hospital.universe[, c("pchc", "est")], by = "pchc")

district.mapping1 <- district.mapping %>% 
  filter(pchc %in% proj.market.sample$pchc)

district.mapping2 <- district.mapping %>% 
  filter(!(pchc %in% proj.market.sample$pchc))

proj.zj <- district.mapping2 %>% 
  left_join(district.mapping1[, c('province', 'pchc', 'est')], by = c('province')) %>% 
  mutate(diff = abs(est.x - est.y)) %>% 
  group_by(province, city, district, pchc.x) %>% 
  filter(diff == min(diff, na.rm = TRUE)) %>% 
  ungroup() %>% 
  select(province, city, district, pchc = pchc.x, est = est.x, 
         pchc1 = pchc.y, est1 = est.y) %>% 
  bind_rows(district.mapping1) %>% 
  mutate(pchc1 = if_else(is.na(pchc1), pchc, pchc1), 
         est1 = if_else(is.na(est1), est, est1)) %>% 
  left_join(proj.market.sample, by = c('pchc1' = 'pchc')) %>% 
  mutate(final_sales = if_else(pchc %in% proj.market.sample$pchc, sales, sales / est1 * est)) %>% 
  group_by(year, month, quarter, province = province.x, city = city.x, 
           district = district.x, atc4, nfc, molecule, product, packid) %>% 
  summarise(sales = sum(final_sales, na.rm = TRUE)) %>% 
  ungroup()

proj.sample.m <- proj.sample %>% 
  filter(province != '浙江') %>% 
  bind_rows(proj.zj)

write.xlsx(proj.sample.m, "03_Outputs/03_Projection_of_Sample_Cities.xlsx")










