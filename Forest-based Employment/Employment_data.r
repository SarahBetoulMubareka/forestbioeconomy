# Employment dataset to test imputation algorithms
## v0
## 2022.02.18 @ Nicolas Robert

library(eurostat)
library(tidyr)
library(dplyr)

# labour force survey
lfsa_egan22d <- get_eurostat("lfsa_egan22d",  time_format = "num", stringsAsFactors=FALSE)
lfsa_egan22d <- label_eurostat(lfsa_egan22d, code = names(lfsa_egan22d)[c(-6,-7)])

# national accounts: employment
nama_10_a64_e <- get_eurostat("nama_10_a64_e",  time_format = "num", stringsAsFactors=FALSE)
nama_10_a64_e <- label_eurostat(nama_10_a64_e, code = names(nama_10_a64_e)[c(-5, -6)])

# national accounts: economic indicators
nama_10_a64 <- get_eurostat("nama_10_a64",  time_format = "num", stringsAsFactors=FALSE)
nama_10_a64 <- label_eurostat(nama_10_a64, code = names(nama_10_a64)[c(-5, -6)])

# business statistics
sbs_na_ind_r2 <- get_eurostat("sbs_na_ind_r2",  time_format = "num", stringsAsFactors=FALSE)
sbs_na_ind_r2 <- label_eurostat(sbs_na_ind_r2, code = names(sbs_na_ind_r2)[c(-4,-5)], fix_duplicated = TRUE)


sectors <- c("A02", "C16", "C17", "C18", "C31", "D35", "F41", "F42", "F43", "J58")
EU27 <- c("AT", "BE", "BG", "CY", "CZ", "DE", "DK", "EE", "EL", "ES", "EU27_2020", "FI", "FR", "HR", "HU", "IE", "IT", "LT", "LU", "LV", "MT", "NL", "PL", "PT", "RO", "SE", "SI", "SK")

# Building the full dataset and merging the sectors that cannot be reported separately (F41, F42 and F43 -> F)


employment <- lfsa_egan22d %>% filter(unit_code=="THS", 
                        lfsa_egan22d$age_code %in% c("Y15-64", "Y25-64", "Y25-49", "Y50-64", "Y_GE15", "Y_GE25", "Y_GE50", "Y_GE65"),
                        lfsa_egan22d$nace_r2_code %in% sectors,
                        lfsa_egan22d$geo_code %in% EU27) %>%
  select(-sex, -age) %>% 
  pivot_wider(names_from = sex_code, values_from = values) %>% 
  mutate(F=ifelse(is.na(F), T-M, F)) %>%
  pivot_longer(cols=c("M", "F", "T"), names_to = "sex_code", values_to = "values" ) %>% 
  pivot_wider(names_from = age_code, values_from = values) %>% 
  mutate(`Y50-64`=ifelse(is.na(`Y50-64`), `Y25-64` - `Y25-49`, `Y50-64`), `Y25-49`=ifelse(is.na(`Y25-49`), `Y_GE25` - `Y_GE50`, `Y25-49`), `Y15-24`=ifelse(!is.na(`Y_GE15`- `Y_GE25`), `Y_GE15`- `Y_GE25`, `Y15-64` - `Y25-64`)) %>% 
  select(unit_code, nace_r2_code, geo_code, unit, time, sex_code, `Y15-24`, `Y25-49`, `Y50-64`, Y_GE65, Y_GE15) %>% 
  mutate(Y_GE65=ifelse(is.na(Y_GE65), Y_GE15-(`Y15-24`+`Y25-49`+`Y50-64`), Y_GE65)) %>% 
  pivot_longer(cols=c("Y15-24", "Y25-49", "Y50-64", "Y_GE65", "Y_GE15"), names_to = "age_code", values_to = "values" ) %>% 
  mutate(values=ifelse(values<0,0, values)) %>% # I checked that negative values come from rounding, not from erroneous values. 
  mutate(nace_r2_code=ifelse(nace_r2_code %in% c("F41", "F42",  "F43"), "F", nace_r2_code) ) %>%  # merging the construction sector
  group_by(unit_code, geo_code, nace_r2_code, sex_code, age_code, time) %>%
  summarise(values=sum(values)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = c(sex_code, age_code), values_from = values) %>% 
  left_join(nama_10_a64_e %>% 
              filter(na_item_code %in% c("EMP_DC"), unit_code %in% c("THS_PER", "THS_HW")) %>% 
              select(nace_r2_code, geo_code, time, unit_code, values) %>% 
              pivot_wider(names_from = unit_code, values_from = values)) %>% 
  left_join(sbs_na_ind_r2 %>% select(nace_r2_code, indic_sb_code, geo_code, time, values) %>% 
              filter(time>2007, indic_sb_code %in% c("V16110", "V16150", "V13320")) %>%
              pivot_wider(names_from = indic_sb_code, values_from = values)) %>% 
  left_join(nama_10_a64 %>% 
              filter(na_item_code %in% c("D1", "D11", "B1G"), unit_code %in% c("CP_MEUR")) %>% # -- could also explore "CLV10_MEUR" with chained prices, but only with the value added.
              select(nace_r2_code, geo_code, time, na_item_code, unit_code, values) %>% 
              pivot_wider(names_from = c(na_item_code, unit_code), values_from = values))


# remove the 0 hours worked then a number of workers is reported (accounting data)
employment$THS_HW <- ifelse(employment$THS_HW==0 & employment$THS_PER>0, NA,  employment$THS_HW) 

# Check some ratios
employment %>% mutate(ratio_NA_LFS = T_Y_GE15/THS_PER, ratio_SBS_LFS = V16110/THS_PER, employ_cost_NA=D11_CP_MEUR/THS_PER, employ_cost_SBS = V13320/V16110*1000) %>% View

# --- Details of the table ---
# unit_code     unit used in LFS data
# geo_code      country code
# nace_r2_code  code of activity (NACE rev2)
# time          year
# F_Y_GE15      number of women employed
# F_Y_GE65      number of women over 65 employed
# F_Y15-24      number of 15 to 24 year-old women employed
# F_Y25-49      number of 25 to 49 year-old women employed
# F_Y50-64      number of 50 to 64 year-old women employed
# M_Y_GE15      number of men employed
# M_Y_GE65      number of men over 65 employed
# M_Y15-24      number of 15 to 24 year-old men employed
# M_Y25-49      number of 25 to 49 year-old men employed
# M_Y50-64      number of 50 to 64 year-old men employed
# T_Y_GE15      number of persons employed
# T_Y_GE65      number of persons over 65 employed
# T_Y15-24      number of 15 to 24 year-old persons employed
# T_Y25-49      number of 25 to 49 year-old persons employed
# T_Y50-64      number of 50 to 64 year-old persons employed
# THS_HW        Thousand hours worked (from the national accounts)
# THS_PER       Thousand persons employed (from the national accounts)
# V16110        Persons employed - number (from the structural business statistics)
# V13320        Wages and Salaries - million euro (from the structural business statistics)
# V16150        Hours worked by employees - number (from the structural business statistics)
# B1G_CP_MEUR   Gross value added in million euros (from the national accounts)
# D1_CP_MEUR    Compensation of employees in million euros (from the national accounts)
# D11_CP_MEUR   Wages and salaries in million euros (from the national accounts)


# NB: SBS only reports manufacturing sectors (no data for forestry and construction)
# NB2: the sectors C31 and C32 are agregated in the national accounts
