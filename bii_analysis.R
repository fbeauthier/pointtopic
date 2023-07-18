# load packages
library(haven)
library(ggpubr)
library(tidyverse)
library(qacEDA)
library(tmap)
library(osmdata)
library(sf)
library(gstat)

library(relaimpo)

setwd("~/Desktop/Point-Topic")

### wales LSOAs separately -----------------------------------------------
wa_lsoa <- readxl::read_xlsx("data/EngWales/wimd-2019-index-and-domain-scores-by-small-area.xlsx", sheet = "Data")
wa_lsoa <- rename(wa_lsoa,
                  LSOA_code = `WIMD 2019: LSOA index and domain scores`,
                  LSOA_name = `...2`,
                  local_authority = `...3`)
wa_lsoa <- wa_lsoa[-c(1,2,3), 1:3]

wa_lsoa$LSOA_code

### England -------------------------------------------------------------------
# 33,755 LSOAs total
# total population (from age) 56,489,949 (official fig. 56.5 mln)
# total # households (from hh_children) 23,435,963 (official fig. )

# read raw files
imd_inc_eng <- readxl::read_xlsx("data/EngWales/IoD2019_Domains_of_Deprivation.xlsx", sheet = "IoD2019 Domains")
age <- read_csv("data/EngWales/age.csv")
hh_disability <- read_csv("data/EngWales/hh_disability.csv")
hh_housing <- read_csv("data/EngWales/hh_housing.csv")
hh_children <- read_csv("data/EngWales/hh_children.csv")
hh_education <- read_csv("data/EngWales/hh_education.csv")
age_edu <- read_csv("data/EngWales/age_edu.csv")
bii <- read_csv("data/EngWales/BII2020.csv")

str(age)
## add age 65+ count
age$over65_count <- ifelse(age$`Age (D) (8 categories) Code` > 6, age$Observation, 0)

# create england LSOA meta table
england_wales <- age %>%
  group_by(`Lower layer Super Output Areas Code`, `Lower layer Super Output Areas`) %>%
  summarise(population = sum(Observation),
            over65_count = sum(over65_count))

# rename fields
england_wales <- england_wales %>%
  rename(LSOA_code = `Lower layer Super Output Areas Code`,
         LSOA_name = `Lower layer Super Output Areas`)

# proportion of LSOA population over 55
# count 65+ / total population - LSOA
england_wales$prop_over65 <- (england_wales$over65_count)/(england_wales$population)


## households children
str(hh_children)
# replace does not apply values -8 = 0
hh_children$`Dependent children in household and their age - indicator (3 categories) Code`[hh_children$`Dependent children in household and their age - indicator (3 categories) Code` == -8] <- 0

hh_children2 <- hh_children %>%
  group_by(`Lower layer Super Output Areas Code`, `Lower layer Super Output Areas`, `Dependent children in household and their age - indicator (3 categories) Code`) %>%
  summarise(count = sum(Observation))

# add variable for total # households in LSOAs
england_wales <- hh_children2 %>%
  group_by(`Lower layer Super Output Areas Code`) %>%
  summarise(households = sum(count)) %>%
  left_join(x = england_wales, by = c("LSOA_code" = "Lower layer Super Output Areas Code"))

# check population and households count
sum(england_wales$population)
sum(england_wales$households)

summary(england_wales)
histograms(england_wales)

# proportion of households with children for each LSOA
england_wales <- hh_children2 %>%
  filter(`Dependent children in household and their age - indicator (3 categories) Code` == 1) %>% # 1 = have children
  left_join(x = england_wales, by = c("LSOA_code" = "Lower layer Super Output Areas Code")) %>%
  rename(hh_children_count = count)

england_wales <- england_wales %>%
  dplyr::select(-`Lower layer Super Output Areas`, -`Dependent children in household and their age - indicator (3 categories) Code`)

# no. of households w children / total no. of households - LSOA
england_wales$prop_hh_children <- (england_wales$hh_children_count)/(england_wales$households)


## households disability
str(hh_disability)
# replace does not apply values -8 = 0
hh_disability$`Number of disabled people in household (3 categories) Code`[hh_disability$`Number of disabled people in household (3 categories) Code` == -8] <- 0

hh_disability2 <- hh_disability %>%
  group_by(`Lower layer Super Output Areas Code`, `Lower layer Super Output Areas`, `Number of disabled people in household (3 categories) Code`) %>%
  summarise(count = sum(Observation))

# check no. of households adds up with LSOA table
hh_disability2 %>%
  group_by(`Lower layer Super Output Areas Code`) %>%
  summarise(sum(count))
# do a standard deviation test??

# proportion of households with disabled ppl for each LSOA
england_wales <- hh_disability2 %>%
  filter(`Number of disabled people in household (3 categories) Code` == 1) %>% # 1 = has disabled
  left_join(x = england_wales, by = c("LSOA_code" = "Lower layer Super Output Areas Code")) %>%
  rename(hh_disability_count = count)

england_wales <- england_wales %>%
  dplyr::select(-`Lower layer Super Output Areas`, -`Number of disabled people in household (3 categories) Code`)

# no. of households w disabled / total no. of households - LSOA
england_wales$prop_hh_disability <- (england_wales$hh_disability_count)/(england_wales$households)


## households social housing
str(hh_housing)

# check no. of households adds up with LSOA table
hh_housing %>%
  group_by(`Lower layer Super Output Areas Code`) %>%
  summarise(sum(Observation))
# do a standard deviation test??

# add social housing binary variable
hh_housing$social_housing <- ifelse(hh_housing$`Tenure of household (5 categories) Code` == 2, 1, 0)

# count: in social housing vs not
hh_housing2 <- hh_housing %>%
  group_by(`Lower layer Super Output Areas Code`, `Lower layer Super Output Areas`, social_housing) %>%
  summarise(count = sum(Observation))

# proportion of households social housing for each LSOA
england_wales <- hh_housing2 %>%
  filter(social_housing == 1) %>%  # 1 = is in social housing
  left_join(x = england_wales, by = c("LSOA_code" = "Lower layer Super Output Areas Code")) %>%
  rename(hh_sochouse_count = count) %>%
  dplyr::select(-`Lower layer Super Output Areas`, -social_housing)

# no. of households social housing / total no. of households - LSOA
england_wales$prop_hh_sochousing <- (england_wales$hh_sochouse_count)/(england_wales$households)


## households education (deprivation)
str(hh_education)
# replace does not apply values -8 = 0
hh_education$`Household deprived in the education dimension (3 categories) Code`[hh_education$`Household deprived in the education dimension (3 categories) Code` == -8] <- 0

hh_education2 <- hh_education %>%
  group_by(`Lower layer Super Output Areas Code`, `Lower layer Super Output Areas`, `Household deprived in the education dimension (3 categories) Code`) %>%
  summarise(count = sum(Observation))

# check no. of households adds up with LSOA table
hh_education2 %>%
  group_by(`Lower layer Super Output Areas Code`) %>%
  summarise(sum(count))
# do a standard deviation test??

# proportion of households deprived education for each LSOA
england_wales <- hh_education2 %>%
  filter(`Household deprived in the education dimension (3 categories) Code` == 1) %>% # 1 = is deprived in education
  left_join(x = england_wales, by = c("LSOA_code" = "Lower layer Super Output Areas Code")) %>%
  dplyr::select(-`Lower layer Super Output Areas`, -`Household deprived in the education dimension (3 categories) Code`) %>%
  rename(hh_dep_edu_count = count)

# no. of households deprived education / total no. of households - LSOA
england_wales$prop_hh_dep_edu <- (england_wales$hh_dep_edu_count)/(england_wales$households)


## proportion of population in each LSOA 16y+ w/o higher education
str(age_edu)
age_edu <- rename(age_edu, age_code = `Age (D) (8 categories) Code`,
                  HLE_code = `Highest level of qualification (7 categories) Code`)

# create var: age 16+ and less than HS education (A-level)
age_edu$age16_noedu <- ifelse(((age_edu$age_code >1) & (age_edu$HLE_code %in% c(0, 1, 2))),
                              1,0)          # 1 = 16+ but no HS education
# population count
sum(age_edu$Observation)

age_edu2 <- age_edu %>%
  group_by(`Lower layer Super Output Areas Code`, age16_noedu) %>%
  summarise(count = sum(Observation))

england_wales <- age_edu2 %>%
  filter(age16_noedu == 1) %>% # 1 = age 16+ and has no HS education
  left_join(x = england_wales, by = c("LSOA_code" = "Lower layer Super Output Areas Code")) %>%
  dplyr::select(-age16_noedu) %>%
  rename(age16_noedu_count = count)

# no. of people age16_noedu / total population - LSOA
england_wales$prop_age16_noedu <- (england_wales$age16_noedu_count)/(england_wales$population)


## join BII onto england_wales
england_wales <- left_join(england_wales, bii, by=c("LSOA_code" = "lsoa"))

# check NA's
colSums(is.na(england_wales))

# export to CSV
write_csv(england_wales, "data/EngWales/070723_raw.csv")


## separate into England and Wales dataframes
wales <- subset(england_wales, LSOA_code > "W01000001")
england <- subset(england_wales, LSOA_code < "W01000001")
#tail(subset(england_wales, LSOA_code < "W01000001"))


## imd income england
imd_inc_eng <- imd_inc_eng %>%
  dplyr::select(`LSOA code (2011)`, `LSOA name (2011)`, `Local Authority District code (2019)`,
                `Local Authority District name (2019)`, `Income Rank (where 1 is most deprived)`,
                `Income Decile (where 1 is most deprived 10% of LSOAs)`) %>%
  rename(LSOA_code = `LSOA code (2011)`,
         LSOA_name = `LSOA name (2011)`,
         LAD_code = `Local Authority District code (2019)`,
         LAD_name = `Local Authority District name (2019)`,
         income_rank = `Income Rank (where 1 is most deprived)`, # 1 = most deprived 
         income_decile = `Income Decile (where 1 is most deprived 10% of LSOAs)`) # 1 = most deprived 10% of LSOAs


## Linear Model + rela_impo ##
# Missing IMD2019 income - no match on LSOA.

# NA's
colSums(is.na(england))

# select variables, omit NAs
england_forlm <- england %>%
  ungroup() %>%
  dplyr::select(prop_over65, prop_hh_children, prop_hh_disability, prop_hh_sochousing, prop_hh_dep_edu,
                prop_age16_noedu,`AvgOfBII total`) %>%
  drop_na()

england_forlm <- rename(england_forlm, bii = `AvgOfBII total`) # rename bii

# run linear model
lm1 <- lm(bii ~., 
          data = england_forlm)

summary(lm1)

lm1_relimp <- calc.relimp(lm1, type = "lmg")
lm1_relimp

### Wales ---------------------------------------------------------------------
# 1,917 LSOAs total
# total population (from age) (official fig. 3.1 mln)
# total # households (from hh_children) (official fig.)

wales

### N Ireland -----------------------------------------------------------------

### Scotland ------------------------------------------------------------------
