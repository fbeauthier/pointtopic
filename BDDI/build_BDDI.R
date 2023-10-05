### This R file contains the code to construct the updated Broadband Digital Deprivation Index (BDDI)
# new BDDI 2021/2022 for ALL UK
# this is a relative index, i.e., we do not calculate raw 'scores', only rank LSOAs relative to each other

# Geographic areas we use:
# England + Wales - 2021 Census LSOAs (35,672)
# Scotland - 2011 Data Zones (6,976)
# Northern Ireland - 2021 Census Super Data Zones (850)

# The variable data comes from: the UK Census for England and Wales (2021),
# National Records (statistics) of Scotland, Scottish Census (2011)
# Northern Ireland Statistics and Research Agency (NISRA), Irish Census (2021)
# as well as from the BII files (see other repo)
# we use the new BII output .csv files for each nation calculated in other the repository (all UK)


## Data sources ## ------------------------------------------------------------
# Note: the data files for this version are NOT included in the repo, sources + links are provided for future reference

# 1. England and Wales Census 2021 populations and households:
# "Number of usual residents" (TS001) and "Number of Households" (TS041) https://www.nomisweb.co.uk/census/2021/bulk 

# 2. England and Wales Census 2021:
# i. Residents: Age - (D) (8 categories), Education & Age - Age (4 categories - Option 1) and Highest level of qualification (7 categories), disability - Disability (3 categories)
# ii. Households: presence of children - Dependent children in household and their age - indicator (3 categories), tenure (social housing) - Tenure of household (5 categories)
# custom datasets for each item need to be downloaded one by one: https://www.ons.gov.uk/datasets/create
# choose Coverage: England + Wales and Area Type: LSOAs

# 3. English Indices of Deprivation 2019 (Income)
# IoD 2019 scores: Income Score (rate) https://www.gov.uk/government/statistics/english-indices-of-deprivation-2019
# "File 5: scores for the indices of deprivation"

# 4. Welsh Indices of Multiple Deprivation 2019 (Income)
# https://www.gov.wales/welsh-index-multiple-deprivation-full-index-update-ranks-2019
# Data: "WIMD 2019 Index and domain scores by small area"
# convert ODS file to .xlsx

### ---

# 5. Scotland Small Area Statistics on Households and Dwellings 2022:
# Household estimates by data zone: 2011 Data Zones
# "Small area household estimates data by 2011 Data Zone" (excel)
# https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/households/household-estimates/small-area-statistics-on-households-and-dwellings

# 6. Scotland Mid-2021 Small Area Population Estimates: Population, age
# Time series (select 2021)
# https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/population/population-estimates/small-area-population-estimates-2011-data-zone-based/time-series

# 7. Scottish Census 2011:
# Residents: education (+ age), disability
# i. Highest level of qualification by sex by age - Table LC5102SC
# https://www.scotlandscensus.gov.uk/search-the-census#/topics/list?topic=Education&categoryId=5
# select Advanced area types: SNS Data Zone 2011 - all (download table as *CSV String Value*.csv)

# ii. Long-term health problem or disability - Table QS303SC
# https://www.scotlandscensus.gov.uk/search-the-census#/topics/list?topic=Health&categoryId=3
# select Advanced area types: SNS Data Zone 2011 - all (download table as *CSV String Value*.csv)

# Households: presence of children, tenure (housing)
# i. Dependent children by family type - Table LC1114SC
# https://www.scotlandscensus.gov.uk/search-the-census/#/topics/list?topic=Population%20and%20Households&categoryId=1
# select Advanced area types: SNS Data Zone 2011 - all (download table as *CSV String Value*.csv)

# ii. Accommodation type by tenure - Households (Table LC4427SC)
# https://www.scotlandscensus.gov.uk/search-the-census/#/topics/list?topic=Population%20and%20Households&categoryId=1
# select Advanced area types: SNS Data Zone 2011 - all (download table as *CSV String Value*.csv)

# 8. Scottish Index of Multiple Deprivation 2020 (Income)
# https://www.gov.scot/publications/scottish-index-of-multiple-deprivation-2020v2-indicator-data/
# SIMD 2020v2 indicators (excel)

### ---

# 9. N Ireland Census 2021 populations and households:
# https://www.nisra.gov.uk/publications/census-2021-person-and-household-estimates-for-data-zones-in-northern-ireland
# "MS-A01 Usual resident population" and "MS-E01 Households"

# 10. N Ireland Census 2021:
# Ready-made tables:
# Residents:
# i. Age https://build.nisra.gov.uk/en/custom/data?d=PEOPLE&v=LGD14&v=AGE_BAND_5YR
# change "Geographic level" to "Census 2021 Super Data Zone" and "Variables" to "Age - 2 Categories D"
# ii. Education https://build.nisra.gov.uk/en/custom/data?d=PEOPLE&v=LGD14&v=HIGHEST_QUALIFICATION&v=AGE_BAND_AGG11&v=UR_SEX
# change "Geographic level" to "Census 2021 Super Data Zone" and remove "Sex" variable
# change Age variable to "Age - 3 Categories"

# Households:
# i. Children https://build.nisra.gov.uk/en/custom/data?d=HOUSEHOLD&v=LGD14&v=HH_DEPENDENT_CHILDREN_AGG11
# change "Geographic level" to "Census 2021 Super Data Zone" and "Variables" to "Dependent Children (Household) - 2 Categories"
# ii. Tenure (housing) https://build.nisra.gov.uk/en/custom/data?d=HOUSEHOLD&v=LGD14&v=HH_TENURE
# change "Geographic level" to "Census 2021 Super Data Zone" and "Variables" to "Tenure - 4 Categories"

# 11. N Ireland Census 2021 custom dataset:
# People (residents) - Disability: Health Condition (Intellectual or Learning Disability)
# Census 2021 Super Data Zone
# https://build.nisra.gov.uk/en/custom/dataset

# 12. Northern Ireland Multiple Deprivation Measure 2017 (NIMDM2017)
# https://www.nisra.gov.uk/publications/nimdm17-soa-level-results
# NIMDM 2017 - SOA level Results (Excel)


### Environment ---------------------------------------------------------------
# load packages
library(ggpubr)
library(tidyverse)
library(qacEDA)

# optional -- depends on your working directory
setwd("~/Desktop/Point-Topic/pointtopic/BDDI/")

# all subsequent file paths follow from the "data" directory

### 1. England + Wales --------------------------------------------------------
# There are 33,755 LSOAs in England and 1,917 in Wales
# https://www.ons.gov.uk/methodology/geography/ukgeographies/censusgeographies/census2021geographies 
# Census variables: total population and households, age, education, children, disability, housing

# read population data
population <- read_csv("data/Eng_Wales/census2021-ts001-population/census2021-ts001-lsoa.csv")

# read households data
households <- read_csv("data/Eng_Wales/census2021-ts041-households/census2021-ts041-lsoa.csv")

# create new tables for England + Wales LSOAs
# England
england <- population %>%
  dplyr::select(3,4) %>%  # select LSOA and total population columns by index
  filter(grepl('^E', `geography code`)) %>% # filter for England only using geo code starting with 'E'
  rename(population = 2) %>%  # rename population
  left_join(y = households[,3:4], by = "geography code") %>% # left-join households data, selecting LSOA and households columns by index
  rename(LSOA = 1, households = 3) %>%  # rename LSOA and households columns
  arrange(LSOA)  # order by LSOA

# Wales
wales <- population %>%
  dplyr::select(3,4) %>%  # select LSOA and total population columns by index
  filter(grepl('^W', `geography code`)) %>% # filter for Wales only using geo code starting with 'W'
  rename(population = 2) %>%  # rename population
  left_join(y = households[,3:4], by = "geography code") %>% # left-join households data, selecting LSOA and households columns by index
  rename(LSOA = 1, households = 3) %>%  # rename LSOA and households columns
  arrange(LSOA)  # order by LSOA

# check there aren't more households than population in each LSOA
# returns 1 if households > population (wrong)
unique(ifelse(england$households > england$population, 1, 0))
unique(ifelse(wales$households > wales$population, 1, 0))


# Pre-processing of demographic variables for each of England and Wales
# a. Age
age <- read_csv("data/Eng_Wales/age.csv")
str(age)
# create 65y+ binary column, returns 1 if over 65y, 0 otherwise
age$over65_flag <- ifelse(age$`Age (D) (8 categories) Code` %in% c(7,8),1,0)

# group by LSOA, "over65_pop"
age <- age %>%
  rename(LSOA = 1) %>% # rename LSOA column
  group_by(LSOA, over65_flag) %>%
  summarise(over65_pop = sum(Observation)) %>%
  ungroup()

# select over 65 population count (over65_flag == 1), left join to each of England and Wales
england <- age %>%
  filter(over65_flag == 1) %>%
  dplyr::select(1,3) %>% # drop flag column
  left_join(x=england, by = "LSOA")

wales <- age %>%
  filter(over65_flag == 1) %>%
  dplyr::select(1,3) %>% # drop flag column
  left_join(x=wales, by = "LSOA")

# create proportion of population over 65 per LSOA for each of England and Wales
england$prop_over65 <- (england$over65_pop)/(england$population)
wales$prop_over65 <- (wales$over65_pop)/(wales$population)

# b. Education
education <- read_csv("data/Eng_Wales/age_edu_v2.csv")
str(education)
# create over16 population variable per LSOA for each of England and Wales
# create 16y+ binary column, returns 1 if over 16y, 0 otherwise
education$over16_flag <- ifelse(education$`Age (4 categories) Code` == 1, 0, 1)

# England
england <- education %>%
  rename(LSOA = 1) %>%  # rename LSOA code column
  dplyr::select(1,8,7) %>%
  group_by(LSOA, over16_flag) %>%
  summarise(over16_pop = sum(Observation)) %>% # sum for over 16y population
  filter(over16_flag == 1) %>%  # select only over 16y population count (over16_flag == 1)
  dplyr::select(1,3) %>% # drop flag column
  left_join(x = england, by = "LSOA")

# Wales
wales <- education %>%
  rename(LSOA = 1) %>%  # rename LSOA code column
  dplyr::select(1,8,7) %>%
  group_by(LSOA, over16_flag) %>%
  summarise(over16_pop = sum(Observation)) %>% # sum for over 16y population
  filter(over16_flag == 1) %>%  # select only over 16y population count (over16_flag == 1)
  dplyr::select(1,3) %>% # drop flag column
  left_join(x = wales, by = "LSOA")

# create over16 population w/o qualifications per LSOA for each of England and Wales
# returns 1 if over 16y w/o qualifications, 0 otherwise
# omits "does not apply"
education$over16_noquals <- ifelse((education$over16_flag == 1 & education$`Highest level of qualification (7 categories) Code` %in% c(0,1,2)),
                                   1, 0)

# left join to England
england <- education %>%
  rename(LSOA = 1) %>%  # rename LSOA code column
  dplyr::select(1,9,7) %>%
  group_by(LSOA, over16_noquals) %>%
  summarise(over16_noqual_pop = sum(Observation)) %>% # sum for over 16y population w/o qualifications
  filter(over16_noquals == 1) %>%
  dplyr::select(1,3) %>% # drop flag column
  left_join(x = england, by = "LSOA")

# left join to Wales
wales <- education %>%
  rename(LSOA = 1) %>%  # rename LSOA code column
  dplyr::select(1,9,7) %>%
  group_by(LSOA, over16_noquals) %>%
  summarise(over16_noqual_pop = sum(Observation)) %>% # sum for over 16y population w/o qualifications
  filter(over16_noquals == 1) %>%
  dplyr::select(1,3) %>% # drop flag column
  left_join(x = wales, by = "LSOA")

# create proportion of over16 population w/o qualifications
# England
england$prop_over16s_noquals <- (england$over16_noqual_pop)/(england$over16_pop)
# Wales
wales$prop_over16s_noquals <- (wales$over16_noqual_pop)/(wales$over16_pop)


# c. Disability
disability <- read_csv("data/Eng_Wales/disability.csv")
str(disability)

# create "disabled" binary column, returns 1 if disabled, 0 otherwise
disability$disability_flag <- ifelse(disability$`Disability (3 categories) Code` == 1, 1, 0)

# group by LSOA, disability
disability <- disability %>%
  rename(LSOA = 1) %>% # rename LSOA column
  dplyr::select(1,6,5) %>%
  group_by(LSOA, disability_flag) %>%
  summarise(disabled_pop = sum(Observation)) %>%
  ungroup()

# select disabled population count (disability_flag == 1), left join to each of England and Wales
england <- disability %>%
  filter(disability_flag == 1) %>%
  dplyr::select(1,3) %>% # drop flag column
  left_join(x=england, by = "LSOA")

wales <- disability %>%
  filter(disability_flag == 1) %>%
  dplyr::select(1,3) %>% # drop flag column
  left_join(x=wales, by = "LSOA")

# create proportion of disabled population
# England
england$prop_disabled <- (england$disabled_pop)/(england$population)
# Wales
wales$prop_disabled <- (wales$disabled_pop)/(wales$population)


# d. Children
hh_children <- read_csv("data/Eng_Wales/hh_children.csv")
str(hh_children)

# create "children" binary column, returns 1 if household has children, 0 otherwise
hh_children$children_flag <- ifelse(hh_children$`Dependent children in household and their age - indicator (3 categories) Code` == 1,
                                    1, 0)

# group by LSOA, "children' flag
hh_children <- hh_children %>%
  rename(LSOA = 1) %>% # rename LSOA column
  dplyr::select(1,6,5) %>%
  group_by(LSOA, children_flag) %>%
  summarise(hh_w_children = sum(Observation)) %>%
  ungroup()

# select households with children count (children_flag == 1), left join to each of England and Wales
england <- hh_children %>%
  filter(children_flag == 1) %>%
  dplyr::select(1,3) %>% # drop flag column
  left_join(x=england, by = "LSOA")

wales <- hh_children %>%
  filter(children_flag == 1) %>%
  dplyr::select(1,3) %>% # drop flag column
  left_join(x=wales, by = "LSOA")

# create proportion of households with children
# England
england$prop_hh_children <- (england$hh_w_children)/(england$households)
# Wales
wales$prop_hh_children <- (wales$hh_w_children)/(wales$households)


# e. Housing
housing <- read_csv("data/Eng_Wales/hh_housing.csv")

# create housing binary column, returns 1 if household rents in social housing, 0 otherwise
housing$housing_flag <- ifelse(housing$`Tenure of household (5 categories) Code` == 2, 1, 0)

# group by LSOA, "housing' flag
housing <- housing %>%
  rename(LSOA = 1) %>% # rename LSOA column
  dplyr::select(1,6,5) %>%
  group_by(LSOA, housing_flag) %>%
  summarise(hh_social_rented = sum(Observation)) %>%
  ungroup()

# select social housing count (housing_flag == 1), left join to each of England and Wales
england <- housing %>%
  filter(housing_flag == 1) %>%
  dplyr::select(1,3) %>% # drop flag column
  left_join(x=england, by = "LSOA")

wales <- housing %>%
  filter(housing_flag == 1) %>%
  dplyr::select(1,3) %>% # drop flag column
  left_join(x=wales, by = "LSOA")

# create proportion of households in social housing
# England
england$prop_hh_sochousing <- (england$hh_social_rented)/(england$households)
# Wales
wales$prop_hh_sochousing <- (wales$hh_social_rented)/(wales$households)


# f. Income deprivation
# England
eng_inc_dep <- readxl::read_xlsx("data/Eng_Wales/File_5_-_IoD2019_Scores.xlsx", sheet = "IoD2019 Scores")

eng_inc_dep <- eng_inc_dep %>%
  dplyr::select(1, 6) %>% # select only relevant columns using indices
  rename(LSOA = 1, inc_dep_rate = 2) # rename columns

summary(eng_inc_dep)
# view distribution
eng_inc_dep %>% ggplot(aes(x = inc_dep_rate)) +
  geom_density() + geom_vline(xintercept = 0.1282)

# Wales
wales_inc_dep <- readxl::read_xlsx("data/Eng_Wales/wimd-2019-index-and-domain-scores-by-small-area.xlsx", sheet = "Data")

wales_inc_dep <- wales_inc_dep %>%
  rename(LSOA = 1, inc_dep_rate = 5) %>% # rename LSOA and income deprivation columns
  dplyr::select(LSOA, inc_dep_rate) # select only relevant columns

str(wales_inc_dep)
# income deprivation to numeric
wales_inc_dep$inc_dep_rate <- as.numeric(wales_inc_dep$inc_dep_rate)

wales_inc_dep <- wales_inc_dep[4:nrow(wales_inc_dep),] # select relevant rows: starting row 4
summary(wales_inc_dep)

# view distribution
wales_inc_dep %>% ggplot(aes(x = inc_dep_rate)) +
  geom_density() + geom_vline(xintercept = 21.7158)

summary(england)
summary(wales)
# finish pre-processing in section 5 #

### 2. Scotland ---------------------------------------------------------------
# Scotland 2011 Data Zones: 6,976
# Scotland Small Area Statistics on Households and Dwellings 2022: use year 2021
# 'occupied dwelling' is their proxy for a household
sc_households <- readxl::read_xlsx("data/Scotland/hh-est-by-2011-dz-small-area-14-22.xlsx", sheet = "2021")

sc_households <- sc_households %>%
  dplyr::select(1, 6) %>% # select only relevant columns by index
  rename(DZ_code = 1, households = 2) # rename DZ and households columns

sc_households <- sc_households[4:nrow(sc_households),] # select relevant rows: starting row 4

# check class type
str(sc_households)
# convert household count to numeric
sc_households$households <- as.numeric(sc_households$households)


# Scotland Small Area Population Estimates (SAPE) 2021: population and age
sc_population <- readxl::read_xlsx("data/Scotland/sape-2021.xlsx", sheet = "Persons")

sc_population <- sc_population %>%
  rename(DZ_code = 1, population = 5, age_65 = 71) %>% # rename DZ, population and age 65 columns
  dplyr::select(c(1, 5, 71:ncol(sc_population))) # select relevant columns

sc_population <- sc_population[4:nrow(sc_population),] # select relevant rows: starting row 4

# check class type
str(sc_population)
# convert necessary columns to numeric
sc_population[2:ncol(sc_population)] <- lapply(sc_population[2:ncol(sc_population)], as.numeric)

# new column: over 65 population count - using rowSums()
sc_population$over65_pop <- rowSums(sc_population[,3:28])

# keep only relevant columns
sc_population <- dplyr::select(sc_population,
                        DZ_code, population, over65_pop)

# new Scotland table: left-join with households table
scotland <- left_join(sc_households, sc_population, by = "DZ_code")
summary(scotland)

# check there aren't more households than population in each DZ
# returns 1 if households > population
unique(ifelse(scotland$households > scotland$population, 1, 0))

# view where households > population
filter(scotland, households > population) # just one DZ: S01010283

## Demographic variables
# a. Age: proportion population over 65
scotland$prop_over65 <- (scotland$over65_pop)/(scotland$population)

# b. Education
# [file]_v2 is transformed to CSV UTF-8 (comma-delimited .csv) for easier readability
sc_education <- read_csv("data/Scotland/table_2023-09-08_education_v2.csv")

sc_education <- sc_education %>%
  dplyr::select(3:7) %>% # select only relevant columns by index
  rename(DZ_code = 1, sex = 2, age = 3, highest_level_edu = 4, count = 5) # rename columns

sc_education <- sc_education[12:nrow(sc_education),] # select relevant rows: starting row 12
unique(sc_education$sex)
# filter out gender
sc_education <- filter(sc_education, sex == "All people aged 16 and over:")

# check class type
str(sc_education)

# get total over16 population
scotland <- sc_education %>%
  filter(age == "Total", highest_level_edu == "All people aged 16 and over") %>%
  rename(over16_pop = 5) %>%
  dplyr::select(1,5) %>%
  left_join(x = scotland, by = "DZ_code")

# # check if over16 population > population in each DZ (returns 1 if true, 0 else)
# unique(ifelse(scotland$over16_pop > scotland$population, 1, 0))
# # view where over16 population > population
# filter(scotland, over16_pop > population) %>% print(n=20) # 232 observations...

# create "over 16 and no quals" binary flag, returns 1 if over 16 w/o qualifications, 0 otherwise
# Level 1 = "O-grade" / "GCSEs" - count as no qualifications
sc_education$over16_noqual_flag <- ifelse((!sc_education$age == "Total" & sc_education$highest_level_edu %in% c("No qualifications", "Level 1")),
                                          1, 0)

# left join to Scotland table
scotland <- sc_education %>%
  dplyr::select(1,6,5) %>%
  group_by(DZ_code, over16_noqual_flag) %>%
  summarise(over16_noqual_pop = sum(count)) %>% # sum for over 16y population w/o qualifications
  filter(over16_noqual_flag == 1) %>%
  dplyr::select(1,3) %>% # drop flag column
  left_join(x = scotland, by = "DZ_code")

# create proportion over16s no qualifications
scotland$prop_over16s_noquals <- (scotland$over16_noqual_pop)/(scotland$over16_pop)


# c. Disability
# [file]_v2 is transformed to CSV UTF-8 (comma-delimited .csv) for easier readability
sc_disability <- read_csv("data/Scotland/table_2023-09-08_disability_v2.csv")
# select and rename relevant columns
sc_disability <- sc_disability %>% 
  dplyr::select(3,4,5) %>%
  rename(DZ_code = 1, disability = 2, count = 3)

sc_disability <- sc_disability[12:nrow(sc_disability),] # select relevant rows: starting row 12
# filter out disability:"All people" rows
sc_disability <- filter(sc_disability,
                        !disability == "All people")

# create binary "disability" flag, returns 1 if disabled, 0 otherwise
sc_disability$disability_flag <- ifelse(sc_disability$disability == "Day-to-day activities not limited", 0,1)

# left-join count to Scotland table
scotland <- sc_disability %>%
  dplyr::select(1,4,3) %>%
  group_by(DZ_code, disability_flag) %>%
  summarise(disabled_pop = sum(count)) %>% # sum for over 16y population w/o qualifications
  filter(disability_flag == 1) %>%
  dplyr::select(1,3) %>% # drop flag column
  left_join(x = scotland, by = "DZ_code")

# create proportion disabled
scotland$prop_disabled <- (scotland$disabled_pop)/(scotland$population)

# d. Children
# [file]_v2 is transformed to CSV UTF-8 (comma-delimited .csv) for easier readability
sc_children <- read_csv("data/Scotland/table_2023-09-08_hh_children_v2.csv")

# select and rename relevant columns
sc_children <- sc_children %>%
  dplyr::select(2:5) %>%
  rename(DZ_code = 1, children_in_family = 2, family_type = 3, count = 4)

sc_children <- sc_children[11:nrow(sc_children),] # select relevant rows: starting row 11

# filter out family type and number of children
sc_children <- filter(sc_children,
                      family_type == "All families in households" & children_in_family %in% c("All families in households",
                                                                                              "Families with no dependent children",
                                                                                              "Families with dependent children: Total"))
# left-join households with children to Scotland table
scotland <- sc_children %>%
  filter(children_in_family == "Families with dependent children: Total") %>%
  rename(hh_children = 4) %>%
  dplyr::select(DZ_code, hh_children) %>%
  left_join(x = scotland, by = "DZ_code")

# create proportion households with children
scotland$prop_hh_children <- (scotland$hh_children)/(scotland$households)


# e. Housing
# [file]_v2 is transformed to CSV UTF-8 (comma-delimited .csv) for easier readability
sc_housing <- read_csv("data/Scotland/table_2023-09-07_hh_tenure_v2.csv")

# select and rename relevant columns
sc_housing <- sc_housing %>%
  dplyr::select(3:6) %>%
  rename(DZ_code = 1, accom_type = 2, tenure = 3, count = 4)

sc_housing <- sc_housing[12:nrow(sc_housing),] # select relevant rows: starting row 12

# filter out accommodation type
sc_housing <- filter(sc_housing, accom_type == "All households")

# left-join social rented to Scotland table
scotland <- sc_housing %>%
  filter(tenure == "Rented or living rent free: Social rented") %>%
  rename(soc_housing = 4) %>%
  dplyr::select(DZ_code, soc_housing) %>%
  left_join(x = scotland, by = "DZ_code")

# create proportion households in social housing
scotland$prop_hh_soc_housing <- (scotland$soc_housing)/(scotland$households)


# f. Income deprivation
sc_inc_dep <- readxl::read_xlsx("data/Scotland/SIMD+2020v2+-+indicators.xlsx", sheet = "Data")
# 2021 v. 2020 population estimates
#left_join(scotland[,c("DZ_code", "population")], sc_inc_dep[,c("Data_Zone", "Total_population")], by = c("DZ_code" = "Data_Zone"))

# select relevant columns
sc_inc_dep <- dplyr::select(sc_inc_dep,
                            Data_Zone, Income_rate)

# left-join income deprivation rates to Scotland table
scotland <- left_join(scotland, sc_inc_dep, by = c("DZ_code" = "Data_Zone")) %>%
  rename(inc_dep_rate = Income_rate)

summary(scotland)
filter(scotland, prop_disabled == Inf)
# some NaNs and Inf's due to 0 households/population in some DZs
# keep all as NA
scotland[!complete.cases(scotland), 2:ncol(scotland)] <- NA

colSums(is.na(scotland))

### 3. N Ireland --------------------------------------------------------------
# Northern Ireland 2021 Census Super Data Zones: 850
# population and households data per SDZ
# population
ni_population <- readxl::read_xlsx("data/N_Ireland/census-2021-ms-a01.xlsx", sheet = "SDZ")
# rename and select relevant columns
ni_population <- ni_population %>%
  rename(SDZ = 2, population = 3) %>%
  dplyr::select(2,3)

ni_population <- ni_population[6:nrow(ni_population),] # select relevant rows: starting row 6

# households
ni_households <-  readxl::read_xlsx("data/N_Ireland/census-2021-ms-e01.xlsx", sheet = "SDZ")
# rename and select relevant columns
ni_households <- ni_households %>%
  rename(SDZ = 2, households = 3) %>% 
  dplyr::select(2,3)

ni_households <- ni_households[6:nrow(ni_households),] # select relevant rows: starting row 6

# left-join: create new N Ireland table
n_ireland <- left_join(ni_population, ni_households, by = "SDZ")
str(n_ireland)
# population and household counts as numeric
n_ireland <- n_ireland %>%
  mutate(population = as.numeric(population),
         households = as.numeric(households))

# check there aren't more households than population in each SDZ
# returns 1 if households > population (wrong)
unique(ifelse(n_ireland$households > n_ireland$population, 1, 0))

# a. Age
n_ire_age <- read_csv("data/N_Ireland/ni-census21-people-sdz21+age_band_agg2d-3a127c8f.csv")
str(n_ire_age)
# age 65y+ population
n_ireland <- n_ire_age %>%
  filter(`Age - 2 Categories D Code` == 2) %>%
  rename(SDZ = 1, over65_pop = 5) %>%
  dplyr::select(SDZ, over65_pop) %>%
  left_join(x = n_ireland, by = "SDZ")

# create proportion population over65
n_ireland$prop_over65 <- (n_ireland$over65_pop)/(n_ireland$population)


# b. Education
n_ire_education <- read_csv("data/N_Ireland/ni-census21-people-sdz21+highest_qualification+age_band_agg3-a6835f98.csv")
str(n_ire_education)

# create 16y+ binary flag, returns 1 if over 16y, 0 otherwise
n_ire_education$over16_flag <- ifelse(n_ire_education$`Age - 3 Categories Code` == 1, 0, 1)

# left-join over16 population to N Ireland table
n_ireland <- n_ire_education %>%
  rename(SDZ = 1) %>%  # rename SDZ code column
  dplyr::select(1,8,7) %>%
  group_by(SDZ, over16_flag) %>%
  summarise(over16_pop = sum(Count)) %>% # sum for over 16y population
  filter(over16_flag == 1) %>%
  dplyr::select(1,3) %>% # drop flag column
  left_join(x = n_ireland, by = "SDZ")

# create "over 16 and no quals" binary flag, returns 1 if over 16 w/o qualifications, 0 otherwise
# Level 1 and Level 2 - count as no qualifications
n_ire_education$over16_noqual_flag <- ifelse((n_ire_education$over16_flag == 1 & n_ire_education$`Qualifications (Highest Level) Code` %in% c(0,1,2)),
                                             1,0)

# left join to N Ireland
n_ireland <- n_ire_education %>%
  rename(SDZ = 1) %>%  # rename SDZ code column
  dplyr::select(1,9,7) %>%
  group_by(SDZ, over16_noqual_flag) %>%
  summarise(over16_noqual_pop = sum(Count)) %>% # sum for over 16y population w/o qualifications
  filter(over16_noqual_flag == 1) %>%
  dplyr::select(1,3) %>% # drop flag column
  left_join(x = n_ireland, by = "SDZ")

# create proportion over16 population w/o qualifications
n_ireland$prop_over16s_noquals <- (n_ireland$over16_noqual_pop)/(n_ireland$over16_pop)


# c. Disability
n_ire_disability <- read_csv("data/N_Ireland/ni-census21-people-sdz21+health_condition_intellectual_learning_disability-234ff795.csv")
str(n_ire_disability)
# left-join to N Ireland
n_ireland <- n_ire_disability %>%
  filter(`Health Condition (Intellectual or Learning Disability) Code` == 1) %>%
  rename(SDZ = 1, disability_pop = 5) %>%
  dplyr::select(SDZ, disability_pop) %>%
  left_join(x = n_ireland, by = "SDZ")

# create proportion population disabled
n_ireland$prop_disabled <- (n_ireland$disability_pop)/(n_ireland$population)


# d. Children
n_ire_children <- read_csv("data/N_Ireland/ni-census21-household-sdz21+hh_dependent_children_ind-caec672f.csv")
str(n_ire_children)
# left-join households with children to N Ireland
n_ireland <- n_ire_children %>%
  filter(`Dependent Children (Household) - 2 Categories Code` == 1) %>%
  rename(SDZ = 1, hh_children = 5) %>%
  dplyr::select(SDZ, hh_children) %>%
  left_join(x = n_ireland, by = "SDZ")

# create proportion households with children
n_ireland$prop_hh_children <- (n_ireland$hh_children)/(n_ireland$households)


# e. Housing
n_ire_housing <- read_csv("data/N_Ireland/ni-census21-household-sdz21+hh_tenure_agg4-56ffa305.csv")
str(n_ire_housing)

# create "social housing" binary flag, returns 1 if social rented, 0 otherwise
n_ire_housing$soc_housing_flag <- ifelse(n_ire_housing$`Tenure - 4 Categories Code` == 2, 1,0)

# left-join social housing households count to N Ireland table
n_ireland <- n_ire_housing %>%
  rename(SDZ = 1) %>%
  dplyr::select(1,6,5) %>%
  group_by(SDZ, soc_housing_flag) %>%
  summarise(soc_housing = sum(Count)) %>%
  filter(soc_housing_flag == 1) %>%
  dplyr::select(1,3) %>% # drop flag column
  left_join(x = n_ireland, by = "SDZ")

# create proportion households in social housing
n_ireland$prop_hh_soc_housing <- (n_ireland$soc_housing)/(n_ireland$households)


# f. Income deprivation
n_ire_inc_dep <- readxl::read_xls("data/N_Ireland/NIMDM17_SOAresults.xls", sheet = "Income")
#colnames(n_ire_inc_dep)

n_ire_inc_dep <- n_ire_inc_dep %>%
  dplyr::select(3,6) %>%
  rename(inc_dep_rate = 2)

summary(n_ireland)

### 5. Fix old boundaries data, create BDDI ranking ---------------------------
## keep only proportion columns in each nation
# England
colnames(england)
england <- dplyr::select(england,
                         LSOA, prop_over65, prop_over16s_noquals, prop_disabled, prop_hh_children, prop_hh_sochousing)

# Wales
colnames(wales)
wales <- dplyr::select(wales,
                       LSOA, prop_over65, prop_over16s_noquals, prop_disabled, prop_hh_children, prop_hh_sochousing)

# Scotland
colnames(scotland)
scotland <- dplyr::select(scotland,
                          DZ_code, prop_over65, prop_over16s_noquals, prop_disabled, prop_hh_children, prop_hh_soc_housing, inc_dep_rate)

# N Ireland
colnames(n_ireland)
# select proportions
n_ireland <- dplyr::select(n_ireland,
                           SDZ, prop_over65, prop_over16s_noquals, prop_disabled, prop_hh_children, prop_hh_soc_housing)

## Fix datasets with old boundaries: income deprivation and BII
# England
# translation table
translate_england <- read_csv("translation_fn_tables/translate_england.csv")
translate_england <- dplyr::select(translate_england, lsoa21, geo_code, prop)

translate_england %>% group_by(lsoa21) %>%
  summarise(sum(prop))

# left join proportions to table needing translation: Income
eng_inc_dep <- left_join(eng_inc_dep, translate_england, by = c("LSOA" = "geo_code"))
# multiply measure with proportions
eng_inc_dep$inc_dep_rate2 <- (eng_inc_dep$inc_dep_rate)*(eng_inc_dep$prop)

# group by new LSOA '21
eng_inc_dep_fixed <- eng_inc_dep %>%
  group_by(lsoa21) %>%
  summarise(inc_dep_rate = sum(inc_dep_rate2))

summary(eng_inc_dep_fixed)
filter(eng_inc_dep_fixed, inc_dep_rate > 1)
# make these values NA
eng_inc_dep_fixed$inc_dep_rate[eng_inc_dep_fixed$inc_dep_rate > 1] <- NA

# left-join fixed income_dep_rate to England table
england <- left_join(england, eng_inc_dep_fixed, by = c("LSOA" = "lsoa21"))

# replace NA value using knn
library(VIM)
sqrt(ncol(england)-1) # find sqrt of number of variables for approx. k
england <- kNN(england, k = 3, trace = FALSE, imp_var = FALSE)  # perform knn imputation

summary(england)

# create empty rank columns for each variable
# follows same procedure as creating BII separately
england <- mutate(england,
                  age_rank = NA,
                  edu_rank = NA,
                  disab_rank = NA,
                  children_rank = NA,
                  housing_rank = NA,
                  income_rank = NA)

# create LSOA ranking for age: lower prop_over65 is better
england$age_rank[order(england$prop_over65, decreasing = FALSE)] <- 1:nrow(england)
# create LSOA ranking for education: lower prop_over16s_noquals is better
england$edu_rank[order(england$prop_over16s_noquals, decreasing = FALSE)] <- 1:nrow(england)
# create LSOA ranking for disability: lower prop_disabled is better
england$disab_rank[order(england$prop_disabled, decreasing = FALSE)] <- 1:nrow(england)
# create LSOA ranking for children: higher prop_hh_children is better
england$children_rank[order(england$prop_hh_children, decreasing = TRUE)] <- 1:nrow(england)
# create LSOA ranking for housing: lower prop_hh_sochousing is better
england$housing_rank[order(england$prop_hh_sochousing, decreasing = FALSE)] <- 1:nrow(england)
# create LSOA ranking for income: lower inc_dep_rate is better
england$income_rank[order(england$inc_dep_rate, decreasing = FALSE)] <- 1:nrow(england)


# BII: left join proportions to table
# LSOAs haven't been updated, in future BII should have updated LSOAs and should only require a left-join
eng_bii <- read_csv("data/BII_outputs/newBII_output2023_England.csv")
# select columns for translation
eng_bii <- dplyr::select(eng_bii, 1:8)

eng_bii <- left_join(eng_bii, translate_england, by = c("LSOA" = "geo_code"))

# multiply each measure with proportions column
eng_bii_fixed <- eng_bii %>%
  mutate(across(c(2:8), function(x) x*prop))

# group by new LSOAs '21
eng_bii_fixed <- eng_bii_fixed %>%
  group_by(lsoa21) %>%
  summarise(FTTP_AVAILABILITY = sum(FTTP_AVAILABILITY),
            FTTC_AVAILABILITY = sum(FTTC_AVAILABILITY),
            DSL_AVAILABILITY = sum(DSL_AVAILABILITY),
            CABLE_AVAILABILITY = sum(CABLE_AVAILABILITY),
            OPERATOR_COUNT = sum(OPERATOR_COUNT),
            DOWN_MBPS = sum(DOWN_MBPS),
            UP_MBPS = sum(UP_MBPS))

# create empty rank columns for each variable
# follows same procedure as creating BII separately
eng_bii_fixed <- mutate(eng_bii_fixed,
                        fttp_rank = NA,
                        fttc_rank = NA,
                        dsl_rank = NA,
                        cable_rank = NA,
                        opscount_rank = NA,
                        down_rank = NA,
                        up_rank = NA)

# create LSOA ranking for broadband technology availability variables (FTTP, FTTC, DSL, Cable)
# 0-100 % availability -- higher (more availability) is better
eng_bii_fixed$fttp_rank[order(eng_bii_fixed$FTTP_AVAILABILITY, decreasing = TRUE)] <- 1:nrow(eng_bii_fixed)
eng_bii_fixed$fttc_rank[order(eng_bii_fixed$FTTC_AVAILABILITY, decreasing = TRUE)] <- 1:nrow(eng_bii_fixed)
eng_bii_fixed$dsl_rank[order(eng_bii_fixed$DSL_AVAILABILITY, decreasing = TRUE)] <- 1:nrow(eng_bii_fixed)
eng_bii_fixed$cable_rank[order(eng_bii_fixed$CABLE_AVAILABILITY, decreasing = TRUE)] <- 1:nrow(eng_bii_fixed)

# create LSOA ranking for OPERATOR_COUNT -- higher (more operators) is better
eng_bii_fixed$opscount_rank[order(eng_bii_fixed$OPERATOR_COUNT, decreasing = TRUE)] <- 1:nrow(eng_bii_fixed)

# create LSOA ranking for speed tests -- higher (faster) is better
# Down_speed
eng_bii_fixed$down_rank[order(eng_bii_fixed$DOWN_MBPS, decreasing = TRUE)] <- 1:nrow(eng_bii_fixed)
# Up_speed
eng_bii_fixed$up_rank[order(eng_bii_fixed$UP_MBPS, decreasing = TRUE)] <- 1:nrow(eng_bii_fixed)

# create column for total sum of ranks for each LSOA
eng_bii_fixed$rank_sum <- rowSums(eng_bii_fixed[9:15])

# use rank_sum to create final relative rank: 1 is best (lower overall rank is better)
eng_bii_fixed$final_BII_RANK[order(eng_bii_fixed$rank_sum)] <- 1:nrow(eng_bii_fixed)

summary(eng_bii_fixed)

## create final BDDI rank
england_BDDI <- england %>%
  dplyr::select(LSOA, age_rank, edu_rank, disab_rank, children_rank, housing_rank, income_rank) %>%
  left_join(y = eng_bii_fixed[c("lsoa21", "final_BII_RANK")], by = c("LSOA" = "lsoa21"))

# sum ranks
england_BDDI$rank_sum = rowSums(england_BDDI[2:8])

# use rank_sum to create final relative rank: 1 is best (lower overall rank is better)
england_BDDI$final_BDDI_RANK[order(england_BDDI$rank_sum)] <- 1:nrow(england_BDDI)

# write to .csv
write.csv(england_BDDI, "BDDI_outputs/BDDI_England.csv")

# Wales
# translation table
translate_wales <- read_csv("translation_fn_tables/translate_wales.csv")
translate_wales <- dplyr::select(translate_wales, lsoa21, geo_code, prop)

translate_wales %>% group_by(lsoa21) %>%
  summarise(sum(prop))

# left join proportions to table needing translation: Income deprivation
wales_inc_dep <- left_join(wales_inc_dep, translate_wales, by = c("LSOA" = "geo_code"))
# multiply measure with proportions
wales_inc_dep$inc_dep_rate2 <- (wales_inc_dep$inc_dep_rate)*(wales_inc_dep$prop)

# group by new LSOA '21
wales_inc_dep_fixed <- wales_inc_dep %>%
  group_by(lsoa21) %>%
  summarise(inc_dep_rate = sum(inc_dep_rate2))

summary(wales_inc_dep_fixed)
filter(wales_inc_dep_fixed, inc_dep_rate > 100)

# left-join fixed income_dep_rate to Wales table
wales <- left_join(wales, wales_inc_dep_fixed, by = c("LSOA" = "lsoa21"))

summary(wales)

# create empty rank columns for each variable
# follows same procedure as creating BII separately
wales <- mutate(wales,
                  age_rank = NA,
                  edu_rank = NA,
                  disab_rank = NA,
                  children_rank = NA,
                  housing_rank = NA,
                  income_rank = NA)

# create LSOA ranking for age: lower prop_over65 is better
wales$age_rank[order(wales$prop_over65, decreasing = FALSE)] <- 1:nrow(wales)
# create LSOA ranking for education: lower prop_over16s_noquals is better
wales$edu_rank[order(wales$prop_over16s_noquals, decreasing = FALSE)] <- 1:nrow(wales)
# create LSOA ranking for disability: lower prop_disabled is better
wales$disab_rank[order(wales$prop_disabled, decreasing = FALSE)] <- 1:nrow(wales)
# create LSOA ranking for children: higher prop_hh_children is better
wales$children_rank[order(wales$prop_hh_children, decreasing = TRUE)] <- 1:nrow(wales)
# create LSOA ranking for housing: lower prop_hh_sochousing is better
wales$housing_rank[order(wales$prop_hh_sochousing, decreasing = FALSE)] <- 1:nrow(wales)
# create LSOA ranking for income: lower inc_dep_rate is better
wales$income_rank[order(wales$inc_dep_rate, decreasing = FALSE)] <- 1:nrow(wales)


# BII: left join proportions to table
# LSOAs haven't been updated, in future BII should have updated LSOAs and should only require a left-join
wales_bii <- read_csv("data/BII_outputs/newBII_output2023_Wales.csv")
# select columns for translation
wales_bii <- dplyr::select(wales_bii, 1:8)

wales_bii <- left_join(wales_bii, translate_wales, by = c("LSOA" = "geo_code"))

# multiply each measure with proportions column
wales_bii_fixed <- wales_bii %>%
  mutate(across(c(2:8), function(x) x*prop))

# group by new LSOAs '21
wales_bii_fixed <- wales_bii_fixed %>%
  group_by(lsoa21) %>%
  summarise(FTTP_AVAILABILITY = sum(FTTP_AVAILABILITY),
            FTTC_AVAILABILITY = sum(FTTC_AVAILABILITY),
            DSL_AVAILABILITY = sum(DSL_AVAILABILITY),
            CABLE_AVAILABILITY = sum(CABLE_AVAILABILITY),
            OPERATOR_COUNT = sum(OPERATOR_COUNT),
            DOWN_MBPS = sum(DOWN_MBPS),
            UP_MBPS = sum(UP_MBPS))

# create empty rank columns for each variable
# follows same procedure as creating BII separately
wales_bii_fixed <- mutate(wales_bii_fixed,
                          fttp_rank = NA,
                          fttc_rank = NA,
                          dsl_rank = NA,
                          cable_rank = NA,
                          opscount_rank = NA,
                          down_rank = NA,
                          up_rank = NA)

# create LSOA ranking for broadband technology availability variables (FTTP, FTTC, DSL, Cable)
# 0-100 % availability -- higher (more availability) is better
wales_bii_fixed$fttp_rank[order(wales_bii_fixed$FTTP_AVAILABILITY, decreasing = TRUE)] <- 1:nrow(wales_bii_fixed)
wales_bii_fixed$fttc_rank[order(wales_bii_fixed$FTTC_AVAILABILITY, decreasing = TRUE)] <- 1:nrow(wales_bii_fixed)
wales_bii_fixed$dsl_rank[order(wales_bii_fixed$DSL_AVAILABILITY, decreasing = TRUE)] <- 1:nrow(wales_bii_fixed)
wales_bii_fixed$cable_rank[order(wales_bii_fixed$CABLE_AVAILABILITY, decreasing = TRUE)] <- 1:nrow(wales_bii_fixed)

# create LSOA ranking for OPERATOR_COUNT -- higher (more operators) is better
wales_bii_fixed$opscount_rank[order(wales_bii_fixed$OPERATOR_COUNT, decreasing = TRUE)] <- 1:nrow(wales_bii_fixed)

# create LSOA ranking for speed tests -- higher (faster) is better
# Down_speed
wales_bii_fixed$down_rank[order(wales_bii_fixed$DOWN_MBPS, decreasing = TRUE)] <- 1:nrow(wales_bii_fixed)
# Up_speed
wales_bii_fixed$up_rank[order(wales_bii_fixed$UP_MBPS, decreasing = TRUE)] <- 1:nrow(wales_bii_fixed)

# create column for total sum of ranks for each LSOA
wales_bii_fixed$rank_sum <- rowSums(wales_bii_fixed[9:15])

# use rank_sum to create final relative rank: 1 is best (lower overall rank is better)
wales_bii_fixed$final_BII_RANK[order(wales_bii_fixed$rank_sum)] <- 1:nrow(wales_bii_fixed)

summary(wales_bii_fixed)

## create final BDDI rank
colnames(wales)
wales_BDDI <- wales %>%
  dplyr::select(LSOA, age_rank, edu_rank, disab_rank, children_rank, housing_rank, income_rank) %>%
  left_join(y = wales_bii_fixed[c("lsoa21", "final_BII_RANK")], by = c("LSOA" = "lsoa21"))

# sum ranks
wales_BDDI$rank_sum = rowSums(wales_BDDI[2:8])

# use rank_sum to create final relative rank: 1 is best (lower overall rank is better)
wales_BDDI$final_BDDI_RANK[order(wales_BDDI$rank_sum)] <- 1:nrow(wales_BDDI)

# write to .csv
write.csv(wales_BDDI, "BDDI_outputs/BDDI_Wales.csv")


# N Ireland
# translation table
translate_n_ire <- read_csv("translation_fn_tables/translate_n_ireland.csv")
translate_n_ire <- dplyr::select(translate_n_ire, lsoa21, geo_code, prop)

translate_n_ire %>% group_by(lsoa21) %>%
  summarise(sum(prop))

# left join proportions to table needing translation: Income deprivation
n_ire_inc_dep <- left_join(n_ire_inc_dep, translate_n_ire, by = c("SOA2001" = "geo_code"))
# multiply measure with proportions
n_ire_inc_dep$inc_dep_rate2 <- (n_ire_inc_dep$inc_dep_rate)*(n_ire_inc_dep$prop)

# group by new LSOA '21
n_ire_inc_dep_fixed <- n_ire_inc_dep %>%
  group_by(lsoa21) %>%
  summarise(inc_dep_rate = sum(inc_dep_rate2))

summary(n_ire_inc_dep_fixed)
filter(n_ire_inc_dep_fixed, inc_dep_rate > 1)

# left-join fixed income_dep_rate to Wales table
n_ireland <- left_join(n_ireland, n_ire_inc_dep_fixed, by = c("SDZ" = "lsoa21"))

summary(n_ireland)

# create empty rank columns for each variable
# follows same procedure as creating BII separately
n_ireland <- mutate(n_ireland,
                    age_rank = NA,
                    edu_rank = NA,
                    disab_rank = NA,
                    children_rank = NA,
                    housing_rank = NA,
                    income_rank = NA)

# create LSOA ranking for age: lower prop_over65 is better
n_ireland$age_rank[order(n_ireland$prop_over65, decreasing = FALSE)] <- 1:nrow(n_ireland)
# create LSOA ranking for education: lower prop_over16s_noquals is better
n_ireland$edu_rank[order(n_ireland$prop_over16s_noquals, decreasing = FALSE)] <- 1:nrow(n_ireland)
# create LSOA ranking for disability: lower prop_disabled is better
n_ireland$disab_rank[order(n_ireland$prop_disabled, decreasing = FALSE)] <- 1:nrow(n_ireland)
# create LSOA ranking for children: higher prop_hh_children is better
n_ireland$children_rank[order(n_ireland$prop_hh_children, decreasing = TRUE)] <- 1:nrow(n_ireland)
# create LSOA ranking for housing: lower prop_hh_sochousing is better
n_ireland$housing_rank[order(n_ireland$prop_hh_soc_housing, decreasing = FALSE)] <- 1:nrow(n_ireland)
# create LSOA ranking for income: lower inc_dep_rate is better
n_ireland$income_rank[order(n_ireland$inc_dep_rate, decreasing = FALSE)] <- 1:nrow(n_ireland)


# BII: left join proportions to table
# LSOAs haven't been updated, in future BII should have updated LSOAs and should only require a left-join
n_ire_bii <- read_csv("data/BII_outputs/newBII_output2023_NIreland.csv")
# select columns for translation
n_ire_bii <- dplyr::select(n_ire_bii, 1:8)

n_ire_bii <- left_join(n_ire_bii, translate_n_ire, by = c("LSOA" = "geo_code"))

# multiply each measure with proportions column
n_ire_bii_fixed <- n_ire_bii %>%
  mutate(across(c(2:8), function(x) x*prop))

# group by new LSOAs '21
n_ire_bii_fixed <- n_ire_bii_fixed %>%
  group_by(lsoa21) %>%
  summarise(FTTP_AVAILABILITY = sum(FTTP_AVAILABILITY),
            FTTC_AVAILABILITY = sum(FTTC_AVAILABILITY),
            DSL_AVAILABILITY = sum(DSL_AVAILABILITY),
            CABLE_AVAILABILITY = sum(CABLE_AVAILABILITY),
            OPERATOR_COUNT = sum(OPERATOR_COUNT),
            DOWN_MBPS = sum(DOWN_MBPS),
            UP_MBPS = sum(UP_MBPS))

# create empty rank columns for each variable
# follows same procedure as creating BII separately
n_ire_bii_fixed <- mutate(n_ire_bii_fixed,
                          fttp_rank = NA,
                          fttc_rank = NA,
                          dsl_rank = NA,
                          cable_rank = NA,
                          opscount_rank = NA,
                          down_rank = NA,
                          up_rank = NA)

# create LSOA ranking for broadband technology availability variables (FTTP, FTTC, DSL, Cable)
# 0-100 % availability -- higher (more availability) is better
n_ire_bii_fixed$fttp_rank[order(n_ire_bii_fixed$FTTP_AVAILABILITY, decreasing = TRUE)] <- 1:nrow(n_ire_bii_fixed)
n_ire_bii_fixed$fttc_rank[order(n_ire_bii_fixed$FTTC_AVAILABILITY, decreasing = TRUE)] <- 1:nrow(n_ire_bii_fixed)
n_ire_bii_fixed$dsl_rank[order(n_ire_bii_fixed$DSL_AVAILABILITY, decreasing = TRUE)] <- 1:nrow(n_ire_bii_fixed)
n_ire_bii_fixed$cable_rank[order(n_ire_bii_fixed$CABLE_AVAILABILITY, decreasing = TRUE)] <- 1:nrow(n_ire_bii_fixed)

# create LSOA ranking for OPERATOR_COUNT -- higher (more operators) is better
n_ire_bii_fixed$opscount_rank[order(n_ire_bii_fixed$OPERATOR_COUNT, decreasing = TRUE)] <- 1:nrow(n_ire_bii_fixed)

# create LSOA ranking for speed tests -- higher (faster) is better
# Down_speed
n_ire_bii_fixed$down_rank[order(n_ire_bii_fixed$DOWN_MBPS, decreasing = TRUE)] <- 1:nrow(n_ire_bii_fixed)
# Up_speed
n_ire_bii_fixed$up_rank[order(n_ire_bii_fixed$UP_MBPS, decreasing = TRUE)] <- 1:nrow(n_ire_bii_fixed)

# create column for total sum of ranks for each LSOA
n_ire_bii_fixed$rank_sum <- rowSums(n_ire_bii_fixed[9:15])

# use rank_sum to create final relative rank: 1 is best (lower overall rank is better)
n_ire_bii_fixed$final_BII_RANK[order(n_ire_bii_fixed$rank_sum)] <- 1:nrow(n_ire_bii_fixed)

summary(n_ire_bii_fixed)

## create final BDDI rank
colnames(n_ireland)
n_ireland_BDDI <- n_ireland %>%
  dplyr::select(SDZ, age_rank, edu_rank, disab_rank, children_rank, housing_rank, income_rank) %>%
  left_join(y = n_ire_bii_fixed[c("lsoa21", "final_BII_RANK")], by = c("SDZ" = "lsoa21"))

# sum ranks
n_ireland_BDDI$rank_sum = rowSums(n_ireland_BDDI[2:8])

# use rank_sum to create final relative rank: 1 is best (lower overall rank is better)
n_ireland_BDDI$final_BDDI_RANK[order(n_ireland_BDDI$rank_sum)] <- 1:nrow(n_ireland_BDDI)

# write to .csv
write.csv(n_ireland_BDDI, "BDDI_outputs/BDDI_NIreland.csv")


# Scotland
# create empty rank columns for each variable
# follows same procedure as creating BII separately
scotland <- mutate(scotland,
                   age_rank = NA,
                    edu_rank = NA,
                    disab_rank = NA,
                    children_rank = NA,
                    housing_rank = NA,
                    income_rank = NA)

# create LSOA ranking for age: lower prop_over65 is better
scotland$age_rank[order(scotland$prop_over65, decreasing = FALSE)] <- 1:nrow(scotland)
# create LSOA ranking for education: lower prop_over16s_noquals is better
scotland$edu_rank[order(scotland$prop_over16s_noquals, decreasing = FALSE)] <- 1:nrow(scotland)
# create LSOA ranking for disability: lower prop_disabled is better
scotland$disab_rank[order(scotland$prop_disabled, decreasing = FALSE)] <- 1:nrow(scotland)
# create LSOA ranking for children: higher prop_hh_children is better
scotland$children_rank[order(scotland$prop_hh_children, decreasing = TRUE)] <- 1:nrow(scotland)
# create LSOA ranking for housing: lower prop_hh_sochousing is better
scotland$housing_rank[order(scotland$prop_hh_soc_housing, decreasing = FALSE)] <- 1:nrow(scotland)
# create LSOA ranking for income: lower inc_dep_rate is better
scotland$income_rank[order(scotland$inc_dep_rate, decreasing = FALSE)] <- 1:nrow(scotland)

# keep all NAs as NA
scotland[!complete.cases(scotland), 2:ncol(scotland)] <- NA
summary(scotland)

# read BII
scotland_bii <- read_csv("data/BII_outputs/newBII_output2023_Scotland.csv")
#scotland_bii[scotland_bii$LSOA %in% c("S01010206", "S01010226", "S01010227"), "final_RANK"]

# left-join ranks and BII
scotland_BDDI <- scotland %>%
  dplyr::select(DZ_code, age_rank, edu_rank, disab_rank, children_rank, housing_rank, income_rank) %>%
  left_join(y = scotland_bii[c("LSOA", "final_RANK")], by = c("DZ_code" = "LSOA"))

scotland_BDDI <- rename(scotland_BDDI, final_BII_RANK = final_RANK) # rename BII
# keep all NAs as NA
scotland_BDDI[!complete.cases(scotland_BDDI), 2:ncol(scotland_BDDI)] <- NA

# sum ranks
scotland_BDDI$rank_sum = rowSums(scotland_BDDI[2:8])

# use rank_sum to create final relative rank: 1 is best (lower overall rank is better)
scotland_BDDI$final_BDDI_RANK[order(scotland_BDDI$rank_sum)] <- 1:nrow(scotland_BDDI)
# keep all NAs as NA
scotland_BDDI[!complete.cases(scotland_BDDI), 2:ncol(scotland_BDDI)] <- NA


# write to .csv
write.csv(scotland_BDDI, "BDDI_outputs/BDDI_Scotland.csv")
