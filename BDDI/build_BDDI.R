### This R file contains the code to construct the updated Broadband Digital Deprivation Index (BDDI)
# new BDDI 2023 for ALL UK
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
# i. Residents: age - (D) (8 categories), education + age - Age (D) (8 categories) and Highest level of qualification (7 categories), disability - Disability (3 categories)
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

summary(england)
summary(wales)

# create proportion of population over 65 per LSOA for each of England and Wales
england$prop_over65 <- (england$over65_pop)/(england$population)
wales$prop_over65 <- (wales$over65_pop)/(wales$population)

# b. Education
education <- read_csv("data/Eng_Wales/age_edu.csv")

# create over16 population variable per LSOA for each of England and Wales
# create 16y+ binary column, returns 1 if over 16y, 0 otherwise
education$over16_flag <- ifelse(education$`Age (D) (8 categories) Code` == 1, 0, 1)

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


# e. Housing
housing <- read_csv("data/Eng_Wales/hh_housing.csv")


# f. Income deprivation
# England
eng_inc <- readxl::read_xlsx("data/Eng_Wales/File_5_-_IoD2019_Scores.xlsx", sheet = "IoD2019 Scores")

eng_inc <- eng_inc %>%
  select(1, 6) %>% # select only relevant columns using indices
  rename(LSOA = 1, inc_dep_rate = 2) # rename columns

summary(eng_inc)
# view distribution
eng_inc %>% ggplot(aes(x = inc_dep_rate)) +
  geom_density()

# Wales
wales_inc <- readxl::read_xlsx("data/Eng_Wales/wimd-2019-index-and-domain-scores-by-small-area.xlsx", sheet = "Data")

wales_inc <- wales_inc %>%
  rename(LSOA = 1, inc_dep_rate = 5) %>% # rename LSOA and income deprivation columns
  select(LSOA, inc_dep_rate) # select only relevant columns

wales_inc <- wales_inc[4:nrow(wales_inc),] # select relevant rows 
summary(wales_inc)

# income deprivation to numeric
wales_inc$inc_dep_rate <- as.numeric(wales_inc$inc_dep_rate)

# view distribution
wales_inc %>% ggplot(aes(x = inc_dep_rate)) +
  geom_density()

### 2. Scotland ---------------------------------------------------------------
# Scotland 2011 Data Zones: 6,976
# Scotland Small Area Statistics on Households and Dwellings 2022: use year 2021
# 'occupied dwelling' is their proxy for a household
sc_households <- readxl::read_xlsx("data/Scotland/hh-est-by-2011-dz-small-area-14-22.xlsx", sheet = "2021")

sc_households <- sc_households %>%
  select(1, 6) %>% # select only relevant columns by index
  rename(DZ_code = 1, households = 2) # rename DZ and households columns

sc_households <- sc_households[4:nrow(sc_households),] # select relevant rows

# check class type
str(sc_households)
# convert household count to numeric
sc_households$households <- as.numeric(sc_households$households)

# Scotland small area population estimates: population and age
sc_population <- readxl::read_xlsx("data/Scotland/sape-2021.xlsx", sheet = "Persons")

sc_population <- sc_population %>%
  rename(DZ_code = 1, population = 5, age_65 = 71) %>% # rename DZ, population and age 65 columns
  select(c(1, 5, 71:ncol(sc_population))) # select relevant columns

sc_population <- sc_population[4:nrow(sc_population),] # select relevant rows

# check class type
str(sc_population)
# convert necessary columns to numeric
sc_population[2:ncol(sc_population)] <- lapply(sc_population[2:ncol(sc_population)], as.numeric)

# new column: over 65 population count - using rowSums()
sc_population$over65_pop <- rowSums(sc_population[,3:ncol(sc_population)])

# keep only relevant columns - by index
sc_population <- select(sc_population, 1, 2, 29)

# new table: left-join with households table
scotland <- left_join(sc_population, sc_households, by = "DZ_code")
summary(scotland)

# check there aren't more households than population in each DZ
# returns 1 if households > population (wrong)
unique(ifelse(scotland$households > scotland$population, 1, 0))

# view DZ's where households > population
filter(scotland, households > population) # just one DZ: S01010283

# Demographic variables
# Education

### 3. N Ireland --------------------------------------------------------------


### 4. geography boundaries function ------------------------------------------
# England, Wales and Northern Ireland datasets require geography translations from old to new boundaries

# required packages
library(rgdal)
library(raster)
library(sf)
library(st)
library(shapefiles)

# translate_geo function to translate old boundaries to new boundaries using proportion of postcodes
# the function requires 3 inputs: old boundaries, new boundaries and postcodes, all as .shp
translate_geo <- function(old, new, postcodes){
  # make sure CRS for all three datasets are equal
  # coerce old and new boundaries to "POLYGON"
  sf_use_s2(FALSE)
  old <- st_cast(st_transform(old, crs = 4326), "POLYGON")
  new <- st_cast(st_transform(new, crs = 4326), "POLYGON")
  postcodes <- st_transform(postcodes, crs = 4326)
  
  # get number of postcodes in new boundaries
  # st_join postcodes in new boundaries
  new_w_postcodes <- st_join(new[new_code_col], postcodes[pcd_col]) # "new_code_col" and "pcd_col" need to be defined in global environment
  #print(new_w_postcodes)
  
  # sum of postcodes in each new boundary
  new_postcode_count <- new_w_postcodes %>%
    as.data.frame() %>%  # convert to regular st dataframe (easier for simple calculations)
    dplyr::select(1,2) %>%  # select only code and postcode cols by index
    group_by_at(1) %>%  # group by area code
    summarise(sum_new_postcodes = n()) %>%
    ungroup()
  #print(new_postcode_count)
  
  # intersection of old boundaries and new boundaries using st_intersection() function
  # returns the intersection polygon for each old boundary part within a new boundary
  new_w_old <- st_intersection(new[new_code_col], old[old_code_col])
  #print(new_w_old)
  
  # save shapefile if wanted
  #st_write(new_w_old, "[filepath/filename].shp")
  
  # left-join postcodes found in each intersection area
  print(paste("CRS matches:", st_crs(new_w_old) == st_crs(postcodes))) # check Coordinate Reference System matches
  new_w_old_postcodes <- st_join(new_w_old, postcodes[pcd_col])
  #print(new_w_old_postcodes)
  
  # count number of postcodes in each intersection area (old boundaries parts within new boundary)
  new_w_old_postcodes <- new_w_old_postcodes %>%
    as.data.frame() %>%  # convert to regular st dataframe (easier for simple calculations)
    dplyr::select(new_code_col, old_code_col, pcd_col) %>%  # select new boundary, old boundary and postcode columns
    group_by(across(c(1, 2))) %>%   # group by new boundary code, old boundary code indexes
    summarise(old_in_new_postcodes = n()) %>%
    ungroup()
  #print(new_w_old_postcodes)
  
  # create new table of proportion of postcodes in old boundaries parts as total postcodes new boundary
  proportions <- new_postcode_count %>%
    left_join(y = new_w_old_postcodes, by = new_code_col) %>%
    mutate(prop = old_in_new_postcodes/sum_new_postcodes,
           prop = round(prop, digits = 3))
  #print(proportions)
  
  return(proportions)
}

## read new boundaries shapefiles
# obtained for England and Wales from: https://geoportal.statistics.gov.uk/datasets/766da1380a3544c5a7ca9131dfd4acb6/explore
# (shapefile)
# obtained for Northern Ireland from: https://www.nisra.gov.uk/publications/geography-super-data-zone-boundaries-gis-format
# (ESRI shapefile)

# England and Wales
new <- st_read("data/geos/Eng_Wales_LSOA_2021_Boundaries/LSOA_2021_EW_BGC.shp")
# keep only relevant columns: boundary code
new <- dplyr::select(new, 1)

# create new "country" column based on LSOA codes
new$country <- ifelse(grepl('^E', new$LSOA21CD), "England", "Wales")

# new England boundaries
new_E <- filter(new, country == 'England')
# new Wales boundaries
new_W <- filter(new, country == 'Wales')

# N Ireland
new_NI <- st_read("data/geos/NI-sdz2021-esri-shapefile/SDZ2021.shp")

# read old boundaries shapefiles (for all UK)
# https://statistics.ukdataservice.ac.uk/dataset/2011-census-geography-boundaries-lower-layer-super-output-areas-and-data-zones
# Features in Shapefile format
all_old <- st_read("data/geos/infuse_lsoa_lyr_2011/infuse_lsoa_lyr_2011.shp")
# keep only relevant columns: boundary code
all_old <- dplyr::select(all_old, 1)

# create new "country" column based on LSOA codes
all_old$country <- ifelse(grepl('^E', all_old$geo_code), "England", # starts with E
                          ifelse(grepl('^W', all_old$geo_code), "Wales", # starts with W
                                 ifelse(grepl('^S', all_old$geo_code), "Scotland", "Northern Ireland"))) # starts with S, else N-Ireland

# old England boundaries
old_E <- filter(all_old, country == 'England')
# old Wales boundaries
old_W <- filter(all_old, country == 'Wales')
# old N Ireland boundaries
old_NI <- filter(all_old, country == 'Northern Ireland')


# read postcodes .csv for all UK from P-T UPC time-series table
# all new boundaries for England, Wales and N Ireland in 2021: selected end year 2021 from UPC using SQL query (see .rtf file):
postcodes <- read_csv("data/PT_UPC_postcodes_Dec2021.csv")
summary(postcodes)

# NSPL 2021 provides longitude & latitude for postcodes
# obtained at: https://geoportal.statistics.gov.uk/datasets/national-statistics-postcode-lookup-2021-census-november-2022/about
nspl21 <- read_csv("data/geos/NSPL21_NOV_2022_UK/Data/NSPL21_NOV_2022_UK.csv")

# left join long & lat using pcds
postcodes <- nspl21 %>%
  dplyr::select(pcds, lat, long) %>%
  left_join(x = postcodes, by = c("POSTCODE" = "pcds"))
# check NA's
summary(postcodes)

# convert postcodes to sf using st_as_sf()
# X is longitude, Y is latitude
postcodes_sf <- st_as_sf(postcodes[c(1,4,5,6)], coords = c("long", "lat"), crs = 4326)
# save as shapefile
#st_write(postcodes_sf, "data/geos/postcodes_shapefile/UK_postcodes_2021.shp")

# England postcodes
postcodes_sf_E <- filter(postcodes_sf, COUNTRY == 'England')
# Wales postcodes
postcodes_sf_W <- filter(postcodes_sf, COUNTRY == 'Wales')
# N Ireland postcodes
postcodes_sf_NI <- filter(postcodes_sf, COUNTRY == 'Northern Ireland')

# # view map
# library(tmap)
# tmap_mode("view")
# tm_shape(postcodes_sf_NI) + tm_dots()


## get England table
old_code_col = "geo_code"
# insert new boundaries code column
new_code_col = "LSOA21CD"
# insert postcodes code column
pcd_col = "POSTCODE"

translate_england <- translate_geo(old_E, new_E, postcodes_sf_E)
write_csv(translate_england, "data/translate_england.csv")

## get Wales table
old_code_col = "geo_code"
# insert new boundaries code column
new_code_col = "LSOA21CD"
# insert postcodes code column
pcd_col = "POSTCODE"

translate_wales <- translate_geo(old_W, new_W, postcodes_sf_W)
write_csv(translate_wales, "data/translate_wales.csv")


## get Northern Ireland table
old_code_col = "geo_code"
# insert new boundaries code column
new_code_col = "SDZ2021_cd"
# insert postcodes code column
pcd_col = "POSTCODE"

translate_nire <- translate_geo(old_NI, new_NI, postcodes_sf_NI)
write_csv(translate_nire, "data/translate_nireland.csv")


### 5. Join all data ----------------------------------------------------------

# read in the BII files
