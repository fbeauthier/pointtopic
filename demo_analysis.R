# load packages
library(haven)
library(ggpubr)
library(tidyverse)
library(qacEDA)
library(gstat)

### load data & pre-processing -----
## 1. demographic data downloaded from UPC 2021
pt <- read_csv("data/UPC_sociodemographics.csv")
colnames(pt)
# rename columns
pt <- rename(pt,
             pt_households = HOUSEHOLDS,
             pt_population = POPULATION)

## 2. demographic data downloaded from Census 2021 (England + Wales)
# 188,880 OAs in 2021 Census
# population
census_pop <- read_csv("data/census_oa_population.csv")
# households
census_hh <- read_csv("data/census_oa_hh.csv")

# length(unique(census_pop$`Output Areas Code`))
# length(unique(census_hh$`Output Areas Code`))

# create OA census table
census <- census_pop %>%
  select(1) %>%
  rename(OA_code = `Output Areas Code`) %>%
  distinct(OA_code, .keep_all = TRUE)

# add population
census <- census_pop %>%
  select(1,5) %>%
  group_by(`Output Areas Code`) %>%
  summarise(census_population = sum(Observation)) %>%
  left_join(x = census, by = c("OA_code" = "Output Areas Code"))

# add households
census <- census_hh %>%
  select(1,5) %>%
  group_by(`Output Areas Code`) %>%
  summarise(census_households = sum(Observation)) %>%
  left_join(x = census, by = c("OA_code" = "Output Areas Code"))

## 3. demographic data downloaded from Ofcom CN21
# residential premises coverage (households)
ofcom <- read_csv("data/202109_fixed_oa11_res_coverage.csv")
colnames(ofcom)
ofcom <- select(ofcom, 1:3)
# rename
ofcom <- rename(ofcom, residential_premises = `All Premises`)

## join all 3 tables by OA
OA <- census %>%
  left_join(y = ofcom[1:2], by = c("OA_code" = "output_area")) %>%
  left_join(y = pt, by = c("OA_code" = "COA_CODE"))

colSums(is.na(OA))
# 13,426 values missing in PT, 13,450 in Ofcom --> new census OAs?
# Ofcom uses 2011 census OAs even in 2021
# PT?
filter(OA, is.na(pt_households) | is.na(residential_premises))

# households differences -------

# P-T households vs Ofcom CN21 residential premises
OA$hh_pt_ofcom <- abs((OA$pt_households)-(OA$residential_premises))

summary(OA$hh_pt_ofcom)
# count over 3rd Qu:
OA %>%
  filter(hh_pt_ofcom > 4) %>%
  nrow()

# plot
OA %>%
  ggplot(aes(x=hh_pt_ofcom)) +
  geom_histogram(binwidth=10, fill="blue", alpha=0.9)


# P-T households vs Census 2021
# census households
summary(OA$census_households)
OA %>%
  filter(census_households < 600) %>%
  ggplot(aes(x=census_households)) +
  geom_histogram(binwidth=5, fill="#69b3a2", alpha=0.9)

# P-T households
summary(OA$pt_households)
OA %>%
  filter(pt_households < 600) %>%
  ggplot(aes(x=pt_households)) +
  geom_histogram(binwidth=5, fill="#69b3a2", alpha=0.9)

# absolute differences P-T vs census 2021 households
OA$hh_pt_census <- abs((OA$census_households)-(OA$pt_households))

summary(OA$hh_pt_census)

# plot differences
OA %>%
  ggplot(aes(x=hh_pt_census)) +
  geom_histogram(binwidth=10, fill="blue", alpha=0.9)

# count over 3rd Qu:
OA %>%
  filter(hh_pt_census > 13) %>%
  nrow()


# population differences --------------
# census population
summary(OA$census_population)
OA %>%
  filter(census_population < 1200) %>%
  ggplot(aes(x=census_population)) +
  geom_histogram(binwidth=10, fill="#69b3a2", alpha=0.9)

# P-T population
summary(OA$pt_population)
OA %>%
  filter(pt_population < 1200) %>%
  ggplot(aes(x=pt_population)) +
  geom_histogram(binwidth=10, fill="#69b3a2", alpha=0.9)

# absolute differences P-T vs census 2021 population
OA$pop_pt_census <- abs((OA$census_population)-(OA$pt_population))

summary(OA$pop_pt_census)

# plot differences
OA %>%
  ggplot(aes(x=pop_pt_census)) +
  geom_histogram(binwidth=20, fill="blue", alpha=0.9)

# count
OA %>%
  filter(pop_pt_census > 53) %>%
  nrow()



write_csv(OA, "demo_analysis_clean.csv")


# queried from snowflake raw codepoint UK 2022 table
codepoint <- read_csv("data/demos/codepoint_raw_households_UK.csv")
str(codepoint)

lookup <- read_csv("data/Lookups/NSP21CL_MAY23_UK_LU.csv")
str(lookup)
lookup <- dplyr::select(lookup,
                        pcds, oa21cd, lsoa21cd) # select postcode pcds

colSums(is.na(lookup)) # 10,741 oa21cd, lsoa21cd missing

codepoint <- left_join(codepoint, lookup, by = c("RM_POSTCODE" = "pcds"))

colSums(is.na(codepoint))
# NAs = 0!
length(unique(codepoint$oa21cd))

# group by OA
codepoint_oa <- codepoint %>%
  dplyr::select(2,3) %>%
  group_by(oa21cd) %>%
  summarise(cp_households = sum(HOUSEHOLDS))


# read census households 2021 data
census <- read_csv("data/census2021-ts041/census2021-ts041-oa.csv") # just Eng + Wales

# compare census with codepoint at OA level
joined <- census %>%
  dplyr::select(3,4) %>%
  rename(OA_code = 1,
         cs_household = 2) %>%
  left_join(y = codepoint_oa, by = c("OA_code" = "oa21cd"))

# census count - codepoint count
joined$hh_cs_cp <- (joined$cs_household) - (joined$cp_households)

summary(joined)

# read UPC queried
upc <- read_csv("data/demos/UPC_sociodemographics.csv")

# compare with census & codepoint
joined <- left_join(joined, upc[,2:3], by = c("OA_code" = "COA_CODE"))
joined <- rename(joined, upc_households = 5)

colSums(is.na(joined))
