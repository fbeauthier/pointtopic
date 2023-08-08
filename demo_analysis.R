# load packages
library(haven)
library(ggpubr)
library(tidyverse)
library(qacEDA)
library(gstat)

lookup <- read_csv("data/Lookups/NSP21CL_MAY23_UK_LU.csv") # 2021
lookup <- read_csv("data/Lookups/NSPCL_2011_UK_LU.csv") # 2011

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

### Code-Point v Census analysis ----------------------------------
# queried from snowflake raw codepoint UK 2022 table
codepoint <- read_csv("data/demos/codepoint_raw_households_UK.csv")
str(codepoint)

lookup <- read_csv("data/Lookups/NSP21CL_MAY23_UK_LU.csv")
str(lookup)
lookup <- dplyr::select(lookup,
                        pcds, oa21cd, lsoa21cd) # select postcode pcds

colSums(is.na(lookup)) # 10,741 oa21cd, lsoa21cd missing

# left join lookup OAs to codepoint
codepoint <- left_join(codepoint, lookup, by = c("RM_POSTCODE" = "pcds"))

colSums(is.na(codepoint))
# NAs = 0!
length(unique(codepoint$lsoa21cd))

# group by OA
codepoint_lsoa <- codepoint %>%
  dplyr::select(2,4) %>%
  group_by(lsoa21cd) %>%
  summarise(cp_households = sum(HOUSEHOLDS))


# read census households 2021 data
census <- read_csv("data/demos/census2021-ts041/census2021-ts041-lsoa.csv") # just Eng + Wales

# compare census with code-point at LSOA level
joined <- census %>%
  dplyr::select(3,4) %>%
  rename(LSOA_code = 1,
         cs_households = 2) %>%
  left_join(y = codepoint_lsoa, by = c("LSOA_code" = "lsoa21cd"))

# census count - codepoint count
joined$hh_cs_cp <- (joined$cs_households) - (joined$cp_households)
# absolute values
joined$hh_cs_cp_abs <- abs((joined$cs_households) - (joined$cp_households))

colSums(is.na(joined))
summary(joined)

# plot
joined %>%
  ggplot(aes(x=hh_cs_cp_abs)) +
  geom_histogram(binwidth=20, fill="blue", alpha=0.9) +
  geom_vline(xintercept = 58, linetype="dotted", 
               color = "red", size = 1.5)

# count
joined %>%
  filter(hh_cs_cp_abs > 58) %>%
  nrow()

joined %>%
  summarise(sum(cs_households),
            sum(cp_households))

write_csv(joined, "data/demos/hh_codepoint_joined_lsoa.csv")

### Code-Point v Ofcom v IDBR businesses --------------------------------------
colnames(lookup)
# using MSOAs

## Code-Point
cp_busprems <- read_csv("data/CP_bus_counts_2021.csv")
# test post-codes match
cp_busprems$test <- ifelse(cp_busprems$RM_POSTCODE == cp_busprems$POSTCODE_SORT, 0, 1)
unique(cp_busprems$test) # not all the same

filter(cp_busprems, test == 1)
# USE RM_POSTCODE

# use "msoa11cd" and "pcds" in look-up
length(unique(lookup$msoa11cd)) # 8,484
lookup_msoa <- dplyr::select(lookup,
                 pcds, msoa11cd)

# left join look-up to code-point
cp_busprems <- left_join(cp_busprems, lookup_msoa, by = c("RM_POSTCODE" = "pcds"))

# without NAs
cp_busprems_omitNA <- na.omit(cp_busprems)
  

# group_by MSOA
cp_busprems_MSOA <- cp_busprems_omitNA %>%
  dplyr::select(3,5) %>%
  group_by(msoa11cd) %>%
  summarise(CP_bus_prems = sum(DP_BUS_Y21M01))

summary(cp_busprems_MSOA)


## IDBR businesses - "districts, counties and unitary authorities"
idbr <- readxl::read_xlsx("data/nomis_bus_counts_idbr_2019.xlsx")

idbr <- idbr %>%
  rename(MSOA = 1, Total = 2) %>%
  filter(!row_number() %in% c(1:7))

# edit MSOA to codes only
idbr$MSOA <- sapply(strsplit(idbr$MSOA, ":"), "[", 2)
idbr$MSOA <- sapply(strsplit(idbr$MSOA, " "), "[", 1)
head(idbr$MSOA)
idbr$Total <- as.integer(idbr$Total)
summary(idbr)

## join IDBR and C-P
all_bus_prems <- full_join(cp_busprems_MSOA, idbr, by = c("msoa11cd" = "MSOA")) %>%
  rename(idbr_bus_prems = 3)

summary(all_bus_prems)


## create differences
# C-P minus IDBR
all_bus_prems$CP_v_idbr <- (all_bus_prems$CP_bus_prems) - (all_bus_prems$idbr_bus_prems)

## absolute values
all_bus_prems$CP_v_idbr_ab <- abs(all_bus_prems$CP_v_idbr)
summary(all_bus_prems)

# total sums of prems
all_bus_prems %>%
  drop_na() %>%
  summarise(sum(CP_bus_prems),
            sum(idbr_bus_prems))

# absolute differences:
all_bus_prems %>%
  filter(CP_v_idbr_ab > 223) %>%
  nrow()

# view differences
all_bus_prems %>% ggplot(aes(x = CP_v_idbr_ab)) +
  geom_histogram(bins = 50) +
  geom_vline(xintercept = 223, linetype="dotted", 
             color = "red", size = 1.2)
  

write_csv(all_bus_prems, "data/business_prems2.csv")


## C-P NA analysis
colSums(is.na(cp_busprems))
