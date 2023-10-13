### Demographic Analysis of P-T UPC table v. Census and IDBR
# See "Demographics Analysis report FB 2023" Word document for data sources

# Population & Households analysis is done at LSOA level
# Business premises analysis is done at MSOA level

### 0. Environment ------------------------------------------------------------
# load packages
library(ggpubr)
library(tidyverse)

# optional -- depends on your working directory
setwd("~/Desktop/Point-Topic/pointtopic/UPC_Pop_Prems/")

### 1. Population & Households ------------------------------------------------
# P-T UPC v. Census 2021 population and households analysis: England and Wales
# load UPC population + households
upc_pop_hh <- read_csv("data/UPC_pop_households_2021.csv")

# rename households & population
upc_pop_hh <- rename(upc_pop_hh,
                     upc_households = 3, upc_population = 4)
# round floats
upc_pop_hh$upc_population <- round(upc_pop_hh$upc_population, digits = 0)


# Census 2021
# load population file
census_pop <- read_csv("data/census2021-ts001-population/census2021-ts001-lsoa.csv")
# load households file
census_hh <- read_csv("data/census2021-ts041-households/census2021-ts041-lsoa.csv")

# clean census_pop
census_pop <- census_pop %>%
  dplyr::select(3,4) %>%
  rename(LSOA = 1, cs_population = 2)

# clean census_hh
census_hh <- census_hh %>%
  dplyr::select(3,4) %>%
  rename(LSOA = 1, cs_households = 2)

# join all
pop_hh_joined <- left_join(census_pop, census_hh, by = "LSOA") %>%
  left_join(y = upc_pop_hh[2:4], by = "LSOA")

# create differences
# Census - UPC
# population
pop_hh_joined$cs_upc_pop <- (pop_hh_joined$cs_population)-(pop_hh_joined$upc_population)
pop_hh_joined$cs_upc_pop_A <- abs(pop_hh_joined$cs_upc_pop) # absolute value

# households
pop_hh_joined$cs_upc_hh <- (pop_hh_joined$cs_households)-(pop_hh_joined$upc_households)
pop_hh_joined$cs_upc_hh_A <- abs(pop_hh_joined$cs_upc_hh) # absolute value

summary(pop_hh_joined)

# View total sums of population and households measured by UPC and Census
pop_hh_joined %>%
  summarise(Census_pop = sum(cs_population),
            Census_households = sum(cs_households),
            UPC_pop = sum(upc_population, na.rm = TRUE),
            UPC_households = sum(upc_households, na.rm = TRUE))

# distribution of differences: population
pop_hh_joined %>% ggplot(aes(x = cs_upc_pop_A)) +
  geom_density()

# distribution of differences: households
pop_hh_joined %>% ggplot(aes(x = cs_upc_hh_A)) +
  geom_density()

# write to .csv
write.csv(pop_hh_joined, "data/pop_hhs_joined.csv")

### 2. Business Premises ------------------------------------------------------
# P-T UPC v. IDBR business premises 2019 analysis
# load UPC business premises 2019 file
upc_bus <- read_csv("data/UPC_bussites_2019.csv")
str(upc_bus)
# round to whole numbers
upc_bus$BUS_SITES_TOTAL <- round(upc_bus$BUS_SITES_TOTAL, digits = 0)

# load IDBR business premises file
# convert downloaded .csv to .xlsx workbook first
idbr <- readxl::read_xlsx("data/IDBR_2116081856727279.xlsx")
# check data classes using: str(idbr)

# rename MSOA code column
idbr <- rename(idbr, MSOA = 2)

# select only the first table by rows and cols (remember the file contains many tables one below the other)
idbr <- idbr[9:7209, 2:ncol(idbr)]

# convert count columns to numeric and sum count
# convert necessary columns to numeric
idbr[2:ncol(idbr)] <- lapply(idbr[2:ncol(idbr)], as.numeric)
idbr$idbr_bus_prems <- rowSums(idbr[,2:ncol(idbr)])
# select columns, order alphabetically
idbr <- idbr %>%
  select(MSOA, idbr_bus_prems)

# join upc to idbr
bus_prems_joined <- left_join(idbr, upc_bus, by = "MSOA")
summary(bus_prems_joined)

# create difference and absolute difference
# IDBR - UPC
bus_prems_joined$idbr_upc <- (bus_prems_joined$idbr_bus_prems)-(bus_prems_joined$BUS_SITES_TOTAL)
bus_prems_joined$idbr_upc_A <- abs(bus_prems_joined$idbr_upc)
summary(bus_prems_joined)

# View total sums of business premises measured by UPC and IDBR
bus_prems_joined %>%
  summarise(sum(idbr_bus_prems),
            sum(BUS_SITES_TOTAL))

# distribution of differences
bus_prems_joined %>% ggplot(aes(x = idbr_upc_A)) +
  geom_density()

# write to .csv
write.csv(bus_prems_joined, "data/bus_prems/bus_prems_joined.csv")
