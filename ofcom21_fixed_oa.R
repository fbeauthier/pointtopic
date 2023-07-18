# load packages
library(haven)
library(ggpubr)
library(tidyverse)
library(qacEDA)
library(gstat)
library(relaimpo)

#setwd("~/Desktop/Point-Topic")
### 0. Useful if filtering OAs by nation e.g. Eng + Wales (2011) --------------
lookup <- read_csv("data/2011_OAC_Lookup/2011 OAC CSV Lookup.csv")
colnames(lookup)
lookup <- select(lookup, 1:8)

unique(lookup$REGION_OR_COUNTRY_NAME)

# keep OAs for England + Wales only
lookup <- filter(lookup, !REGION_OR_COUNTRY_NAME %in% c("Northern Ireland", "Scotland"))
# 181,408 OAs for England + Wales in total

### 1. low, medium and high speed subscription counts -------------------------
# requires Ofcom "Fixed Output Area: Fixed performance 2011 census output area data" 
# https://www.ofcom.org.uk/research-and-data/multi-sector-research/infrastructure-research/connected-nations-2021/data-downloads


# If binding of files required:
# set path to Ofcom CN2021 dataset directory
# requires Ofcom CN2021 fixed performance data
# file_path <- ("data/cn_performance_postcode/")
# files <- list.files(file_path)
# 
# # bind all files together
# oa_fixedlines <- tibble()
# for (f in files){
#   df <- read.csv(file.path(file_path,f))
#   oa_fixedlines <- oa_fixedlines %>% bind_rows(df)
# }

oa_fixedlines <- read_csv("data/202105_fixed_oa11_performance_r01.csv")

# keeping only count of broadband lines by speed
colnames(oa_fixedlines)
oa_fixedlines <- oa_fixedlines %>%
  select(1, 25:31)

oa_fixedlines <- oa_fixedlines %>%
  rename(Mbps_5to10 = 2, 
         Mbps_30to300 = 3,
         Mbps_30ormore = 4,
         Mbps_300ormore = 5,
         Mbps_2to5 = 6,
         Mbps_2orless = 7,
         Mbps_10to30 = 8)

# reorder by speed
oa_fixedlines <- oa_fixedlines[, c(1,7,6,2,8,3,5,4)]

# new column: count number of low-speed lines <10 Mbps
oa_fixedlines$low_speed_count <- (oa_fixedlines$Mbps_2orless) + (oa_fixedlines$Mbps_2to5) + (oa_fixedlines$Mbps_5to10)

# new column: number of medium-speed lines 10-30 Mbps
oa_fixedlines$med_speed_count <- oa_fixedlines$Mbps_10to30

# new column: number of high-speed lines >30 Mbps
oa_fixedlines$high_speed_count <- (oa_fixedlines$Mbps_30to300) + (oa_fixedlines$Mbps_300ormore)

# check: high-speed count should equal >30 Mbps column
oa_fixedlines$test1 <- ifelse(oa_fixedlines$high_speed_count == oa_fixedlines$Mbps_30ormore, 0, 1)
unique(oa_fixedlines$test1) # no 0s

# new column: total broadband lines
oa_fixedlines$total_lines <- (oa_fixedlines$low_speed_count)+(oa_fixedlines$med_speed_count)+(oa_fixedlines$high_speed_count)

# drop test column
oa_fixedlines <- select(oa_fixedlines, -test1)
oa_fixedlines <- oa_fixedlines[1:nrow(oa_fixedlines)-1, ]

# save as .csv -- just subscription counts
#write_csv(oa_fixedlines, "data/ofcom21_fixedlines_oa_140723.csv")

### 2. total broadband penetration rates --------------------------------------

## requires Ofcom "Fixed output area: Fixed coverage 2011 census output area data"
# RESIDENTIAL coverage file
# https://www.ofcom.org.uk/research-and-data/multi-sector-research/infrastructure-research/connected-nations-2021/data-downloads

# read Ofcom coverage (residential)
oa_coverage <- read_csv("data/cn-2021-fixed-oa11-coverage/202109_fixed_oa11_res_coverage_r01.csv")
colnames(oa_coverage)

# select columns
oa_coverage <- select(oa_coverage, 1, 2, 3, 26:37)

# check number of OAs
length(unique(oa_coverage$output_area))
# 60 less OAs than ofcom fixed lines data
# not a problem to aggregate to LSOA

# test "All premises" vs. "All Matched Premises"
oa_coverage$premise_diff <- ifelse(oa_coverage$`All Premises` == oa_coverage$`All Matched Premises`, 0, 1)
# view difference
filter(oa_coverage, premise_diff == 1)

# left join premises to Ofcom line counts
oa_fixedlines <- oa_coverage %>%
  select(1, 2) %>%
  left_join(x = oa_fixedlines, by = c("oa11" = "output_area"))

# rename premises (residential) to households
oa_fixedlines <- rename(oa_fixedlines,
                        households = `All Premises`)

# test fixed lines count <= households (in theory penetration rate = 1 maximum)
oa_fixedlines$test2 <- ifelse(oa_fixedlines$total_lines <= oa_fixedlines$households,0,1)
# view
filter(oa_fixedlines, test2 == 1)

# new column: create a total broadband penetration measure
# Penetration rate (PR) = total number of lines (subscriptions)/ households
# In theory should be <= 1
oa_fixedlines$total_PR <- (oa_fixedlines$total_lines)/(oa_fixedlines$households)

summary(oa_fixedlines)

# potentially interesting: penetration rate for high speed broadband lines
# hh_oa$highspeed_PR <- (hh_oa$high_speed_count)/(hh_oa$households)

#write_csv(oa_fixedlines, "data/fixed_bbpr_140723.csv")

### 3. BDDI indicators vs broadband penetration rate --------------------------

#oa_fixedlines <- read_csv("data/fixed_bbpr_140723.csv")

oa_fixedlines <- oa_fixedlines %>%
  select(-test2) %>%
  na.omit()
# 231,902 to 231,809 rows (OAs)

# descriptive statistics about penetration rate
oa_fixedlines %>%
  filter(total_PR < 10) %>%
  ggplot(aes(x=total_PR)) +
  geom_boxplot()

summary(oa_fixedlines)
#  3rd Qu.: 0.9052  
IQR(oa_fixedlines$total_PR) # 0.129053

# Maximum value to exclude outliers usually = Q3 + 1.5*IQR
0.9052 + 1.5*0.129053
# = 1.09878

# filter data where PR <= 1.1
oa_fixedlines %>%
  filter(total_PR <= 1.1) # 225,838 rows (OAs)
# 97% data remaining is OK

oa_fixedlines <- filter(oa_fixedlines, total_PR <= 1.1)
# view distribution
oa_fixedlines %>%
  ggplot(aes(x=total_PR)) +
  geom_density()
# right skew

## read look-up for OA to LSOA
# pulled from snowflake upc table
lookup <- read_csv("data/UPC_OA_lookup.csv")
str(lookup)

# remove duplicates in look-up
lookup <- lookup %>%
  distinct(COA_CODE, .keep_all = TRUE)

# left join LSOAs
oa_fixedlines <- left_join(oa_fixedlines, lookup, by = c("oa11" = "COA_CODE"))


# aggreate to LSOA: oa_fixedlines groupby LSOA 
LSOA_fixedlines <- oa_fixedlines %>%
  group_by(LSOA) %>%
  summarise(Mbps_2orless = sum(Mbps_2orless),
            Mbps_2to5 = sum(Mbps_2to5),
            Mbps_5to10 = sum(Mbps_5to10),
            Mbps_10to30 = sum(Mbps_10to30),
            Mbps_30to300 = sum(Mbps_30to300),
            Mbps_300ormore = sum(Mbps_300ormore),
            Mbps_30ormore = sum(Mbps_30ormore),
            low_speed_count = sum(low_speed_count),
            med_speed_count = sum(med_speed_count),
            high_speed_count = sum(high_speed_count),
            total_lines = sum(total_lines),
            households = sum(households),
            total_PR = mean(total_PR))


## LSOA fixed lines vs socio-demographic indicators: England + Wales ##
## requires raw file created 07.07.23
df1 <- read_csv("data/EngWales/070723_raw.csv")

# MSOA 2018 income estimate data
# https://www.ons.gov.uk/employmentandlabourmarket/peopleinwork/earningsandworkinghours/datasets/smallareaincomeestimatesformiddlelayersuperoutputareasenglandandwales

msoa_income2018 <- readxl::read_xlsx("data/EngWales/incomeestimatesforsmallareasdatasetfinancialyearending20181.xlsx",
                                     sheet = "Net income after housing costs")

# rename columns, select
msoa_income2018 <- msoa_income2018 %>%
  rename(MSOA_code = 1,
         MSOA_name = 2,
         annual_disposable_income = 7) %>%  # Net annual income after housing costs (£)
  dplyr::select(1,2,7)

msoa_income2018 <- msoa_income2018[5:nrow(msoa_income2018),]

## read look-up for LSOA to MSOA
# pulled from snowflake upc table
msoa_lookup <- read_csv("data/UPC_MSOA_lookup.csv")

# remove duplicates in look-up
msoa_lookup <- msoa_lookup %>%
  distinct(LSOA, .keep_all = TRUE)

# left join MSOAs by LSOAs
df1 <- left_join(df1, msoa_lookup, by = c("LSOA_code" = "LSOA"))
dplyr::select(df1, LSOA_code, MSOA_AND_IM)

# left join MSOA income to df1
df1 <- left_join(df1, msoa_income2018, by = c("MSOA_AND_IM" = "MSOA_code"))
class(df1$annual_disposable_income)
# convert to numeric
df1$annual_disposable_income <- as.numeric(df1$annual_disposable_income)

## left join Ofcom LSOA fixed lines to df1
df1 <- LSOA_fixedlines %>%
  dplyr::select(1, 12:14) %>%
  left_join(x = df1, by = c("LSOA_code" = "LSOA"))

colSums(is.na(df1))
# 2025 BII for LSOA missing, 2028 PR LSOAs missing

# check difference in households count census (x) vs ofcom (y)
df1$households_diff <- (df1$households.y) - (df1$households.x) 
summary(df1$households_diff)


# Linear Model + rela_impo
# select variables, omit NAs
colnames(df1)
df1_forlm <- df1 %>%
  ungroup() %>%
  dplyr::select(prop_over65, prop_hh_children, prop_hh_disability, prop_hh_sochousing, prop_hh_dep_edu,
                prop_age16_noedu, AvgOfBII_total, annual_disposable_income, total_lines, total_PR) %>%
  drop_na()

str(df1_forlm) # should all be numeric

# run linear model
colnames(df1_forlm)
# regressing LSOA broadband penetration rate on 7 socio-demographic indicators
lm1 <- lm(total_PR ~ prop_over65 + prop_hh_children + prop_hh_disability + prop_hh_sochousing + prop_hh_dep_edu + prop_age16_noedu + annual_disposable_income, 
          data = df1_forlm)

summary(lm1)

lm1_relimp <- calc.relimp(lm1, type = "lmg")
lm1_relimp