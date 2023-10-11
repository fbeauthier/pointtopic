# load packages
library(haven)
library(ggpubr)
library(tidyverse)
library(qacEDA)
library(gstat)
library(relaimpo)

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

oa_fixedlines <- read_csv("data/ofcom21_fixedlines_oa_130723.csv")

# read Ofcom coverage (residential)
oa_coverage <- read_csv("data/202109_fixed_oa11_res_coverage.csv")
colnames(oa_coverage)

# select columns
oa_coverage <- dplyr::select(oa_coverage, 1, 2, 3, 26:37)

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
  dplyr::select(1, 2) %>%
  left_join(x = oa_fixedlines, by = c("oa11" = "output_area"))

# rename premises (residential) to households
oa_fixedlines <- rename(oa_fixedlines,
                        households = `All Premises`)

# test fixed lines count <= households (in theory penetration rate = 1 maximum)
oa_fixedlines$test2 <- ifelse(oa_fixedlines$total_lines <= oa_fixedlines$households,0,1)
# view
filter(oa_fixedlines, test2 == 1)

oa_fixedlines <- filter(oa_fixedlines, oa11 %in% joined$OA_code) # filter to England and Wales only

# aggregate to LSOA
oa_fixedlines <- left_join(oa_fixedlines, lookup[2:3], by = c("oa11" = "oa21cd"))
oa_fixedlines <- distinct(oa_fixedlines)
  
LSOA_fixedlines <- oa_fixedlines %>%
  group_by(lsoa21cd) %>%
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
            households = sum(households))
            

# new column: create a total broadband penetration measure
# Penetration rate (PR) = total number of lines (subscriptions)/ households
# In theory should be <= 1
oa_fixedlines$total_PR <- (oa_fixedlines$total_lines)/(oa_fixedlines$households)

summary(oa_fixedlines)

# penetration rate for high speed broadband lines
oa_fixedlines$PR_highspeed <- (oa_fixedlines$high_speed_count)/(oa_fixedlines$households)

# penetration rate for medium speed broadband lines
oa_fixedlines$PR_med_speed <- (oa_fixedlines$med_speed_count)/(oa_fixedlines$households)

# penetration rate for low speed broadband lines
oa_fixedlines$PR_low_speed <- (oa_fixedlines$low_speed_count)/(oa_fixedlines$households)


# column adjust total PR not above 1
oa_fixedlines$total_PR_ajd <- ifelse(oa_fixedlines$total_PR > 1, 1, oa_fixedlines$total_PR)
summary(oa_fixedlines)

write_csv(oa_fixedlines, "data/ofcom_LSOA_fixed_bbpr_260723.csv")


filter(oa_fixedlines, PR_low_speed > 0.5)

oa_fixedlines %>%
  dplyr::select(9:12) %>%
  summarise(per_low = sum(low_speed_count)/sum(total_lines),
            per_medium = sum(med_speed_count)/sum(total_lines),
            per_high = sum(high_speed_count)/sum(total_lines)) %>%
  round(digits = 3)

### 3. BDDI indicators vs broadband penetration rate LSOA ---------------------

LSOA_fixedlines <- read_csv("data/ofcom_LSOA_fixed_bbpr_260723.csv") # 35304

colSums(is.na(LSOA_fixedlines)) # 24 NAs

LSOA_fixedlines <- na.omit(LSOA_fixedlines)
summary(LSOA_fixedlines)

# total PR 3rd Qu.: 0.8909  
IQR(LSOA_fixedlines$total_PR) # 0.09220001
# Maximum value to exclude outliers usually = Q3 + 1.5*IQR
0.8909 + 1.5*0.09220001
# = 1.0292

# descriptive statistics
LSOA_fixedlines %>%
  filter(total_PR < 1.03) %>%
  ggplot(aes(x=total_PR)) +
  geom_boxplot()

# filter data where PR <= 1.03 (see calc above)
LSOA_fixedlines %>%
  filter(total_PR <= 1.03) # 34,839 rows (LSOAs)
# 98.75% data remaining is OK

LSOA_fixedlines <- filter(LSOA_fixedlines, total_PR <= 1.03)
# view distribution
LSOA_fixedlines %>%
  ggplot(aes(x=total_PR)) +
  geom_density()
# right skew


## LSOA fixed lines vs socio-demographic indicators: England + Wales ##

# requires raw file created 07.07.23 + income deprivation 2019
df1 <- read_csv("data/EngWales/070723_raw.csv")
# income
income <- read_csv("data/EngWales/2019_IoD_Englandincome.csv")
# modify income LSOAs to 2021 using new lookup
lookup <- read_csv("data/Lookups/LSOA(2011)_to_LSOA(2021).csv")

# left join 2021 LSOAs to income
income <- lookup %>%
  dplyr::select(2,4) %>%
  left_join(x = income, by = c("LSOA code (2011)" = "LSOA11CD"))

# left join income to df1
df1 <- income %>%
  dplyr::select(5,6) %>%
  rename(income_dep_rate = 1) %>%
  left_join(x = df1, by = c("LSOA_code" = "LSOA21CD"))
# remove duplicates
df1 <- distinct(df1, LSOA_code, .keep_all = TRUE)

colSums(is.na(df1))
na.omit(df1) # 31,799 non-NAs
31799/nrow(df1)

df1 <- na.omit(df1)
# final demographic regressors table obtained #

# join total PR with df1
df1_for_lm <- df1 %>%
  dplyr::select(LSOA_code, prop_over65, prop_hh_children, prop_hh_disability, prop_hh_sochousing, prop_hh_dep_edu,
                prop_age16_noedu, income_dep_rate, AvgOfBII_total) %>%
  inner_join(y = LSOA_fixedlines[,c("lsoa21cd","total_PR","PR_highspeed","PR_med_speed","PR_low_speed")],
             by = c("LSOA_code" = "lsoa21cd")) # PR other than total_PR not filtered

str(df1_for_lm) # should all be numeric
colSums(is.na(df1_for_lm))

# Linear Model + rela_impo
colnames(df1_for_lm)
# regressing LSOA broadband penetration rate on 7 socio-demographic indicators
lm1 <- lm(total_PR ~ prop_over65 + prop_hh_children + prop_hh_disability + prop_hh_sochousing + prop_hh_dep_edu + prop_age16_noedu + income_dep_rate, 
          data = df1_for_lm)

summary(lm1)

calc.relimp(lm1, type = "lmg", rela = TRUE)

