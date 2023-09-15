### This R file contains the code to construct the updated Broadband Infrastructure Index (BII)
# new BII 2023 for ALL UK
# this is a relative index, i.e., we do not calculate raw 'scores', only rank LSOAs relative to each other

# the variables included to construct the BII are queried directly from the:
# FACT_OPERATOR, UPC_OUTPUT, FACT_POSTCODE_TECH and FACT_SPEED_TEST tables in the Snowflake database
# see "BII_query_all_in_one.rtf" file in 'BII' directory for the full SQL query

# load packages
library(tidyverse)

# optional -- depends on your working directory
setwd("~/Desktop/Point-Topic/pointtopic/BII/")

# read the query output file
new_bii <- read_csv("BII_query_output_July2023.csv")

# create new COUNTRY column based on LSOA codes
new_bii$COUNTRY <- ifelse(grepl('^E', new_bii$LSOA), "England", # starts with E
                          ifelse(grepl('^W', new_bii$LSOA), "Wales", # starts with W
                                 ifelse(grepl('^S', new_bii$LSOA), "Scotland", "Northern Ireland")))

### 1. Exploratory Data Analysis ----------------------------------------------
# is GFAST relevant?
summary(new_bii$GFAST_AVAILABILITY)
# plot
new_bii %>% ggplot(aes(x = GFAST_AVAILABILITY)) +
  geom_density()

# is DSL relevant?
summary(new_bii$DSL_AVAILABILITY)
# plot
new_bii %>% ggplot(aes(x = DSL_AVAILABILITY)) +
  geom_boxplot()

# is Cable relevant?
summary(new_bii$CABLE_AVAILABILITY)
# plot
new_bii %>% ggplot(aes(x = CABLE_AVAILABILITY)) +
  geom_density()

# operator count
summary(new_bii$OPERATOR_COUNT)
# plot
new_bii %>% ggplot(aes(x = OPERATOR_COUNT)) +
  geom_boxplot()

### 2. creating the index -----------------------------------------------------
# from the EDA, we choose to drop Gfast as it does not add much information on broadband availability
# data suggests almost no one uses Gfast anymore
new_bii <- dplyr::select(new_bii, -GFAST_AVAILABILITY)

# create empty rank columns for each variable
new_bii <- mutate(new_bii,
                  fttp_rank = NA,
                  fttc_rank = NA,
                  dsl_rank = NA,
                  cable_rank = NA,
                  opscount_rank = NA,
                  down_rank = NA,
                  up_rank = NA)

# create new tables for each nation
# England
new_bii_england <- filter(new_bii, COUNTRY == 'England')
# Wales
new_bii_wales <- filter(new_bii, COUNTRY == 'Wales')
# Scotland
new_bii_scotland <- filter(new_bii, COUNTRY == 'Scotland')
# Northern Ireland
new_bii_nireland <- filter(new_bii, COUNTRY == 'Northern Ireland')

# FOR EACH NATION: creating LSOA ranking for broadband technology availability variables (FTTP, FTTC, DSL, Cable)
# 0-100 % availability -- higher (more availability) is better

# FTTP
# England
new_bii_england$fttp_rank[order(new_bii_england$FTTP_AVAILABILITY, decreasing = TRUE)] <- 1:nrow(new_bii_england)
# Wales
new_bii_wales$fttp_rank[order(new_bii_wales$FTTP_AVAILABILITY, decreasing = TRUE)] <- 1:nrow(new_bii_wales)
# Scotland
new_bii_scotland$fttp_rank[order(new_bii_scotland$FTTP_AVAILABILITY, decreasing = TRUE)] <- 1:nrow(new_bii_scotland)
# Northern Ireland
new_bii_nireland$fttp_rank[order(new_bii_nireland$FTTP_AVAILABILITY, decreasing = TRUE)] <- 1:nrow(new_bii_nireland)


# FTTC
# England
new_bii_england$fttc_rank[order(new_bii_england$FTTC_AVAILABILITY, decreasing = TRUE)] <- 1:nrow(new_bii_england)
# Wales
new_bii_wales$fttc_rank[order(new_bii_wales$FTTC_AVAILABILITY, decreasing = TRUE)] <- 1:nrow(new_bii_wales)
# Scotland
new_bii_scotland$fttc_rank[order(new_bii_scotland$FTTC_AVAILABILITY, decreasing = TRUE)] <- 1:nrow(new_bii_scotland)
# Northern Ireland
new_bii_nireland$fttc_rank[order(new_bii_nireland$FTTC_AVAILABILITY, decreasing = TRUE)] <- 1:nrow(new_bii_nireland)


# DSL
# England
new_bii_england$dsl_rank[order(new_bii_england$DSL_AVAILABILITY, decreasing = TRUE)] <- 1:nrow(new_bii_england)
# Wales
new_bii_wales$dsl_rank[order(new_bii_wales$DSL_AVAILABILITY, decreasing = TRUE)] <- 1:nrow(new_bii_wales)
# Scotland
new_bii_scotland$dsl_rank[order(new_bii_scotland$DSL_AVAILABILITY, decreasing = TRUE)] <- 1:nrow(new_bii_scotland)
# Northern Ireland
new_bii_nireland$dsl_rank[order(new_bii_nireland$DSL_AVAILABILITY, decreasing = TRUE)] <- 1:nrow(new_bii_nireland)


# Cable
# England
new_bii_england$cable_rank[order(new_bii_england$CABLE_AVAILABILITY, decreasing = TRUE)] <- 1:nrow(new_bii_england)
# Wales
new_bii_wales$cable_rank[order(new_bii_wales$CABLE_AVAILABILITY, decreasing = TRUE)] <- 1:nrow(new_bii_wales)
# Scotland
new_bii_scotland$cable_rank[order(new_bii_scotland$CABLE_AVAILABILITY, decreasing = TRUE)] <- 1:nrow(new_bii_scotland)
# Northern Ireland
new_bii_nireland$cable_rank[order(new_bii_nireland$CABLE_AVAILABILITY, decreasing = TRUE)] <- 1:nrow(new_bii_nireland)


# FOR EACH NATION: LSOA ranking for OPERATOR_COUNT -- higher (more operators) is better
# England
new_bii_england$opscount_rank[order(new_bii_england$OPERATOR_COUNT, decreasing = TRUE)] <- 1:nrow(new_bii_england)
# Wales
new_bii_wales$opscount_rank[order(new_bii_wales$OPERATOR_COUNT, decreasing = TRUE)] <- 1:nrow(new_bii_wales)
# Scotland
new_bii_scotland$opscount_rank[order(new_bii_scotland$OPERATOR_COUNT, decreasing = TRUE)] <- 1:nrow(new_bii_scotland)
# Northern Ireland
new_bii_nireland$opscount_rank[order(new_bii_nireland$OPERATOR_COUNT, decreasing = TRUE)] <- 1:nrow(new_bii_nireland)


# FOR EACH NATION: LSOA ranking for speed tests -- higher (faster) is better
# Down_speed
# England
new_bii_england$down_rank[order(new_bii_england$DOWN_MBPS, decreasing = TRUE)] <- 1:nrow(new_bii_england)
# Wales
new_bii_wales$down_rank[order(new_bii_wales$DOWN_MBPS, decreasing = TRUE)] <- 1:nrow(new_bii_wales)
# Scotland
new_bii_scotland$down_rank[order(new_bii_scotland$DOWN_MBPS, decreasing = TRUE)] <- 1:nrow(new_bii_scotland)
# Northern Ireland
new_bii_nireland$down_rank[order(new_bii_nireland$DOWN_MBPS, decreasing = TRUE)] <- 1:nrow(new_bii_nireland)

# Up_speed
# England
new_bii_england$up_rank[order(new_bii_england$UP_MBPS, decreasing = TRUE)] <- 1:nrow(new_bii_england)
# Wales
new_bii_wales$up_rank[order(new_bii_wales$UP_MBPS, decreasing = TRUE)] <- 1:nrow(new_bii_wales)
# Scotland
new_bii_scotland$up_rank[order(new_bii_scotland$UP_MBPS, decreasing = TRUE)] <- 1:nrow(new_bii_scotland)
# Northern Ireland
new_bii_nireland$up_rank[order(new_bii_nireland$UP_MBPS, decreasing = TRUE)] <- 1:nrow(new_bii_nireland)



# FOR EACH NATION: create column for total sum of ranks for each LSOA
# England
new_bii_england$rank_sum <- (new_bii_england$fttp_rank)+(new_bii_england$fttc_rank)+(new_bii_england$dsl_rank)+
  (new_bii_england$cable_rank)+(new_bii_england$opscount_rank)+(new_bii_england$down_rank)+(new_bii_england$up_rank)
# Wales
new_bii_wales$rank_sum <- (new_bii_wales$fttp_rank)+(new_bii_wales$fttc_rank)+(new_bii_wales$dsl_rank)+
  (new_bii_wales$cable_rank)+(new_bii_wales$opscount_rank)+(new_bii_wales$down_rank)+(new_bii_wales$up_rank)
# Scotland
new_bii_scotland$rank_sum <- (new_bii_scotland$fttp_rank)+(new_bii_scotland$fttc_rank)+(new_bii_scotland$dsl_rank)+
  (new_bii_scotland$cable_rank)+(new_bii_scotland$opscount_rank)+(new_bii_scotland$down_rank)+(new_bii_scotland$up_rank)
# Northern Ireland
new_bii_nireland$rank_sum <- (new_bii_nireland$fttp_rank)+(new_bii_nireland$fttc_rank)+(new_bii_nireland$dsl_rank)+
  (new_bii_nireland$cable_rank)+(new_bii_nireland$opscount_rank)+(new_bii_nireland$down_rank)+(new_bii_nireland$up_rank)


# FOR EACH NATION: use rank_sum to create final relative rank: 1 is best (lower overall rank is better)
# England
new_bii_england$final_RANK[order(new_bii_england$rank_sum)] <- 1:nrow(new_bii_england)
# Wales
new_bii_wales$final_RANK[order(new_bii_wales$rank_sum)] <- 1:nrow(new_bii_wales)
# Scotland
new_bii_scotland$final_RANK[order(new_bii_scotland$rank_sum)] <- 1:nrow(new_bii_scotland)
# Northern Ireland
new_bii_nireland$final_RANK[order(new_bii_nireland$rank_sum)] <- 1:nrow(new_bii_nireland)

# summaries of each nation BII
summary(new_bii_england)
summary(new_bii_wales)
summary(new_bii_scotland)
summary(new_bii_nireland)


# save each nation BII as .csv
write_csv(new_bii_england, "BII_output/newBII_output2023_England.csv")
write_csv(new_bii_wales, "BII_output/newBII_output2023_Wales.csv")
write_csv(new_bii_scotland, "BII_output/newBII_output2023_Scotland.csv")
write_csv(new_bii_nireland, "BII_output/newBII_output2023_NIreland.csv")
