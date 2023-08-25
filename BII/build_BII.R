### This R file contains the code to construct the updated Broadband Infrastructure Index (BII)
# new BII 2023 for ALL UK
# this is a relative index, i.e., we do not calculate raw 'scores', only rank LSOAs relative to each other

# the variables included to construct the BII are queried directly from the:
# FACT_OPERATOR, UPC_OUTPUT, FACT_POSTCODE_TECH and FACT_SPEED_TEST tables in the Snowflake database
# see XXDOC for the full SQL query

# load packages
library(tidyverse)

#setwd("BII/")

# read the query output file (in same directory as this .R file)
new_bii <- read_csv("BIIquery_2023.csv")

### 1. exploratory data analysis ----------------------------------------------
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
# create empty rank columns for each variable
new_bii <- mutate(new_bii,
                  fttp_rank = NA,
                  fttc_rank = NA,
                  gfast_rank = NA,
                  dsl_rank = NA,
                  cable_rank = NA,
                  opscount_rank = NA,
                  down_rank = NA,
                  up_rank = NA)

# from the EDA, we choose to drop Gfast as it does not contribute/add much information 
new_bii <- dplyr::select(new_bii, -c(GFAST_AVAILABILITY, gfast_rank))

# creating LSOA ranking for broadband technology availability variables (FTTP, FTTC, DSL, Cable)
# 0-1 % availability -- higher is better

# FTTP
new_bii$fttp_rank[order(new_bii$FTTP_AVAILABILITY, decreasing = TRUE)] <- 1:nrow(new_bii)
new_bii[,c("FTTP_AVAILABILITY", "fttp_rank")]

# FTTC
new_bii$fttc_rank[order(new_bii$FTTC_AVAILABILITY, decreasing = TRUE)] <- 1:nrow(new_bii)
new_bii[,c("FTTC_AVAILABILITY", "fttc_rank")]

# DSL
new_bii$dsl_rank[order(new_bii$DSL_AVAILABILITY, decreasing = TRUE)] <- 1:nrow(new_bii)
new_bii[,c("DSL_AVAILABILITY", "dsl_rank")]

# Cable
new_bii$cable_rank[order(new_bii$CABLE_AVAILABILITY, decreasing = TRUE)] <- 1:nrow(new_bii)
new_bii[,c("CABLE_AVAILABILITY", "cable_rank")]


# LSOA ranking for OPERATOR_COUNT -- higher is better
new_bii$opscount_rank[order(new_bii$OPERATOR_COUNT, decreasing = TRUE)] <- 1:nrow(new_bii)
new_bii[,c("OPERATOR_COUNT", "opscount_rank")]


# LSOA ranking for speed tests -- higher is better
# Down_speed
new_bii$down_rank[order(new_bii$DOWN_MBPS, decreasing = TRUE)] <- 1:nrow(new_bii)
new_bii[,c("DOWN_MBPS", "down_rank")]

# Up_speed
new_bii$up_rank[order(new_bii$UP_MBPS, decreasing = TRUE)] <- 1:nrow(new_bii)
new_bii[,c("UP_MBPS", "up_rank")]


# create column for total sum of ranks for each LSOA
new_bii$rank_sum <- (new_bii$fttp_rank)+(new_bii$fttc_rank)+(new_bii$dsl_rank)+(new_bii$cable_rank)+
  (new_bii$opscount_rank)+(new_bii$down_rank)+(new_bii$up_rank)

# final relative rank: 1 is best (lower overall rank is better)
new_bii$RANK[order(new_bii$rank_sum)] <- 1:nrow(new_bii)


# save as .csv
write_csv(new_bii, "newBII_2023.csv")