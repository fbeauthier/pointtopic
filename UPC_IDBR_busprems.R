library(ggpubr)
library(tidyverse)
library(qacEDA)
library(gstat)

lookup

### UPC estimated business premises 20XX ----

### IDBR business premises 20XX ----
# IDBR UK Business Counts - enterprises by industry and employment size band

# change directory/file path as required #
idbr <- readxl::read_xlsx("data/demos/Code-Point analysis/nomis_bus_counts_idbr_2019.xlsx")

idbr <- idbr %>%
  rename(MSOA = 1, Total = 2) %>%
  filter(!row_number() %in% c(1:7))

# edit MSOA to codes only
idbr$MSOA <- sapply(strsplit(idbr$MSOA, ":"), "[", 2)
idbr$MSOA <- sapply(strsplit(idbr$MSOA, " "), "[", 1)
head(idbr$MSOA)
idbr$Total <- as.integer(idbr$Total)
summary(idbr)

### IDBR local units 20XX ----

