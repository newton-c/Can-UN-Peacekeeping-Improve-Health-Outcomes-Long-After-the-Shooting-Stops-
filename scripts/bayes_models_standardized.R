library(dplyr)
library(haven)
library(rethinking)

d_s <- read_dta("data/hale_data_standardized.dta")

# SD fron table 1 added---------------------------------------------------------
# Using DALY 15-44 averaged for fe/males
# PKO, additive.
