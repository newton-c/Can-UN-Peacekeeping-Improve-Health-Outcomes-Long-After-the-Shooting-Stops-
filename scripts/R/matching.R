library(dplyr)
library(haven)
library(MatchIt)
library(modelsummary)

d <- read_dta("data/hale_data.dta")
d$pko_binary <- ifelse(d$pko_years > 0, 1, 0)

match_data <- d %>%
    select(year, ccode, hale, pko_years, pko_binary, total_violence_1000,
           osv_1000, brd_1000, pkoxdeath, pkoxosv, pkoxbrd, hdi_knn, helog_knn,
           civilwarborder, urbangrowth_knn, gini_knn, tropical, xpolity_knn,
           ef_knn, y05, y10, y15)

cem_data <- matchit(pko_binary ~ total_violence_1000 + tropical +
                    xpolity_knn + year, data = match_data, method = "cem")

cem_data <- match.data(cem_data)

write_dta(cem_data, "data/matched_data.dta", version = 13)
