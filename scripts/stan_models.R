library(dplyr)
library(haven)
library(rstan)

d_s <- read_dta("data/hale_data_standardized.dta")

predictors <- d_s %>%
    select(pko_years, total_violence_1000, pkoxdeath, civilwarborder,
           helog_knn, hdi_knn, urbangrowth_knn, gini_knn, tropical, xpolity_knn,
           ef_knn, y05, y10, y15)

y <- d_s$hale

stan_data <- list(
    N = 681,
    K = 14,
    X = predictors,
    y = d_s$hale
)

fit_rstan  <- stan(
    file = "scripts/int_tv_stn_reg.stan",
    data = stan_data,
    warmup = 500,
    iter = 1000,
    chains = 4,
    cores = 2
)

fit_rstan



fit_rstan  <- stan(
    file = "scripts/int_tv_stn_inform.stan",
    data = stan_data,
    warmup = 500,
    iter = 1000,
    chains = 4,
    cores = 2
)

fit_rstan
