library(dplyr)
library(haven)

d <- read_dta("data/data21Dec2020_2.dta")

d$y05 <- ifelse(d$year == 2005, 1, 0)
d$y10 <- ifelse(d$year == 2010, 1, 0)
d$y15 <- ifelse(d$year == 2015, 1, 0)
d$tropical <- ifelse(is.na(d$tropical), 1, d$tropical)
d$priorpko <- ifelse(is.na(d$priorpko), 1, d$priorpko)
d$osv_per1000 <- d$osv / 1000
d$deathstotal_and_osv_1000 <- d$deathstotal_and_osv_10000 * 10

d$pkoxdeath <- (d$pkoyearsany * d$deathstotal_and_osv_1000)
d$pkoxosv <- (d$pkoyearsany * d$osv_per1000)
d$pkoxbrd <- (d$pkoyearsany * d$deathstotal_new)

# unstandardized data
d <- d %>%
    mutate("hale" = dale,
        "pko_years" = pkoyearsany,
        "total_violence_1000" = deathstotal_and_osv_1000,
        "osv_1000" = osv_per1000,
        "brd_1000" = deathstotal_new
    ) %>%
    select(year, ccode, hale, pko_years, total_violence_1000, osv_1000,
        brd_1000, pkoxdeath, pkoxosv, pkoxbrd, helog_knn, helog, hdi_knn, hdi,
        civilwarborder, urbangrowth_knn, urbangrowth, gini_knn, gini, tropical,
        xpolity_knn, xpolity, ef_knn, ef, y05, y10, y15, cem_weights)

write_dta(d, "data/hale_data.dta", version = 13)

# standardized data
d_s <- d %>%
    mutate_at(c("pko_years", "total_violence_1000", "osv_1000", "brd_1000",
        "helog_knn", "hdi_knn", "civilwarborder", "urbangrowth_knn", "gini_knn",
        "xpolity_knn", "ef_knn"), ~(scale(.) %>%
    as.vector))

d_s$pkoxdeath <- d_s$pko_years * d_s$total_violence_1000
d_s$pkoxosv <- (d_s$pko_years * d_s$osv_1000)
d_s$pkoxbrd <- (d_s$pko_years * d_s$brd_1000)

write_dta(d_s, "data/hale_data_standardized.dta", version = 13)
