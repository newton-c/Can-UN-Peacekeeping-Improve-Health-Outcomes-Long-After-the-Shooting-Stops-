library(dplyr)
library(haven)
library(rethinking)

d_s <- read_dta("data/hale_data_standardized.dta")


# Informative priors ----------------------------------------------------------
# Using DALY 15-44 averaged for fe/males.
# Untransformed variables.
# PKO, no interaction.
total_violence_add_inform_sd <- map2stan(
    alist(
        hale ~ dnorm(mu, sigma),
        mu <- a + Bpko * pko_years + Bdt * total_violence_1000 +
            Bhe * helog_knn + Bhdi * hdi_knn +
            Bbcw * civilwarborder + Bug * urbangrowth_knn +
            Bgini * gini_knn + Btrop * tropical + Bpol * xpolity_knn +
            Bef * ef_knn + B05 * y05 + B10 * y10 + B15 * y15,
        a ~ dnorm(55, 10),
        Bpko ~ dnorm(0.36, 1.43),
        Bdt ~ dnorm(-0.44, 2.30),
        Bbcw ~ dnorm(-0.61, 2.58),
        Bhe ~ dnorm(0.36, 5.01),
        Bhdi ~ dnorm(0.36, 4.01),
        Bug ~ dnorm(-0.99, 5.59),
        Bgini ~ dnorm(-0.52, 3.65),
        Btrop ~ dnorm(-0.38, 6.90),
        Bpol ~ dnorm(-0.11, 3.08),
        Bef ~ dnorm(-0.12, 2.44),
        c(B05, B10, B15) ~ dnorm(0, 10),
        sigma ~ dcauchy(0, 2)

    ), data = d_s, iter = 2000, chains = 4, cores = 2
)

# PKO x total violence.
total_violence_int_inform_sd <- map2stan(
    alist(
        hale ~ dnorm(mu, sigma),
        mu <- a + Bpko * pko_years + Bdt * total_violence_1000 +
            Bint * pkoxdeath + Bhe * helog_knn + Bhdi * hdi_knn +
            Bbcw * civilwarborder + Bug * urbangrowth_knn +
            Bgini * gini_knn + Btrop * tropical + Bpol * xpolity_knn +
            Bef * ef_knn + B05 * y05 + B10 * y10 + B15 * y15,
        a ~ dnorm(55, 10),
        Bpko ~ dnorm(0.35, 1.42),
        Bdt ~ dnorm(-0.64, 4.89),
        Bint ~ dnorm(0.11, 2.38),
        Bbcw ~ dnorm(-0.60, 2.59),
        Bhe ~ dnorm(0.35, 5.08),
        Bhdi ~ dnorm(0.36, 4.02),
        Bug ~ dnorm(-0.99, 5.61),
        Bgini ~ dnorm(-0.53, 3.69),
        Btrop ~ dnorm(-0.38, 6.92),
        Bpol ~ dnorm(-0.11, 3.09),
        Bef ~ dnorm(-0.13, 2.45),
        c(B05, B10, B15) ~ dnorm(0, 10),
        sigma ~ dcauchy(0, 2)

    ), data = d_s, iter = 2000, chains = 4, cores = 2
)

# OSV as measure of violence --------------------------------------------------
# PKO no interaction.
osv_add_inform_sd <- map2stan(
    alist(
        hale ~ dnorm(mu, sigma),
        mu <- a + Bpko * pko_years + Bdt * osv_1000 +
            Bhe * helog_knn + Bhdi * hdi_knn +
            Bbcw * civilwarborder + Bug * urbangrowth_knn +
            Bgini * gini_knn + Btrop * tropical + Bpol * xpolity_knn +
            Bef * ef_knn + B05 * y05 + B10 * y10 + B15 * y15,
        a ~ dnorm(55, 10),
        Bpko ~ dnorm(0.36, 1.43),
        Bdt ~ dnorm(-0.44, 2.30),
        Bbcw ~ dnorm(-0.61, 2.58),
        Bhe ~ dnorm(0.36, 5.01),
        Bhdi ~ dnorm(0.36, 4.01),
        Bug ~ dnorm(-0.99, 5.59),
        Bgini ~ dnorm(-0.52, 3.65),
        Btrop ~ dnorm(-0.38, 6.90),
        Bpol ~ dnorm(-0.11, 3.08),
        Bef ~ dnorm(-0.12, 2.44),
        c(B05, B10, B15) ~ dnorm(0, 10),
        sigma ~ dcauchy(0, 2)

    ), data = d_s, iter = 2000, chains = 4, cores = 2
)

# PKO x OSV 
osv_int_inform_sd <- map2stan(
    alist(
        hale ~ dnorm(mu, sigma),
        mu <- a + Bpko * pko_years + Bdt * osv_1000 +
            Bint * pkoxosv + Bhe * helog_knn + Bhdi * hdi_knn +
            Bbcw * civilwarborder + Bug * urbangrowth_knn +
            Bgini * gini_knn + Btrop * tropical + Bpol * xpolity_knn +
            Bef * ef_knn + B05 * y05 + B10 * y10 + B15 * y15,
        a ~ dnorm(55, 10),
        Bpko ~ dnorm(0.35, 1.42),
        Bdt ~ dnorm(-0.64, 4.89),
        Bint ~ dnorm(0.11, 2.38),
        Bbcw ~ dnorm(-0.60, 2.59),
        Bhe ~ dnorm(0.35, 5.08),
        Bhdi ~ dnorm(0.36, 4.02),
        Bug ~ dnorm(-0.99, 5.61),
        Bgini ~ dnorm(-0.53, 3.69),
        Btrop ~ dnorm(-0.38, 6.92),
        Bpol ~ dnorm(-0.11, 3.09),
        Bef ~ dnorm(-0.13, 2.45),
        c(B05, B10, B15) ~ dnorm(0, 10),
        sigma ~ dcauchy(0, 2)

    ), data = d_s, iter = 2000, chains = 4, cores = 2
)


# Only BRD as measure of violence. --------------------------------------------
# PKO, no interaction.
brd_add_inform_sd <- map2stan(
    alist(
        hale ~ dnorm(mu, sigma),
        mu <- a + Bpko * pko_years + Bdt * brd_1000 +
            Bhe * helog_knn + Bhdi * hdi_knn +
            Bbcw * civilwarborder + Bug * urbangrowth_knn +
            Bgini * gini_knn + Btrop * tropical + Bpol * xpolity_knn +
            Bef * ef_knn + B05 * y05 + B10 * y10 + B15 * y15,
        a ~ dnorm(55, 10),
        Bpko ~ dnorm(0.36, 1.43),
        Bdt ~ dnorm(-0.44, 2.30),
        Bbcw ~ dnorm(-0.61, 2.58),
        Bhe ~ dnorm(0.36, 5.01),
        Bhdi ~ dnorm(0.36, 4.01),
        Bug ~ dnorm(-0.99, 5.59),
        Bgini ~ dnorm(-0.52, 3.65),
        Btrop ~ dnorm(-0.38, 6.90),
        Bpol ~ dnorm(-0.11, 3.08),
        Bef ~ dnorm(-0.12, 2.44),
        c(B05, B10, B15) ~ dnorm(0, 10),
        sigma ~ dcauchy(0, 2)

    ), data = d_s, iter = 2000, chains = 4, cores = 2
)

# PKO x BRD
brd_int_inform_sd <- map2stan(
    alist(
        hale ~ dnorm(mu, sigma),
        mu <- a + Bpko * pko_years + Bdt * brd_1000 +
            Bint * pkoxbrd + Bhe * helog_knn + Bhdi * hdi_knn +
            Bbcw * civilwarborder + Bug * urbangrowth_knn +
            Bgini * gini_knn + Btrop * tropical + Bpol * xpolity_knn +
            Bef * ef_knn + B05 * y05 + B10 * y10 + B15 * y15,
        a ~ dnorm(55, 10),
        Bpko ~ dnorm(0.35, 1.42),
        Bdt ~ dnorm(-0.64, 4.89),
        Bint ~ dnorm(0.11, 2.38),
        Bbcw ~ dnorm(-0.60, 2.59),
        Bhe ~ dnorm(0.35, 5.08),
        Bhdi ~ dnorm(0.36, 4.02),
        Bug ~ dnorm(-0.99, 5.61),
        Bgini ~ dnorm(-0.53, 3.69),
        Btrop ~ dnorm(-0.38, 6.92),
        Bpol ~ dnorm(-0.11, 3.09),
        Bef ~ dnorm(-0.13, 2.45),
        c(B05, B10, B15) ~ dnorm(0, 10),
        sigma ~ dcauchy(0, 2)

    ), data = d_s, iter = 2000, chains = 4, cores = 2
)
