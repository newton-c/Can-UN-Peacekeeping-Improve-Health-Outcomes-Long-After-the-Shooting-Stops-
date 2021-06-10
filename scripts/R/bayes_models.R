library(dplyr)
library(haven)
library(modelsummary)
library(rethinking)

set.seed(2753)

d <- read_dta("data/hale_data.dta")

# Regularized priors ----------------------------------------------------------
# PKO, no interaction.
total_violence_add <- map2stan(
    alist(
        hale ~ dnorm(mu, sigma),
        mu <- a + Bpko * pko_years + Bdt * total_violence_1000 +
            Bhe * helog_knn + Bhdi * hdi_knn +
            Bbcw * civilwarborder + Bug * urbangrowth_knn +
            Bgini * gini_knn + Btrop * tropical + Bpol * xpolity_knn +
            Bef * ef_knn + B05 * y05 + B10 * y10 + B15 * y15,
        a ~ dnorm(55, 10),
        Bpko ~ dnorm(0, 10),
        Bdt ~ dnorm(0, 10),
        Bbcw ~ dnorm(0, 10),
        Bhe ~ dnorm(0, 10),
        Bhdi ~ dnorm(0, 10),
        Bug ~ dnorm(0, 10),
        Bgini ~ dnorm(0, 10),
        Btrop ~ dnorm(0, 10),
        Bpol ~ dnorm(0, 10),
        Bef ~ dnorm(0, 10),
        c(B05, B10, B15) ~ dnorm(0, 10),
        sigma ~ dcauchy(0, 2)

    ), data = d, iter = 2000, chains = 4, cores = 2
)

# PKO x total violence.
total_violence_int <- map2stan(
    alist(
        hale ~ dnorm(mu, sigma),
        mu <- a + Bpko * pko_years + Bdt * total_violence_1000 +
            Bint * pkoxdeath + Bhe * helog_knn + Bhdi * hdi_knn +
            Bbcw * civilwarborder + Bug * urbangrowth_knn +
            Bgini * gini_knn + Btrop * tropical + Bpol * xpolity_knn +
            Bef * ef_knn + B05 * y05 + B10 * y10 + B15 * y15,
        a ~ dnorm(55, 10),
        Bpko ~ dnorm(0, 10),
        Bdt ~ dnorm(0, 10),
        Bint ~ dnorm(0, 10),
        Bbcw ~ dnorm(0, 10),
        Bhe ~ dnorm(0, 10),
        Bhdi ~ dnorm(0, 10),
        Bug ~ dnorm(0, 10),
        Bgini ~ dnorm(0, 10),
        Btrop ~ dnorm(0, 10),
        Bpol ~ dnorm(0, 10),
        Bef ~ dnorm(0, 10),
        c(B05, B10, B15) ~ dnorm(0, 10),
        sigma ~ dcauchy(0, 2)

    ), data = d, iter = 2000, chains = 4, cores = 2
)

# Only OSV as measure of violence. --------------------------------------------
# PKO, no interaction.
osv_add <- map2stan(
    alist(
        hale ~ dnorm(mu, sigma),
        mu <- a + Bpko * pko_years + Bdt * osv_1000 +
            Bhe * helog_knn + Bhdi * hdi_knn +
            Bbcw * civilwarborder + Bug * urbangrowth_knn +
            Bgini * gini_knn + Btrop * tropical + Bpol * xpolity_knn +
            Bef * ef_knn + B05 * y05 + B10 * y10 + B15 * y15,
        a ~ dnorm(55, 10),
        Bpko ~ dnorm(0, 10),
        Bdt ~ dnorm(0, 10),
        Bbcw ~ dnorm(0, 10),
        Bhe ~ dnorm(0, 10),
        Bhdi ~ dnorm(0, 10),
        Bug ~ dnorm(0, 10),
        Bgini ~ dnorm(0, 10),
        Btrop ~ dnorm(0, 10),
        Bpol ~ dnorm(0, 10),
        Bef ~ dnorm(0, 10),
        c(B05, B10, B15) ~ dnorm(0, 10),
        sigma ~ dcauchy(0, 2)

    ), data = d, iter = 2000, chains = 4, cores = 2
)

# PKO x OSV
osv_int <- map2stan(
    alist(
        hale ~ dnorm(mu, sigma),
        mu <- a + Bpko * pko_years + Bdt * osv_1000 +
            Bint * pkoxosv + Bhe * helog_knn + Bhdi * hdi_knn +
            Bbcw * civilwarborder + Bug * urbangrowth_knn +
            Bgini * gini_knn + Btrop * tropical + Bpol * xpolity_knn +
            Bef * ef_knn + B05 * y05 + B10 * y10 + B15 * y15,
        a ~ dnorm(55, 10),
        Bpko ~ dnorm(0, 10),
        Bdt ~ dnorm(0, 10),
        Bint ~ dnorm(0, 10),
        Bbcw ~ dnorm(0, 10),
        Bhe ~ dnorm(0, 10),
        Bhdi ~ dnorm(0, 10),
        Bug ~ dnorm(0, 10),
        Bgini ~ dnorm(0, 10),
        Btrop ~ dnorm(0, 10),
        Bpol ~ dnorm(0, 10),
        Bef ~ dnorm(0, 10),
        c(B05, B10, B15) ~ dnorm(0, 10),
        sigma ~ dcauchy(0, 2)

    ), data = d, iter = 2000, chains = 4, cores = 2
)

# Only BRDs as measure of violence. --------------------------------------------
# PKO, no interaction.
brd_add <- map2stan(
    alist(
        hale ~ dnorm(mu, sigma),
        mu <- a + Bpko * pko_years + Bdt * brd_1000 +
            Bhe * helog_knn + Bhdi * hdi_knn +
            Bbcw * civilwarborder + Bug * urbangrowth_knn +
            Bgini * gini_knn + Btrop * tropical + Bpol * xpolity_knn +
            Bef * ef_knn + B05 * y05 + B10 * y10 + B15 * y15,
        a ~ dnorm(55, 10),
        Bpko ~ dnorm(0, 10),
        Bdt ~ dnorm(0, 10),
        Bbcw ~ dnorm(0, 10),
        Bhe ~ dnorm(0, 10),
        Bhdi ~ dnorm(0, 10),
        Bug ~ dnorm(0, 10),
        Bgini ~ dnorm(0, 10),
        Btrop ~ dnorm(0, 10),
        Bpol ~ dnorm(0, 10),
        Bef ~ dnorm(0, 10),
        c(B05, B10, B15) ~ dnorm(0, 10),
        sigma ~ dcauchy(0, 2)

    ), data = d, iter = 2000, chains = 4, cores = 2
)

# PKO X BRDs.
brd_int <- map2stan(
    alist(
        hale ~ dnorm(mu, sigma),
        mu <- a + Bpko * pko_years + Bdt * brd_1000 +
            Bint * pkoxbrd + Bhe * helog_knn + Bhdi * hdi_knn +
            Bbcw * civilwarborder + Bug * urbangrowth_knn +
            Bgini * gini_knn + Btrop * tropical + Bpol * xpolity_knn +
            Bef * ef_knn + B05 * y05 + B10 * y10 + B15 * y15,
        a ~ dnorm(55, 10),
        Bpko ~ dnorm(0, 10),
        Bdt ~ dnorm(0, 10),
        Bint ~ dnorm(0, 10),
        Bbcw ~ dnorm(0, 10),
        Bhe ~ dnorm(0, 10),
        Bhdi ~ dnorm(0, 10),
        Bug ~ dnorm(0, 10),
        Bgini ~ dnorm(0, 10),
        Btrop ~ dnorm(0, 10),
        Bpol ~ dnorm(0, 10),
        Bef ~ dnorm(0, 10),
        c(B05, B10, B15) ~ dnorm(0, 10),
        sigma ~ dcauchy(0, 2)

    ), data = d, iter = 2000, chains = 4, cores = 2
)

# Informedative priors ---------------------------------------------------------
# Using DALY 15-44 averaged for fe/males.
# Untransformed variables.
# PKO, no interaction.
total_violence_add_inform <- map2stan(
    alist(
        hale ~ dnorm(mu, sigma),
        mu <- a + Bpko * pko_years + Bdt * total_violence_1000 +
            Bhe * helog_knn + Bhdi * hdi_knn +
            Bbcw * civilwarborder + Bug * urbangrowth_knn +
            Bgini * gini_knn + Btrop * tropical + Bpol * xpolity_knn +
            Bef * ef_knn + B05 * y05 + B10 * y10 + B15 * y15,
        a ~ dnorm(55, 10),
        Bpko ~ dnorm(1.714, 6.886),
        Bdt ~ dnorm(-0.363, 1.881),
        Bbcw ~ dnorm(-10.196, 43.414),
        Bhe ~ dnorm(2.109, 29.542),
        Bhdi ~ dnorm(5.520, 60.861),
        Bug ~ dnorm(-7.211, 40.847),
        Bgini ~ dnorm(-51.364, 359.454),
        Btrop ~ dnorm(-3.22, 58.099),
        Bpol ~ dnorm(-0.135, 3.757),
        Bef ~ dnorm(-0.922, 18.047),
        c(B05, B10, B15) ~ dnorm(0, 10),
        sigma ~ dcauchy(0, 2)

    ), data = d, iter = 2000, chains = 4, cores = 2
)

# PKO x total violence.
start_time <- Sys.time()
total_violence_int_inform <- map2stan(
    alist(
        hale ~ dnorm(mu, sigma),
        mu <- a + Bpko * pko_years + Bdt * total_violence_1000 +
            Bint * pkoxdeath + Bhe * helog_knn + Bhdi * hdi_knn +
            Bbcw * civilwarborder + Bug * urbangrowth_knn +
            Bgini * gini_knn + Btrop * tropical + Bpol * xpolity_knn +
            Bef * ef_knn + B05 * y05 + B10 * y10 + B15 * y15,
        a ~ dnorm(55, 10),
        Bpko ~ dnorm(1.551, 7.509),
        Bdt ~ dnorm(-0.558, 4.630),
        Bint ~ dnorm(0.052, 1.113),
        Bbcw ~ dnorm(-10.161, 43.603),
        Bhe ~ dnorm(2.034, 29.953),
        Bhdi ~ dnorm(5.427, 61.087),
        Bug ~ dnorm(-7.229, 41.008),
        Bgini ~ dnorm(-52.547, 363.284),
        Btrop ~ dnorm(-3.189, 58.275),
        Bpol ~ dnorm(-0.133, 3.771),
        Bef ~ dnorm(-0.967, 18.066),
        c(B05, B10, B15) ~ dnorm(0, 10),
        sigma ~ dcauchy(0, 2)

    ), data = d, iter = 2000, chains = 4, cores = 2
)
end_time <- Sys.time()
print(end_time - start_time)
# OSV as measure of violence --------------------------------------------------
# PKO no interaction.
osv_add_inform <- map2stan(
    alist(
        hale ~ dnorm(mu, sigma),
        mu <- a + Bpko * pko_years + Bdt * osv_1000 +
            Bhe * helog_knn + Bhdi * hdi_knn +
            Bbcw * civilwarborder + Bug * urbangrowth_knn +
            Bgini * gini_knn + Btrop * tropical + Bpol * xpolity_knn +
            Bef * ef_knn + B05 * y05 + B10 * y10 + B15 * y15,
        a ~ dnorm(55, 10),
        Bpko ~ dnorm(1.714, 6.886),
        Bdt ~ dnorm(-0.363, 1.881),
        Bbcw ~ dnorm(-10.196, 43.414),
        Bhe ~ dnorm(2.109, 29.542),
        Bhdi ~ dnorm(5.520, 60.861),
        Bug ~ dnorm(-7.211, 40.847),
        Bgini ~ dnorm(-51.364, 359.454),
        Btrop ~ dnorm(-3.22, 58.099),
        Bpol ~ dnorm(-0.135, 3.757),
        Bef ~ dnorm(-0.922, 18.047),
        c(B05, B10, B15) ~ dnorm(0, 10),
        sigma ~ dcauchy(0, 2)

    ), data = d, iter = 2000, chains = 4, cores = 2
)

# PKO x OSV 
osv_int_inform <- map2stan(
    alist(
        hale ~ dnorm(mu, sigma),
        mu <- a + Bpko * pko_years + Bdt * osv_1000 +
            Bint * pkoxosv + Bhe * helog_knn + Bhdi * hdi_knn +
            Bbcw * civilwarborder + Bug * urbangrowth_knn +
            Bgini * gini_knn + Btrop * tropical + Bpol * xpolity_knn +
            Bef * ef_knn + B05 * y05 + B10 * y10 + B15 * y15,
        a ~ dnorm(55, 10),
        Bpko ~ dnorm(1.551, 7.509),
        Bdt ~ dnorm(-0.558, 4.630),
        Bint ~ dnorm(0.052, 1.113),
        Bbcw ~ dnorm(-10.161, 43.603),
        Bhe ~ dnorm(2.034, 29.953),
        Bhdi ~ dnorm(5.427, 61.087),
        Bug ~ dnorm(-7.229, 41.008),
        Bgini ~ dnorm(-52.547, 363.284),
        Btrop ~ dnorm(-3.189, 58.275),
        Bpol ~ dnorm(-0.133, 3.771),
        Bef ~ dnorm(-0.967, 18.066),
        c(B05, B10, B15) ~ dnorm(0, 10),
        sigma ~ dcauchy(0, 2)

    ), data = d, iter = 2000, chains = 4, cores = 2
)


# Only BRD as measure of violence. --------------------------------------------
# PKO, no interaction.
brd_add_inform <- map2stan(
    alist(
        hale ~ dnorm(mu, sigma),
        mu <- a + Bpko * pko_years + Bdt * brd_1000 +
            Bhe * helog_knn + Bhdi * hdi_knn +
            Bbcw * civilwarborder + Bug * urbangrowth_knn +
            Bgini * gini_knn + Btrop * tropical + Bpol * xpolity_knn +
            Bef * ef_knn + B05 * y05 + B10 * y10 + B15 * y15,
        a ~ dnorm(55, 10),
        Bpko ~ dnorm(1.714, 6.886),
        Bdt ~ dnorm(-0.363, 1.881),
        Bbcw ~ dnorm(-10.196, 43.414),
        Bhe ~ dnorm(2.109, 29.542),
        Bhdi ~ dnorm(5.520, 60.861),
        Bug ~ dnorm(-7.211, 40.847),
        Bgini ~ dnorm(-51.364, 359.454),
        Btrop ~ dnorm(-3.22, 58.099),
        Bpol ~ dnorm(-0.135, 3.757),
        Bef ~ dnorm(-0.922, 18.047),
        c(B05, B10, B15) ~ dnorm(0, 10),
        sigma ~ dcauchy(0, 2)

    ), data = d, iter = 2000, chains = 4, cores = 2
)

# PKO x BRD
brd_int_inform <- map2stan(
    alist(
        hale ~ dnorm(mu, sigma),
        mu <- a + Bpko * pko_years + Bdt * brd_1000 +
            Bint * pkoxbrd + Bhe * helog_knn + Bhdi * hdi_knn +
            Bbcw * civilwarborder + Bug * urbangrowth_knn +
            Bgini * gini_knn + Btrop * tropical + Bpol * xpolity_knn +
            Bef * ef_knn + B05 * y05 + B10 * y10 + B15 * y15,
        a ~ dnorm(55, 10),
        Bpko ~ dnorm(1.551, 7.509),
        Bdt ~ dnorm(-0.558, 4.630),
        Bint ~ dnorm(0.052, 1.113),
        Bbcw ~ dnorm(-10.161, 43.603),
        Bhe ~ dnorm(2.034, 29.953),
        Bhdi ~ dnorm(5.427, 61.087),
        Bug ~ dnorm(-7.229, 41.008),
        Bgini ~ dnorm(-52.547, 363.284),
        Btrop ~ dnorm(-3.189, 58.275),
        Bpol ~ dnorm(-0.133, 3.771),
        Bef ~ dnorm(-0.967, 18.066),
        c(B05, B10, B15) ~ dnorm(0, 10),
        sigma ~ dcauchy(0, 2)

    ), data = d, iter = 2000, chains = 4, cores = 2
)

