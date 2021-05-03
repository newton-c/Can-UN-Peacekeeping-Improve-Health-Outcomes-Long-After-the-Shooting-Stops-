library(dplyr)
library(haven)
library(rethinking)

d <- read_dta("data/data21Dec2020_2.dta")

d$y00 <- ifelse(d$year == 2000, 1, 0)
d$y05 <- ifelse(d$year == 2005, 1, 0)
d$y10 <- ifelse(d$year == 2010, 1, 0)
d$y15 <- ifelse(d$year == 2015, 1, 0)
d$tropical <- ifelse(is.na(d$tropical), 1, d$tropical)
d$priorpko <- ifelse(is.na(d$priorpko), 1, d$priorpko)
d$osv_per1000 <- d$osv / 1000
d$deathstotal_and_osv_1000 <- d$deathstotal_and_osv_10000 / 10

d_s <- d %>%
    mutate_at(c('pkoyearsany', 'deathstotal_and_osv_1000', 'helog_knn',
        'hdi_knn', 'civilwarborder', 'urbangrowth_knn', 'gini_knn',
        'xpolity_knn', 'ef_knn'), ~(scale(.) %>%
    as.vector))

d_s$pkoxdeath <- d_s$pkoyearsany * d_s$deathstotal_and_osv_1000
d_s$pkoxosv <- (d_s$pkoyearsany * d_s$osv_per1000)
d_s$pkoxbrd <- (d_s$pkoyearsany * d_s$deathstotal_new)

# SD fron table 1 added---------------------------------------------------------
# Using DALY 15-44 averaged for fe/males
# PKO, additive.
T2_M1s <- map2stan(
    alist(
        dale ~ dnorm(mu, sigma),
        mu <- a + Bpko * pkoyearsany + Bdt * deathstotal_and_osv_1000 +
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
        Btrop ~ dnorm(-3.220, 58.099),
        Bpol ~ dnorm(-0.135, 3.757),
        Bef ~ dnorm(-0.922, 18.047),
        c(B05, B10, B15) ~ dnorm(0, 10),
        sigma ~ dcauchy(0, 2)

    ), data = d_s, iter = 2000, chains = 4, cores = 2
)


# PKO x total violence.
T2_M2s <- map2stan(
    alist(
        dale ~ dnorm(mu, sigma),
        mu <- a + Bpko * pkoyearsany + Bdt * deathstotal_and_osv_1000 +
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
        Bug ~ dnorm(-7.223, 41.008),
        Bgini ~ dnorm(-52.547, 363.294),
        Btrop ~ dnorm(-3.189, 58.275),
        Bpol ~ dnorm(-0.133, 3.771),
        Bef ~ dnorm(0.967, 18.066),
        c(B05, B10, B15) ~ dnorm(0, 10),
        sigma ~ dcauchy(0, 2)

    ), data = d_s, iter = 2000, chains = 4, cores = 2
)

# No outliers.
# PKO additive.
T2_M3s <- map2stan(
    alist(
        dale ~ dnorm(mu, sigma),
        mu <- a + Bpko * pkoyearsany + Bdt * deathstotal_and_osv_1000 +
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
        Btrop ~ dnorm(-3.220, 58.099),
        Bpol ~ dnorm(-0.135, 3.757),
        Bef ~ dnorm(-0.922, 18.047),
        c(B05, B10, B15) ~ dnorm(0, 10),
        sigma ~ dcauchy(0, 2)

    ), data = d_s, iter = 2000, chains = 4, cores = 2
)
# PKO x total violence.
T2_M4s <- map2stan(
    alist(
        dale ~ dnorm(mu, sigma),
        mu <- a + Bpko * pkoyearsany + Bdt * deathstotal_and_osv_1000 +
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
        Bug ~ dnorm(-7.223, 41.008),
        Bgini ~ dnorm(-52.547, 363.294),
        Btrop ~ dnorm(-3.189, 58.275),
        Bpol ~ dnorm(-0.133, 3.771),
        Bef ~ dnorm(0.967, 18.066),
        c(B05, B10, B15) ~ dnorm(0, 10),
        sigma ~ dcauchy(0, 2)

    ), data = d_s, iter = 2000, chains = 4, cores = 2
)

# Only OSV as measure of violence. --------------------------------------------
# PKO, additive.
T3_M1s <- map2stan(
    alist(
        dale ~ dnorm(mu, sigma),
        mu <- a + Bpko * pkoyearsany + Bdt * osv_per1000 +
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
        Btrop ~ dnorm(-3.220, 58.099),
        Bpol ~ dnorm(-0.135, 3.757),
        Bef ~ dnorm(-0.922, 18.047),
        c(B05, B10, B15) ~ dnorm(0, 10),
        sigma ~ dcauchy(0, 2)

    ), data = d_s, iter = 2000, chains = 4, cores = 2
)

# PKO x OSV
T3_M2s <- map2stan(
    alist(
        dale ~ dnorm(mu, sigma),
        mu <- a + Bpko * pkoyearsany + Bdt * osv_per1000 +
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
        Bug ~ dnorm(-7.223, 41.008),
        Bgini ~ dnorm(-52.547, 363.294),
        Btrop ~ dnorm(-3.189, 58.275),
        Bpol ~ dnorm(-0.133, 3.771),
        Bef ~ dnorm(0.967, 18.066),
        c(B05, B10, B15) ~ dnorm(0, 10),
        sigma ~ dcauchy(0, 2)

    ), data = d_s, iter = 2000, chains = 4, cores = 2
)

# PKO, no interation, no outliers
T3_M3s <- map2stan(
    alist(
        dale ~ dnorm(mu, sigma),
        mu <- a + Bpko * pkoyearsany + Bdt * osv_per1000 +
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
        Btrop ~ dnorm(-3.220, 58.099),
        Bpol ~ dnorm(-0.135, 3.757),
        Bef ~ dnorm(-0.922, 18.047),
        c(B05, B10, B15) ~ dnorm(0, 10),
        sigma ~ dcauchy(0, 2)

    ), data = d_s, iter = 2000, chains = 4, cores = 2
)

# PKO x OSV, no outliers
T3_M4s <- map2stan(
    alist(
        dale ~ dnorm(mu, sigma),
        mu <- a + Bpko * pkoyearsany + Bdt * osv_per1000 +
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
        Bug ~ dnorm(-7.223, 41.008),
        Bgini ~ dnorm(-52.547, 363.294),
        Btrop ~ dnorm(-3.189, 58.275),
        Bpol ~ dnorm(-0.133, 3.771),
        Bef ~ dnorm(0.967, 18.066),
        c(B05, B10, B15) ~ dnorm(0, 10),
        sigma ~ dcauchy(0, 2)
    ), data = d_s, iter = 2000, chains = 4, cores = 2
)


# Only BRDs as measure of violence. --------------------------------------------
# PKO, additive.
T4_M1s <- map2stan(
    alist(
        dale ~ dnorm(mu, sigma),
        mu <- a + Bpko * pkoyearsany + Bdt * deathstotal_new +
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
        Btrop ~ dnorm(-3.220, 58.099),
        Bpol ~ dnorm(-0.135, 3.757),
        Bef ~ dnorm(-0.922, 18.047),
        c(B05, B10, B15) ~ dnorm(0, 10),
        sigma ~ dcauchy(0, 2)

    ), data = d_s, iter = 2000, chains = 4, cores = 2
)

# PKO X BRDs.
T4_M2s <- map2stan(
    alist(
        dale ~ dnorm(mu, sigma),
        mu <- a + Bpko * pkoyearsany + Bdt * deathstotal_new +
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
        Bug ~ dnorm(-7.223, 41.008),
        Bgini ~ dnorm(-52.547, 363.294),
        Btrop ~ dnorm(-3.189, 58.275),
        Bpol ~ dnorm(-0.133, 3.771),
        Bef ~ dnorm(0.967, 18.066),
        c(B05, B10, B15) ~ dnorm(0, 10),
        sigma ~ dcauchy(0, 2)

    ), data = d_s, iter = 2000, chains = 4, cores = 2
)

# PKO, additive, no outliers.
T4_M3s <- map2stan(
    alist(
        dale ~ dnorm(mu, sigma),
        mu <- a + Bpko * pkoyearsany + Bdt * deathstotal_new +
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
        Btrop ~ dnorm(-3.220, 58.099),
        Bpol ~ dnorm(-0.135, 3.757),
        Bef ~ dnorm(-0.922, 18.047),
        c(B05, B10, B15) ~ dnorm(0, 10),
        sigma ~ dcauchy(0, 2)

    ), data = d_s, iter = 2000, chains = 4, cores = 2
)

# PKO x BRDs, no outliers.
T4_M4s <- map2stan(
    alist(
        dale ~ dnorm(mu, sigma),
        mu <- a + Bpko * pkoyearsany + Bdt * deathstotal_new +
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
        Bug ~ dnorm(-7.223, 41.008),
        Bgini ~ dnorm(-52.547, 363.294),
        Btrop ~ dnorm(-3.189, 58.275),
        Bpol ~ dnorm(-0.133, 3.771),
        Bef ~ dnorm(0.967, 18.066),
        c(B05, B10, B15) ~ dnorm(0, 10),
        sigma ~ dcauchy(0, 2)
    ), data = d_s, iter = 2000, chains = 4, cores = 2
)
