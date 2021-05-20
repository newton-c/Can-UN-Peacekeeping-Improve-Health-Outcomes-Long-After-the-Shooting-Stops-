library(dplyr)
library(haven)
library(rethinking)

d_s <- read_dta("data/hale_data_standardized.dta")

# SD fron table 1 added---------------------------------------------------------
# Using DALY 15-44 averaged for fe/males
# PKO, additive.
M1s <- map2stan(
    alist(
        hale ~ dnorm(mu, sigma),
        mu <- a + Bpko * pko_years + Bdt * total_violence_1000 +
            Bhe * helog_knn + Bhdi * hdi_knn + Bbcw * civilwarborder +
            Bug * urbangrowth_knn + Bgini * gini_knn + Btrop * tropical +
            Bpol * xpolity_knn + Bef * ef_knn + B05 * y05 + B10 * y10 +
            B15 * y15,
        a ~ dnorm(55, 10),
        Bpko ~ dnorm(0.356, 1.432),
        Bdt ~ dnorm(-0.443, 2.296),
        Bbcw ~ dnorm(-0.605, 2.577),
        Bhe ~ dnorm(0.358, 5.013),
        Bhdi ~ dnorm(0.363, 4.001),
        Bug ~ dnorm(-0.987, 5.593),
        Bgini ~ dnorm(-0.522, 3.653),
        Btrop ~ dnorm(-0.383, 6.903),
        Bpol ~ dnorm(-0.111, 3.081),
        Bef ~ dnorm(-0.125, 3.444),
        c(B05, B10, B15) ~ dnorm(0, 10),
        sigma ~ dcauchy(0, 2)

    ), data = d_s, iter = 2000, chains = 4, cores = 2
)

# PKO, additive.
#M1s <- map2stan(
#    alist(
#        dale ~ dnorm(mu, sigma),
#        mu <- a + Bpko * pko_years + Bdt * total_violence_1000 +
#            Bhe * helog_knn + Bhdi * hdi_knn + Bbcw * civilwarborder +
#            Bug * urbangrowth_knn + Bgini * gini_knn + Btrop * tropical +
#            Bpol * xpolity_knn + Bef * ef_knn + B05 * y05 + B10 * y10 +
#            B15 * y15,
#        a ~ dnorm(55, 10),
#        Bpko ~ dnorm(1.714, 6.886),
#        Bdt ~ dnorm(-0.363, 1.881),
#        Bbcw ~ dnorm(-10.196, 43.414),
#        Bhe ~ dnorm(2.109, 29.542),
#        Bhdi ~ dnorm(5.520, 60.861),
#        Bug ~ dnorm(-7.211, 40.847),
#        Bgini ~ dnorm(-51.364, 359.454),
#        Btrop ~ dnorm(-3.220, 58.099),
#        Bpol ~ dnorm(-0.135, 3.757),
#        Bef ~ dnorm(-0.922, 18.047),
#        c(B05, B10, B15) ~ dnorm(0, 10),
#        sigma ~ dcauchy(0, 2)
#
#    ), data = d_s, iter = 2000, chains = 4, cores = 2
#)
#

# PKO x total violence.
m2_s <- map2stan(
    alist(
        hale ~ dnorm(mu, sigma),
        mu <- a + Bpko * pko_years + Bdt * total_violence_1000 +
            Bint * pkoxdeath + Bhe * helog_knn + Bhdi * hdi_knn +
            Bbcw * civilwarborder + Bug * urbangrowth_knn +
            Bgini * gini_knn + Btrop * tropical + Bpol * xpolity_knn +
            Bef * ef_knn + B05 * y05 + B10 * y10 + B15 * y15,
        a ~ dnorm(55, 10),
        Bpko ~ dnorm(0.346, 1.425),
        Bdt ~ dnorm(-0.642, 4.894),
        Bint ~ dnorm(0.111, 2.378),
        Bbcw ~ dnorm(-0.603, 2.588),
        Bhe ~ dnorm(0.345, 5.083),
        Bhdi ~ dnorm(0.357, 4.021),
        Bug ~ dnorm(-0.990, 5.615),
        Bgini ~ dnorm(-0.534, 3.692),
        Btrop ~ dnorm(-0.379, 6.924),
        Bpol ~ dnorm(-0.109, 3.092),
        Bef ~ dnorm(-0.131, 2.446),
        c(B05, B10, B15) ~ dnorm(0, 10),
        sigma ~ dcauchy(0, 2)

    ), data = d_s, iter = 2000, chains = 4, cores = 2
)
#precis(M2s)

# PKO x total violence.
m2_uninform_priors <- map2stan(
    alist(
        hale ~ dnorm(mu, sigma),
        mu <- a + Bpko * pko_years + Bdt * total_violence_1000 +
            Bint * pkoxdeath + Bhe * helog_knn + Bhdi * hdi_knn +
            Bbcw * civilwarborder + Bug * urbangrowth_knn +
            Bgini * gini_knn + Btrop * tropical + Bpol * xpolity_knn +
            Bef * ef_knn + B05 * y05 + B10 * y10 + B15 * y15,
        a ~ dnorm(55, 10),
        Bpko ~  dnorm(0, 10),
        Bdt ~  dnorm(0, 10),
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

    ), data = d_s, iter = 2000, chains = 4, cores = 2
)

precis(m2_s)
precis(m2_uninform_priors)
compare(m2_s, m2_uninform_priors)



d_sml <- transform(d_s, id=as.numeric(factor(ccode)))
M2ml <- map2stan(
    alist(
        hale ~ dnorm(mu, sigma),
        mu <- a + a_country[id] +Bpko * pko_years + Bdt * total_violence_1000 +
            Bint * pkoxdeath + Bhe * helog_knn + Bhdi * hdi_knn +
            Bbcw * civilwarborder + Bug * urbangrowth_knn +
            Bgini * gini_knn + Btrop * tropical + Bpol * xpolity_knn +
            Bef * ef_knn + B05 * y05 + B10 * y10 + B15 * y15,
        a ~ dnorm(55, 10),
        Bpko ~ dnorm(0.346, 1.425),
        Bdt ~ dnorm(-0.642, 4.894),
        Bint ~ dnorm(0.111, 2.378),
        Bbcw ~ dnorm(-0.603, 2.588),
        Bhe ~ dnorm(0.345, 5.083),
        Bhdi ~ dnorm(0.357, 4.021),
        Bug ~ dnorm(-0.990, 5.615),
        Bgini ~ dnorm(-0.534, 3.692),
        Btrop ~ dnorm(-0.379, 6.924),
        Bpol ~ dnorm(-0.109, 3.092),
        Bef ~ dnorm(-0.131, 2.446),
        c(B05, B10, B15) ~ dnorm(0, 10),
        a_country[id] ~ dnorm(55, sigma_country),
        sigma_country ~ dcauchy(0, 2),
        sigma ~ dcauchy(0, 2)

    ), data = d_sml, iter = 2000, chains = 4, cores = 2
)


precis(M2ml)

# PKO x total violence.
#T2_M2s <- map2stan(
#    alist(
#        dale ~ dnorm(mu, sigma),
#        mu <- a + Bpko * pko_years + Bdt * total_violence_1000 +
#            Bint * pkoxdeath + Bhe * helog_knn + Bhdi * hdi_knn +
#            Bbcw * civilwarborder + Bug * urbangrowth_knn +
#            Bgini * gini_knn + Btrop * tropical + Bpol * xpolity_knn +
#            Bef * ef_knn + B05 * y05 + B10 * y10 + B15 * y15,
#        a ~ dnorm(55, 10),
#        Bpko ~ dnorm(1.551, 7.509),
#        Bdt ~ dnorm(-0.558, 4.630),
#        Bint ~ dnorm(0.052, 1.113),
#        Bbcw ~ dnorm(-10.161, 43.603),
#        Bhe ~ dnorm(2.034, 29.953),
#        Bhdi ~ dnorm(5.427, 61.087),
#        Bug ~ dnorm(-7.223, 41.008),
#        Bgini ~ dnorm(-52.547, 363.294),
#        Btrop ~ dnorm(-3.189, 58.275),
#        Bpol ~ dnorm(-0.133, 3.771),
#        Bef ~ dnorm(0.967, 18.066),
#        c(B05, B10, B15) ~ dnorm(0, 10),
#        sigma ~ dcauchy(0, 2)
#
#    ), data = d_s, iter = 2000, chains = 4, cores = 2
#)
#
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
