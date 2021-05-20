library(dplyr)
library(haven)
library(modelsummary)
library(rethinking)

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
ovs_int <- map2stan(
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

# Informed priors. ------------------------------------------------------------
# Using DALY 15-44 averaged for fe/males.
# Untransformed variables.
# PKO, no interaction.
T2_M1 <- map2stan(
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
        Btrop ~ dnorm(-3.22, 58.099),
        Bpol ~ dnorm(-0.135, 3.757),
        Bef ~ dnorm(-0.922, 18.047),
        c(B05, B10, B15) ~ dnorm(0, 10),
        sigma ~ dcauchy(0, 2)

    ), data = d, iter = 2000, chains = 4, cores = 2
)

# PKO x total violence.
T2_M2 <- map2stan(
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
        Bug ~ dnorm(-7.229, 41.008),
        Bgini ~ dnorm(-52.547, 363.284),
        Btrop ~ dnorm(-3.189, 58.275),
        Bpol ~ dnorm(-0.133, 3.771),
        Bef ~ dnorm(-0.967, 18.066),
        c(B05, B10, B15) ~ dnorm(0, 10),
        sigma ~ dcauchy(0, 2)

    ), data = d, iter = 2000, chains = 4, cores = 2
)


# PKO no interaction.
T2_M3 <- map2stan(
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
        Btrop ~ dnorm(-3.22, 58.099),
        Bpol ~ dnorm(-0.135, 3.757),
        Bef ~ dnorm(-0.922, 18.047),
        c(B05, B10, B15) ~ dnorm(0, 10),
        sigma ~ dcauchy(0, 2)

    ), data = d_no, iter = 2000, chains = 4, cores = 2
)
# PKO x total violence.
T2_M4 <- map2stan(
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
        Bug ~ dnorm(-7.229, 41.008),
        Bgini ~ dnorm(-52.547, 363.284),
        Btrop ~ dnorm(-3.189, 58.275),
        Bpol ~ dnorm(-0.133, 3.771),
        Bef ~ dnorm(-0.967, 18.066),
        c(B05, B10, B15) ~ dnorm(0, 10),
        sigma ~ dcauchy(0, 2)

    ), data = d_no, iter = 2000, chains = 4, cores = 2
)


# Only OSV as measure of violence. --------------------------------------------
# PKO, no interaction.
T3_M1 <- map2stan(
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
        Btrop ~ dnorm(-3.22, 58.099),
        Bpol ~ dnorm(-0.135, 3.757),
        Bef ~ dnorm(-0.922, 18.047),
        c(B05, B10, B15) ~ dnorm(0, 10),
        sigma ~ dcauchy(0, 2)

    ), data = d, iter = 2000, chains = 4, cores = 2
)

# PKO x OSV
T3_M2 <- map2stan(
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
        Bug ~ dnorm(-7.229, 41.008),
        Bgini ~ dnorm(-52.547, 363.284),
        Btrop ~ dnorm(-3.189, 58.275),
        Bpol ~ dnorm(-0.133, 3.771),
        Bef ~ dnorm(-0.967, 18.066),
        c(B05, B10, B15) ~ dnorm(0, 10),
        sigma ~ dcauchy(0, 2)

    ), data = d, iter = 2000, chains = 4, cores = 2
)

# PKO, no interation, no outliers
T3_M3 <- map2stan(
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
        Btrop ~ dnorm(-3.22, 58.099),
        Bpol ~ dnorm(-0.135, 3.757),
        Bef ~ dnorm(-0.922, 18.047),
        c(B05, B10, B15) ~ dnorm(0, 10),
        sigma ~ dcauchy(0, 2)

    ), data = d_no, iter = 2000, chains = 4, cores = 2
)

# PKO x OSV, no outliers
T3_M4 <- map2stan(
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
        Bug ~ dnorm(-7.229, 41.008),
        Bgini ~ dnorm(-52.547, 363.284),
        Btrop ~ dnorm(-3.189, 58.275),
        Bpol ~ dnorm(-0.133, 3.771),
        Bef ~ dnorm(-0.967, 18.066),
        c(B05, B10, B15) ~ dnorm(0, 10),
        sigma ~ dcauchy(0, 2)
    ), data = d_no, iter = 2000, chains = 4, cores = 2
)



# Only BRDs as measure of violence. --------------------------------------------
# PKO, no interaction.
T4_M1 <- map2stan(
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
        Btrop ~ dnorm(-3.22, 58.099),
        Bpol ~ dnorm(-0.135, 3.757),
        Bef ~ dnorm(-0.922, 18.047),
        c(B05, B10, B15) ~ dnorm(0, 10),
        sigma ~ dcauchy(0, 2)

    ), data = d, iter = 2000, chains = 4, cores = 2
)

# PKO X BRDs.
T4_M2 <- map2stan(
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
        Bug ~ dnorm(-7.229, 41.008),
        Bgini ~ dnorm(-52.547, 363.284),
        Btrop ~ dnorm(-3.189, 58.275),
        Bpol ~ dnorm(-0.133, 3.771),
        Bef ~ dnorm(-0.967, 18.066),
        c(B05, B10, B15) ~ dnorm(0, 10),
        sigma ~ dcauchy(0, 2)

    ), data = d, iter = 2000, chains = 4, cores = 2
)

# PKO, no interaction, no outliers.
T4_M3 <- map2stan(
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
        Btrop ~ dnorm(-3.22, 58.099),
        Bpol ~ dnorm(-0.135, 3.757),
        Bef ~ dnorm(-0.922, 18.047),
        c(B05, B10, B15) ~ dnorm(0, 10),
        sigma ~ dcauchy(0, 2)

    ), data = d_no, iter = 2000, chains = 4, cores = 2
)

# PKO x BRDs, no outliers.
T4_M4 <- map2stan(
    alist(
        dale ~ dnorm(mu, sigma),
        mu <- a + Bpko * pkoyearsany + Bdt * deathstotal_new +
            Bint * pkoxbrd + Bhe * helog_knn + Bhdi * hdi_knn +
            Bbcw * civilwarborder + Bug * urbangrowth_knn +
            Bgini * gini_knn + Btrop * tropical + Bpol * xpolity_knn +
            Bef * ef_knn + B05 * y05 + B10 * y10 + B15 * y15,        a ~ dnorm(55, 10),
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
    ), data = d_no, iter = 2000, chains = 4, cores = 2
)


# Comparing informed and regularized priors -----------------------------------
# PKO x total violence.
t2_m2_ip <- map2stan(
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
        Bug ~ dnorm(-7.229, 41.008),
        Bgini ~ dnorm(-52.547, 363.284),
        Btrop ~ dnorm(-3.189, 58.275),
        Bpol ~ dnorm(-0.133, 3.771),
        Bef ~ dnorm(-0.967, 18.066),
        c(B05, B10, B15) ~ dnorm(0, 10),
        sigma ~ dcauchy(0, 2)

    ), data = d, iter = 2000, chains = 4, cores = 2
)

t2_m2_rp <- map2stan(
    alist(
        dale ~ dnorm(mu, sigma),
        mu <- a + Bpko * pkoyearsany + Bdt * deathstotal_and_osv_1000 +
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
t3_m1_ip <- map2stan(
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
        Btrop ~ dnorm(-3.22, 58.099),
        Bpol ~ dnorm(-0.135, 3.757),
        Bef ~ dnorm(-0.922, 18.047),
        c(B05, B10, B15) ~ dnorm(0, 10),
        sigma ~ dcauchy(0, 2)

    ), data = d, iter = 2000, chains = 4, cores = 2
)

# PKO x OSV
t3_m2_rp <- map2stan(
    alist(
        dale ~ dnorm(mu, sigma),
        mu <- a + Bpko * pkoyearsany + Bdt * osv_per1000 +
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
t4_m1_ip <- map2stan(
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
        Btrop ~ dnorm(-3.22, 58.099),
        Bpol ~ dnorm(-0.135, 3.757),
        Bef ~ dnorm(-0.922, 18.047),
        c(B05, B10, B15) ~ dnorm(0, 10),
        sigma ~ dcauchy(0, 2)

    ), data = d, iter = 2000, chains = 4, cores = 2
)

# PKO X BRDs.
t4_m2_rp <- map2stan(
    alist(
        dale ~ dnorm(mu, sigma),
        mu <- a + Bpko * pkoyearsany + Bdt * deathstotal_new +
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


