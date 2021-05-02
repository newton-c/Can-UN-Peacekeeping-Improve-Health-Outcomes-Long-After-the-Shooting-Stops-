library(dplyr)
library(haven)
library(modelsummary)
library(rethinking)

d <- read_dta("data/data21Dec2020_2.dta")

d$y00 <- ifelse(d$year == 2000, 1, 0)
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


d_no <- filter(d, deathstotal_and_osv_10000 < 4) # Within 2 s.d.
unstd <- lm(dale ~ pkoyearsany + deathstotal_and_osv_1000 +
   helog_knn + hdi_knn + civilwarborder + urbangrowth_knn +
   gini_knn + tropical + xpolity_knn + ef_knn + y05 + y10 +  y15, data = d)

std <- lm(dale ~ pkoyearsany + deathstotal_and_osv_1000 +
   helog_knn + hdi_knn + civilwarborder + urbangrowth_knn +
   gini_knn + tropical + xpolity_knn + ef_knn + y05 + y10 +  y15, data = d_s)
# Informed priors. ------------------------------------------------------------
# Using DALY 15-44 averaged for fe/males
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
        Bpko ~ dnorm(1.714, 0.918),
        Bdt ~ dnorm(-0.363, 0.156),
        Bbcw ~ dnorm(-10.196, 3.319),
        Bhe ~ dnorm(-5.915, 1.619),
        Bhdi ~ dnorm(5.52, 4.3935),
        Bug ~ dnorm(-5.915, 1.619),
        Bgini ~ dnorm(-51.3645, 21.6019),
        Btrop ~ dnorm(-3.22, 3.284),
        Bpol ~ dnorm(0.135, 0.295),
        Bef ~ dnorm(-0.9215, 1.4757),
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
        Bpko ~ dnorm(1.55, 0.56),
        Bdt ~ dnorm(-0.555, 0.345),
        Bint ~ dnorm(0.05, 0.085),
        Bbcw ~ dnorm(-10.17, 3.265),
        Bhe ~ dnorm(2.035, 2.245),
        Bhdi ~ dnorm(5.43, 4.58),
        Bug ~ dnorm(-7.23, 3.075),
        Bgini ~ dnorm(-52.55, 27.23),
        Btrop ~ dnorm(-3.19, 4.37),
        Bpol ~ dnorm(-0.135, 0.28),
        Bef ~ dnorm(-0.97, 1.355),
        c(B05, B10, B15) ~ dnorm(0, 10),
        sigma ~ dcauchy(0, 2)

    ), data = d, iter = 2000, chains = 4, cores = 2
)

# No outliers.
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
        Bpko ~ dnorm(1.714, 0.918),
        Bdt ~ dnorm(-0.363, 0.156),
        Bbcw ~ dnorm(-10.196, 3.319),
        Bhe ~ dnorm(-5.915, 1.619),
        Bhdi ~ dnorm(5.52, 4.3935),
        Bug ~ dnorm(-5.915, 1.619),
        Bgini ~ dnorm(-51.3645, 21.6019),
        Btrop ~ dnorm(-3.22, 3.284),
        Bpol ~ dnorm(0.135, 0.295),
        Bef ~ dnorm(-0.9215, 1.4757),
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
        Bpko ~ dnorm(1.55, 0.56),
        Bdt ~ dnorm(-0.555, 0.345),
        Bint ~ dnorm(0.05, 0.085),
        Bbcw ~ dnorm(-10.17, 3.265),
        Bhe ~ dnorm(2.035, 2.245),
        Bhdi ~ dnorm(5.43, 4.58),
        Bug ~ dnorm(-7.23, 3.075),
        Bgini ~ dnorm(-52.55, 27.23),
        Btrop ~ dnorm(-3.19, 4.37),
        Bpol ~ dnorm(-0.135, 0.28),
        Bef ~ dnorm(-0.97, 1.355),
        c(B05, B10, B15) ~ dnorm(0, 10),
        sigma ~ dcauchy(0, 2)

    ), data = d_no, iter = 2000, chains = 4, cores = 2
)
#precis(T2_M1)
#precis(T2_M2)
#precis(T2_M3)
#precis(T2_M4)

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
        Bpko ~ dnorm(1.714, 0.918),
        Bdt ~ dnorm(-0.363, 0.156),
        Bbcw ~ dnorm(-10.196, 3.319),
        Bhe ~ dnorm(-5.915, 1.619),
        Bhdi ~ dnorm(5.52, 4.3935),
        Bug ~ dnorm(-5.915, 1.619),
        Bgini ~ dnorm(-51.3645, 21.6019),
        Btrop ~ dnorm(-3.22, 3.284),
        Bpol ~ dnorm(0.135, 0.295),
        Bef ~ dnorm(-0.9215, 1.4757),
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
        Bpko ~ dnorm(1.55, 0.56),
        Bdt ~ dnorm(-0.555, 0.345),
        Bint ~ dnorm(0.05, 0.085),
        Bbcw ~ dnorm(-10.17, 3.265),
        Bhe ~ dnorm(2.035, 2.245),
        Bhdi ~ dnorm(5.43, 4.58),
        Bug ~ dnorm(-7.23, 3.075),
        Bgini ~ dnorm(-52.55, 27.23),
        Btrop ~ dnorm(-3.19, 4.37),
        Bpol ~ dnorm(-0.135, 0.28),
        Bef ~ dnorm(-0.97, 1.355),
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
        Bpko ~ dnorm(1.714, 0.918),
        Bdt ~ dnorm(-0.363, 0.156),
        Bbcw ~ dnorm(-10.196, 3.319),
        Bhe ~ dnorm(-5.915, 1.619),
        Bhdi ~ dnorm(5.52, 4.3935),
        Bug ~ dnorm(-5.915, 1.619),
        Bgini ~ dnorm(-51.3645, 21.6019),
        Btrop ~ dnorm(-3.22, 3.284),
        Bpol ~ dnorm(0.135, 0.295),
        Bef ~ dnorm(-0.9215, 1.4757),
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
        Bpko ~ dnorm(1.55, 0.56),
        Bdt ~ dnorm(-0.555, 0.345),
        Bint ~ dnorm(0.05, 0.085),
        Bbcw ~ dnorm(-10.17, 3.265),
        Bhe ~ dnorm(2.035, 2.245),
        Bhdi ~ dnorm(5.43, 4.58),
        Bug ~ dnorm(-7.23, 3.075),
        Bgini ~ dnorm(-52.55, 27.23),
        Btrop ~ dnorm(-3.19, 4.37),
        Bpol ~ dnorm(-0.135, 0.28),
        Bef ~ dnorm(-0.97, 1.355),
        c(B05, B10, B15) ~ dnorm(0, 10),
        sigma ~ dcauchy(0, 2)
    ), data = d_no, iter = 2000, chains = 4, cores = 2
)
#precis(T3_M1)
#precis(T3_M2)
#precis(T3_M3)
#precis(T3_M4)



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
        Bpko ~ dnorm(1.714, 0.918),
        Bdt ~ dnorm(-0.363, 0.156),
        Bbcw ~ dnorm(-10.196, 3.319),
        Bhe ~ dnorm(-5.915, 1.619),
        Bhdi ~ dnorm(5.52, 4.3935),
        Bug ~ dnorm(-5.915, 1.619),
        Bgini ~ dnorm(-51.3645, 21.6019),
        Btrop ~ dnorm(-3.22, 3.284),
        Bpol ~ dnorm(0.135, 0.295),
        Bef ~ dnorm(-0.9215, 1.4757),
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
        Bpko ~ dnorm(1.55, 0.56),
        Bdt ~ dnorm(-0.555, 0.345),
        Bint ~ dnorm(0.05, 0.085),
        Bbcw ~ dnorm(-10.17, 3.265),
        Bhe ~ dnorm(2.035, 2.245),
        Bhdi ~ dnorm(5.43, 4.58),
        Bug ~ dnorm(-7.23, 3.075),
        Bgini ~ dnorm(-52.55, 27.23),
        Btrop ~ dnorm(-3.19, 4.37),
        Bpol ~ dnorm(-0.135, 0.28),
        Bef ~ dnorm(-0.97, 1.355),
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
        Bpko ~ dnorm(1.714, 0.918),
        Bdt ~ dnorm(-0.363, 0.156),
        Bbcw ~ dnorm(-10.196, 3.319),
        Bhe ~ dnorm(-5.915, 1.619),
        Bhdi ~ dnorm(5.52, 4.3935),
        Bug ~ dnorm(-5.915, 1.619),
        Bgini ~ dnorm(-51.3645, 21.6019),
        Btrop ~ dnorm(-3.22, 3.284),
        Bpol ~ dnorm(0.135, 0.295),
        Bef ~ dnorm(-0.9215, 1.4757),
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
        Bpko ~ dnorm(1.55, 0.56),
        Bdt ~ dnorm(-0.555, 0.345),
        Bint ~ dnorm(0.05, 0.085),
        Bbcw ~ dnorm(-10.17, 3.265),
        Bhe ~ dnorm(2.035, 2.245),
        Bhdi ~ dnorm(5.43, 4.58),
        Bug ~ dnorm(-7.23, 3.075),
        Bgini ~ dnorm(-52.55, 27.23),
        Btrop ~ dnorm(-3.19, 4.37),
        Bpol ~ dnorm(-0.135, 0.28),
        Bef ~ dnorm(-0.97, 1.355),
        c(B05, B10, B15) ~ dnorm(0, 10),
        sigma ~ dcauchy(0, 2)
    ), data = d_no, iter = 2000, chains = 4, cores = 2
)

print("Table 2")
precis(T2_M1)
precis(T2_M2)
precis(T2_M3)
precis(T2_M4)

print("Table 3")
precis(T3_M1)
precis(T3_M2)
precis(T3_M3)
precis(T3_M4)

print("Table 4")
precis(T4_M1)
precis(T4_M2)
precis(T4_M3)
precis(T4_M4)
compare(T2_M1, T2_M2, T3_M1, T3_M2, T4_M1, T4_M2)
compare(T2_M3, T2_M4, T3_M3, T3_M4, T4_M3, T4_M4)

library(bayesplot)
p0 <- mcmc_areas(as.matrix(T2_M1@stanfit), pars=vars('Bpko', 'Bdt'),
                 prob_outer=0.95, area_method = "scaled height")+
    geom_vline(xintercept = 0, alpha = 0.4) +
    ggtitle("Model 1", "All Observations") +
    scale_y_discrete(labels=c("Bpko" = "PKO", "Bdt" = "Violence")) +
    mcmc_areas(as.matrix(T2_M2@stanfit), pars=vars('Bpko', 'Bdt', 'Bint'),
                 prob_outer=0.95, area_method = "scaled height") +
    geom_vline(xintercept = 0, alpha = 0.4) +
    ggtitle("Model 2", "All Observations") +
    scale_y_discrete(labels=c("Bpko" = "PKO", "Bdt" = "Violence",
        "Bint" = "Interaction")) +
    mcmc_areas(as.matrix(T2_M3@stanfit), pars=vars('Bpko', 'Bdt'),
                 prob_outer=0.95, area_method = "scaled height") +
    geom_vline(xintercept = 0, alpha = 0.4) +
    scale_y_discrete(labels=c("Bpko" = "PKO", "Bdt" = "Violence")) +
    ggtitle("Model 3", "No Outliers") +
    mcmc_areas(as.matrix(T2_M4@stanfit), pars=vars('Bpko', 'Bdt', 'Bint'),
                 prob_outer=0.95, area_method = "scaled height") +
    geom_vline(xintercept = 0, alpha = 0.4) +
    ggtitle("Model 4", "No Outliers") +
    scale_y_discrete(labels=c("Bpko" = "PKO", "Bdt" = "Violence",
        "Bint" = "Interaction"))
#ggsave("figs/distplots.png", height = 6, width = 20)

library(bayesplot)
p0 <- mcmc_areas(as.matrix(T2_M2@stanfit), pars = vars("Bpko", "Bdt", "Bint"),
                 prob_outer=0.95, area_method = "scaled height") +
    geom_vline(xintercept = 0, alpha = 0.4) + title("Total Violence") +
    mcmc_areas(as.matrix(T3_M2@stanfit), pars = vars("Bpko", "Bdt", "Bint"),
                 prob_outer=0.95, area_method = "scaled height") +
    geom_vline(xintercept = 0, alpha = 0.4) + title("One-Sided Violence") +
    mcmc_areas(as.matrix(T4_M2@stanfit), pars = vars("Bpko", "Bdt", "Bint"),
                 prob_outer=0.95, area_method = "scaled height")+
    geom_vline(xintercept = 0, alpha = 0.4) + title("Battle-Related Deaths")


# Only conflict countries. ----------------------------------------------------
d_con <- filter(d, intensity > 0)

# PKO, no interaction.
OnlyCon_M1 <- map2stan(
    alist(
        dale ~ dnorm(mu, sigma),
        mu <- a + Bpko * pkoyearsany + Bdt * deathstotal_and_osv_10000 +
            Bhe * helog_knn + Bhdi * hdi_knn +
            Bbcw * civilwarborder + Bug * urbangrowth_knn +
            Bgini * gini_knn + Btrop * tropical + Bpol * xpolity_knn +
            Bef * ef_knn + B05 * y05 + B10 * y10 + B15 * y15,
        a ~ dnorm(55, 10),
        Bpko ~ dnorm(1.714, 0.918),
        Bdt ~ dnorm(-0.363, 0.156),
        Bbcw ~ dnorm(-10.196, 3.319),
        Bhe ~ dnorm(-5.915, 1.619),
        Bhdi ~ dnorm(5.52, 4.3935),
        Bug ~ dnorm(-5.915, 1.619),
        Bgini ~ dnorm(-51.3645, 21.6019),
        Btrop ~ dnorm(-3.22, 3.284),
        Bpol ~ dnorm(0.135, 0.295),
        Bef ~ dnorm(-0.9215, 1.4757),
        c(B05, B10, B15) ~ dnorm(0, 10),
        sigma ~ dcauchy(0, 2)

    ), data = d_con, iter = 2000, chains = 4, cores = 2
)


# PKO x total violence.
OnlyCon_M2 <- map2stan(
    alist(
        dale ~ dnorm(mu, sigma),
        mu <- a + Bpko * pkoyearsany + Bdt * deathstotal_and_osv_10000 +
            Bint * pkoxdeath + Bhe * helog_knn + Bhdi * hdi_knn +
            Bbcw * civilwarborder + Bug * urbangrowth_knn +
            Bgini * gini_knn + Btrop * tropical + Bpol * xpolity_knn +
            Bef * ef_knn + B05 * y05 + B10 * y10 + B15 * y15,
        a ~ dnorm(55, 10),
        Bpko ~ dnorm(1.55, 0.56),
        Bdt ~ dnorm(-0.555, 0.345),
        Bint ~ dnorm(0.05, 0.085),
        Bbcw ~ dnorm(-10.17, 3.265),
        Bhe ~ dnorm(2.035, 2.245),
        Bhdi ~ dnorm(5.43, 4.58),
        Bug ~ dnorm(-7.23, 3.075),
        Bgini ~ dnorm(-52.55, 27.23),
        Btrop ~ dnorm(-3.19, 4.37),
        Bpol ~ dnorm(-0.135, 0.28),
        Bef ~ dnorm(-0.97, 1.355),
        c(B05, B10, B15) ~ dnorm(0, 10),
        sigma ~ dcauchy(0, 2)

    ), data = d_con, iter = 2000, chains = 4, cores = 2
)

precis(OnlyCon_M1)
precis(OnlyCon_M2)


# Only countries with conflict or pko -----------------------------------------
d_conpko <- filter(d, intensity > 0 | pkoyearsany > 0)

# PKO, no interaction.
OnlyConPKO_M1 <- map2stan(
    alist(
        dale ~ dnorm(mu, sigma),
        mu <- a + Bpko * pkoyearsany + Bdt * deathstotal_and_osv_10000 +
            Bhe * helog_knn + Bhdi * hdi_knn +
            Bbcw * civilwarborder + Bug * urbangrowth_knn +
            Bgini * gini_knn + Btrop * tropical + Bpol * xpolity_knn +
            Bef * ef_knn + B05 * y05 + B10 * y10 + B15 * y15,
        a ~ dnorm(55, 10),
        Bpko ~ dnorm(1.714, 0.918),
        Bdt ~ dnorm(-0.363, 0.156),
        Bbcw ~ dnorm(-10.196, 3.319),
        Bhe ~ dnorm(-5.915, 1.619),
        Bhdi ~ dnorm(5.52, 4.3935),
        Bug ~ dnorm(-5.915, 1.619),
        Bgini ~ dnorm(-51.3645, 21.6019),
        Btrop ~ dnorm(-3.22, 3.284),
        Bpol ~ dnorm(0.135, 0.295),
        Bef ~ dnorm(-0.9215, 1.4757),
        c(B05, B10, B15) ~ dnorm(0, 10),
        sigma ~ dcauchy(0, 2)

    ), data = d_conpko, iter = 2000, chains = 4, cores = 2
)


# PKO x total violence.
OnlyConPKO_M2 <- map2stan(
    alist(
        dale ~ dnorm(mu, sigma),
        mu <- a + Bpko * pkoyearsany + Bdt * deathstotal_and_osv_10000 +
            Bint * pkoxdeath + Bhe * helog_knn + Bhdi * hdi_knn +
            Bbcw * civilwarborder + Bug * urbangrowth_knn +
            Bgini * gini_knn + Btrop * tropical + Bpol * xpolity_knn +
            Bef * ef_knn + B05 * y05 + B10 * y10 + B15 * y15,
        a ~ dnorm(55, 10),
        Bpko ~ dnorm(1.55, 0.56),
        Bdt ~ dnorm(-0.555, 0.345),
        Bint ~ dnorm(0.05, 0.085),
        Bbcw ~ dnorm(-10.17, 3.265),
        Bhe ~ dnorm(2.035, 2.245),
        Bhdi ~ dnorm(5.43, 4.58),
        Bug ~ dnorm(-7.23, 3.075),
        Bgini ~ dnorm(-52.55, 27.23),
        Btrop ~ dnorm(-3.19, 4.37),
        Bpol ~ dnorm(-0.135, 0.28),
        Bef ~ dnorm(-0.97, 1.355),
        c(B05, B10, B15) ~ dnorm(0, 10),
        sigma ~ dcauchy(0, 2)
    ), data = d_conpko, iter = 2000, chains = 4, cores = 2
)

precis(OnlyConPKO_M1)
precis(OnlyConPKO_M2)



# SD fron table 1 added---------------------------------------------------------
# Using DALY 15-44 averaged for fe/males
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
        Bpko ~ dnorm(1.715, 6.871),
        Bdt ~ dnorm(-0.363, 1.881),
        Bbcw ~ dnorm(-10.196, 43.360),
        Bhe ~ dnorm(2.1, 29.552),
        Bhdi ~ dnorm(5.52, 60.838),
        Bug ~ dnorm(-7.21, 40.892),
        Bgini ~ dnorm(-52.365, 359.424),
        Btrop ~ dnorm(-3.215, 58.036),
        Bpol ~ dnorm(0.137, 3.736),
        Bef ~ dnorm(-0.9215, 18.011),
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
        Bpko ~ dnorm(1.55, 7.471),
        Bdt ~ dnorm(-0.555, 4.603),
        Bint ~ dnorm(0.05, 1.134),
        Bbcw ~ dnorm(-10.17, 43.561),
        Bhe ~ dnorm(2.035, 29.952),
        Bhdi ~ dnorm(5.43, 61.105),
        Bug ~ dnorm(-7.23, 41.026),
        Bgini ~ dnorm(-52.55, 363.294),
        Btrop ~ dnorm(-3.19, 58.303),
        Bpol ~ dnorm(-0.135, 3.736),
        Bef ~ dnorm(-0.97, 18.078),
        c(B05, B10, B15) ~ dnorm(0, 10),
        sigma ~ dcauchy(0, 2)

    ), data = d, iter = 2000, chains = 4, cores = 2
)

# No outliers.
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
        Bpko ~ dnorm(1.715, 6.871),
        Bdt ~ dnorm(-0.363, 1.881),
        Bbcw ~ dnorm(-10.196, 43.360),
        Bhe ~ dnorm(2.1, 29.552),
        Bhdi ~ dnorm(5.52, 60.838),
        Bug ~ dnorm(-7.21, 40.892),
        Bgini ~ dnorm(-52.365, 359.424),
        Btrop ~ dnorm(-3.215, 58.036),
        Bpol ~ dnorm(0.137, 3.736),
        Bef ~ dnorm(-0.9215, 18.011),
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
        Bpko ~ dnorm(1.55, 7.471),
        Bdt ~ dnorm(-0.555, 4.603),
        Bint ~ dnorm(0.05, 1.134),
        Bbcw ~ dnorm(-10.17, 43.561),
        Bhe ~ dnorm(2.035, 29.952),
        Bhdi ~ dnorm(5.43, 61.105),
        Bug ~ dnorm(-7.23, 41.026),
        Bgini ~ dnorm(-52.55, 363.294),
        Btrop ~ dnorm(-3.19, 58.303),
        Bpol ~ dnorm(-0.135, 3.736),
        Bef ~ dnorm(-0.97, 18.078),
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
        Bpko ~ dnorm(1.715, 6.871),
        Bdt ~ dnorm(-0.363, 1.881),
        Bbcw ~ dnorm(-10.196, 43.360),
        Bhe ~ dnorm(2.1, 29.552),
        Bhdi ~ dnorm(5.52, 60.838),
        Bug ~ dnorm(-7.21, 40.892),
        Bgini ~ dnorm(-52.365, 359.424),
        Btrop ~ dnorm(-3.215, 58.036),
        Bpol ~ dnorm(0.137, 3.736),
        Bef ~ dnorm(-0.9215, 18.011),
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
        Bpko ~ dnorm(1.55, 7.471),
        Bdt ~ dnorm(-0.555, 4.603),
        Bint ~ dnorm(0.05, 1.134),
        Bbcw ~ dnorm(-10.17, 43.561),
        Bhe ~ dnorm(2.035, 29.952),
        Bhdi ~ dnorm(5.43, 61.105),
        Bug ~ dnorm(-7.23, 41.026),
        Bgini ~ dnorm(-52.55, 363.294),
        Btrop ~ dnorm(-3.19, 58.303),
        Bpol ~ dnorm(-0.135, 3.736),
        Bef ~ dnorm(-0.97, 18.078),
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
        Bpko ~ dnorm(1.715, 6.871),
        Bdt ~ dnorm(-0.363, 1.881),
        Bbcw ~ dnorm(-10.196, 43.360),
        Bhe ~ dnorm(2.1, 29.552),
        Bhdi ~ dnorm(5.52, 60.838),
        Bug ~ dnorm(-7.21, 40.892),
        Bgini ~ dnorm(-52.365, 359.424),
        Btrop ~ dnorm(-3.215, 58.036),
        Bpol ~ dnorm(0.137, 3.736),
        Bef ~ dnorm(-0.9215, 18.011),
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
        Bpko ~ dnorm(1.55, 7.471),
        Bdt ~ dnorm(-0.555, 4.603),
        Bint ~ dnorm(0.05, 1.134),
        Bbcw ~ dnorm(-10.17, 43.561),
        Bhe ~ dnorm(2.035, 29.952),
        Bhdi ~ dnorm(5.43, 61.105),
        Bug ~ dnorm(-7.23, 41.026),
        Bgini ~ dnorm(-52.55, 363.294),
        Btrop ~ dnorm(-3.19, 58.303),
        Bpol ~ dnorm(-0.135, 3.736),
        Bef ~ dnorm(-0.97, 18.078),
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
        Bpko ~ dnorm(1.715, 6.871),
        Bdt ~ dnorm(-0.363, 1.881),
        Bbcw ~ dnorm(-10.196, 43.360),
        Bhe ~ dnorm(2.1, 29.552),
        Bhdi ~ dnorm(5.52, 60.838),
        Bug ~ dnorm(-7.21, 40.892),
        Bgini ~ dnorm(-52.365, 359.424),
        Btrop ~ dnorm(-3.215, 58.036),
        Bpol ~ dnorm(0.137, 3.736),
        Bef ~ dnorm(-0.9215, 18.011),
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
        Bpko ~ dnorm(1.55, 7.471),
        Bdt ~ dnorm(-0.555, 4.603),
        Bint ~ dnorm(0.05, 1.134),
        Bbcw ~ dnorm(-10.17, 43.561),
        Bhe ~ dnorm(2.035, 29.952),
        Bhdi ~ dnorm(5.43, 61.105),
        Bug ~ dnorm(-7.23, 41.026),
        Bgini ~ dnorm(-52.55, 363.294),
        Btrop ~ dnorm(-3.19, 58.303),
        Bpol ~ dnorm(-0.135, 3.736),
        Bef ~ dnorm(-0.97, 18.078),
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
        Bpko ~ dnorm(1.715, 6.871),
        Bdt ~ dnorm(-0.363, 1.881),
        Bbcw ~ dnorm(-10.196, 43.360),
        Bhe ~ dnorm(2.1, 29.552),
        Bhdi ~ dnorm(5.52, 60.838),
        Bug ~ dnorm(-7.21, 40.892),
        Bgini ~ dnorm(-52.365, 359.424),
        Btrop ~ dnorm(-3.215, 58.036),
        Bpol ~ dnorm(0.137, 3.736),
        Bef ~ dnorm(-0.9215, 18.011),
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
            Bef * ef_knn + B05 * y05 + B10 * y10 + B15 * y15,
        a ~ dnorm(55, 10),
        Bpko ~ dnorm(1.55, 7.471),
        Bdt ~ dnorm(-0.555, 4.603),
        Bint ~ dnorm(0.05, 1.134),
        Bbcw ~ dnorm(-10.17, 43.561),
        Bhe ~ dnorm(2.035, 29.952),
        Bhdi ~ dnorm(5.43, 61.105),
        Bug ~ dnorm(-7.23, 41.026),
        Bgini ~ dnorm(-52.55, 363.294),
        Btrop ~ dnorm(-3.19, 58.303),
        Bpol ~ dnorm(-0.135, 3.736),
        Bef ~ dnorm(-0.97, 18.078),
        c(B05, B10, B15) ~ dnorm(0, 10),
        sigma ~ dcauchy(0, 2)
    ), data = d_no, iter = 2000, chains = 4, cores = 2
)


# Standardized varibles -------------------------------------------------------
