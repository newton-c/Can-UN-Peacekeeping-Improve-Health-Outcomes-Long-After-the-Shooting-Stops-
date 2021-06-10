m2s_start <- Sys.time()
map2stan_model <- map2stan(
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

m2s_end <- Sys.time()
stan_model_start <- Sys.time()
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
    file = "scripts/for_loop.stan",
    data = stan_data,
    warmup = 1000,
    iter = 2000,
    chains = 4,
    cores = 2
)

fit_rstan

stan_model_end <- Sys.time()


m2s_duration  <- m2s_end - m2s_start
stan_model_duration  <- stan_model_end - stan_model_start
print(m2s_duration)
print(stan_model_duration)
