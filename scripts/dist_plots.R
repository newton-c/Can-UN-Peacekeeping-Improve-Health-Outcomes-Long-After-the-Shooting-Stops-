#library(bayesplot)

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


p0 <- mcmc_areas(as.matrix(T2_M2@stanfit), pars = vars("Bpko", "Bdt", "Bint"),
                 prob_outer=0.95, area_method = "scaled height") +
    geom_vline(xintercept = 0, alpha = 0.4) + title("Total Violence") +
    mcmc_areas(as.matrix(T3_M2@stanfit), pars = vars("Bpko", "Bdt", "Bint"),
                 prob_outer=0.95, area_method = "scaled height") +
    geom_vline(xintercept = 0, alpha = 0.4) + title("One-Sided Violence") +
    mcmc_areas(as.matrix(T4_M2@stanfit), pars = vars("Bpko", "Bdt", "Bint"),
                 prob_outer=0.95, area_method = "scaled height")+
    geom_vline(xintercept = 0, alpha = 0.4) + title("Battle-Related Deaths")
