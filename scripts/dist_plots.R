#library(bayesplot)

mcmc_areas(as.matrix(T2_M1@stanfit), pars=vars('Bpko', 'Bdt'),
                 prob = 0.89, prob_outer=0.99, area_method = "scaled height")+
    geom_vline(xintercept = 0, alpha = 0.4) +
    ggtitle("Total Violence") +
    scale_y_discrete(labels=c("Violence", "PKO"), limits = c("Bdt", "Bpko"))
ggsave("figs/dist_add_tv.png", height = 8, width = 20)

mcmc_areas(as.matrix(T3_M1@stanfit), pars=vars('Bpko', 'Bdt'),
                 prob = 0.89, prob_outer=0.99, area_method = "scaled height")+
    geom_vline(xintercept = 0, alpha = 0.4) +
    ggtitle("One-Sided Violence") +
    scale_y_discrete(labels=c("Violence", "PKO"), limits = c("Bdt", "Bpko"))
ggsave("figs/dist_add_osv.png", height = 8, width = 20)

mcmc_areas(as.matrix(T4_M1@stanfit), pars=vars('Bpko', 'Bdt'),
                 prob = 0.89, prob_outer=0.99, area_method = "scaled height")+
    geom_vline(xintercept = 0, alpha = 0.4) +
    ggtitle("Battle-Related Deaths") +
    scale_y_discrete(labels=c("Violence", "PKO"), limits = c("Bdt", "Bpko"))
ggsave("figs/dist_add_brd.png", height = 8, width = 20)

# Interations
mcmc_areas(as.matrix(T2_M2@stanfit), pars=vars('Bpko', 'Bdt', 'Bint'),
                 prob = 0.89, prob_outer=0.99, area_method = "scaled height")+
    geom_vline(xintercept = 0, alpha = 0.4) +
    ggtitle("Total Violence") +
    scale_y_discrete(labels=c("Violence x PKO", "Violence", "PKO"),
    limits = c("Bint", "Bdt", "Bpko"))
ggsave("figs/dist_add_tv.png", height = 8, width = 20)

mcmc_areas(as.matrix(T3_M2@stanfit), pars=vars('Bpko', 'Bdt', 'Bint'),
                 prob = 0.89, prob_outer=0.99, area_method = "scaled height")+
    geom_vline(xintercept = 0, alpha = 0.4) +
    ggtitle("One-Sided Violence") +
    scale_y_discrete(labels=c("Violence x PKO", "Violence", "PKO"),
    limits = c("Bint", "Bdt", "Bpko"))
ggsave("figs/dist_add_osv.png", height = 8, width = 20)

mcmc_areas(as.matrix(T4_M2@stanfit), pars=vars('Bpko', 'Bdt', 'Bint'),
                 prob = 0.89, prob_outer=0.99, area_method = "scaled height")+
    geom_vline(xintercept = 0, alpha = 0.4) +
    ggtitle("Battle-Related Deaths") +
    scale_y_discrete(labels=c("Violence x PKO", "Violence", "PKO"),
    limits = c("Bint", "Bdt", "Bpko"))
ggsave("figs/dist_add_brd.png", height = 8, width = 20)
