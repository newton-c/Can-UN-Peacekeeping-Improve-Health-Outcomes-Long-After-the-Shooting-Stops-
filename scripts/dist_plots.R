library(bayesplot)
color_scheme_set("darkgray")

# Total Violence Regularized
total_violence_add_plot <- mcmc_areas(as.matrix(total_violence_add@stanfit),
                     pars = vars("Bpko", "Bdt"),
                 prob = 0.68, prob_outer = 0.99,
                 area_method = "scaled height") +
    ggtitle(label = "Total Violence", subtitle = "Regularized Priors") +
    geom_vline(xintercept = 0, linetype = 1) +
    scale_y_discrete(labels = c("Violence", "PKO"),
                     limits = c("Bdt", "Bpko"))
ggsave("figs/dist_plots/dist_add_tv.png", height = 4, width = 12)

total_violence_int_plot <- mcmc_areas(as.matrix(total_violence_int@stanfit),
                                 pars = vars("Bpko", "Bdt", "Bint"),
                 prob = 0.68, prob_outer = 0.99,
                 area_method = "scaled height") +
    ggtitle(label = "Total Violence", subtitle = "Regularized Priors") +
    geom_vline(xintercept = 0, linetype = 1) +
    scale_y_discrete(labels = c("PKO x Violence", "Violence", "PKO"),
                     limits = c("Bint", "Bdt", "Bpko"))
ggsave("figs/dist_plots/dist_int_tv.png", height = 4, width = 12)

# Total Violence Informative
total_violence_add_inform_plot <-
    mcmc_areas(as.matrix(total_violence_add_inform@stanfit),
                     pars = vars("Bpko", "Bdt"),
                 prob = 0.68, prob_outer = 0.99,
                 area_method = "scaled height") +
    ggtitle(label = "Total Violence", subtitle = "Informative Priors") +
    geom_vline(xintercept = 0, linetype = 1) +
    scale_y_discrete(labels = c("Violence", "PKO"),
                     limits = c("Bdt", "Bpko"))
ggsave("figs/dist_plots/dist_add_tv_inform.png", height = 4, width = 12)

total_violence_int_inform_plot <-
    mcmc_areas(as.matrix(total_violence_int_inform@stanfit),
                                 pars = vars("Bpko", "Bdt", "Bint"),
                 prob = 0.68, prob_outer = .99,
                 area_method = "scaled height") +
    ggtitle(label = "Total Violence", subtitle = "Informative Priors") +
    geom_vline(xintercept = 0, linetype = 1) +
    scale_y_discrete(labels = c("PKO x Violence", "Violence", "PKO"),
                     limits = c("Bint", "Bdt", "Bpko"))
ggsave("figs/dist_plots/dist_int_tv_inform.png", height = 4, width = 12)

# One-Sided Violence ----------------------------------------------------------
# Regularized
osv_add_plot <- mcmc_areas(as.matrix(osv_add@stanfit),
                     pars = vars("Bpko", "Bdt"),
                 prob = 0.68, prob_outer = 0.99,
                 area_method = "scaled height") +
    ggtitle(label = "One-Sided Violence", subtitle = "Regularized Priors") +
    geom_vline(xintercept = 0, linetype = 1) +
    scale_y_discrete(labels = c("Violence", "PKO"),
                     limits = c("Bdt", "Bpko"))
ggsave("figs/dist_plots/dist_add_osv.png", height = 4, width = 12)

osv_int_plot  <- mcmc_areas(as.matrix(osv_int@stanfit),
                                 pars = vars("Bpko", "Bdt", "Bint"),
                 prob = 0.68, prob_outer = 0.99,
                 area_method = "scaled height") +
    ggtitle(label = "One-Sided Violence", subtitle = "Regularized Priors") +
    geom_vline(xintercept = 0, linetype = 1) +
    scale_y_discrete(labels = c("PKO x Violence", "Violence", "PKO"),
                     limits = c("Bint", "Bdt", "Bpko"))
ggsave("figs/dist_plots/dist_int_osv.png", height = 4, width = 12)

# Informative
osv_add_inform_plot <- mcmc_areas(as.matrix(osv_add_inform@stanfit),
                     pars = vars("Bpko", "Bdt"),
                 prob = 0.68, prob_outer = 0.99,
                 area_method = "scaled height") +
    ggtitle(label = "One-Sided Violence", subtitle = "Informative Priors") +
    geom_vline(xintercept = 0, linetype = 1) +
    scale_y_discrete(labels = c("Violence", "PKO"),
                     limits = c("Bdt", "Bpko"))
ggsave("figs/dist_plots/dist_add_osv_inform.png", height = 4, width = 12)

osv_int_inform_plot <- mcmc_areas(as.matrix(osv_int_inform@stanfit),
                                 pars = vars("Bpko", "Bdt", "Bint"),
                 prob = 0.68, prob_outer = .99,
                 area_method = "scaled height") +
    ggtitle(label = "One-Sided Violence", subtitle = "Informative Priors") +
    geom_vline(xintercept = 0, linetype = 1) +
    scale_y_discrete(labels = c("PKO x Violence", "Violence", "PKO"),
                     limits = c("Bint", "Bdt", "Bpko"))
ggsave("figs/dist_plots/dist_int_osv_inform.png", height = 4, width = 12)


# Battle-Related Deaths -------------------------------------------------------
# Regularized
brd_add_plot <- mcmc_areas(as.matrix(brd_add@stanfit),
                     pars = vars("Bpko", "Bdt"),
                 prob = 0.68, prob_outer = 0.99,
                 area_method = "scaled height") +
    ggtitle(label = "Battle-Related Deaths", subtitle = "Regularized Priors") +
    geom_vline(xintercept = 0, linetype = 1) +
    scale_y_discrete(labels = c("Violence", "PKO"),
                     limits = c("Bdt", "Bpko"))
ggsave("figs/dist_plots/dist_add_brd.png", height = 4, width = 12)

brd_int_plot  <- mcmc_areas(as.matrix(brd_int@stanfit),
                                 pars = vars("Bpko", "Bdt", "Bint"),
                 prob = 0.68, prob_outer = 0.99,
                 area_method = "scaled height") +
    ggtitle(label = "Battle-Related Deaths", subtitle = "Regularized Priors") +
    geom_vline(xintercept = 0, linetype = 1) +
    scale_y_discrete(labels = c("PKO x Violence", "Violence", "PKO"),
                     limits = c("Bint", "Bdt", "Bpko"))
ggsave("figs/dist_plots/dist_int_brd.png", height = 4, width = 12)

# Informative
brd_add_inform_plot <- mcmc_areas(as.matrix(brd_add_inform@stanfit),
                     pars = vars("Bpko", "Bdt"),
                 prob = 0.68, prob_outer = 0.99,
                 area_method = "scaled height") +
    ggtitle(label = "Battle-Related Deaths", subtitle = "Informative Priors") +
    geom_vline(xintercept = 0, linetype = 1) +
    scale_y_discrete(labels = c("Violence", "PKO"),
                     limits = c("Bdt", "Bpko"))
ggsave("figs/dist_plots/dist_add_brd_inform.png", height = 4, width = 12)

brd_int_inform_plot <- mcmc_areas(as.matrix(brd_int_inform@stanfit),
                                 pars = vars("Bpko", "Bdt", "Bint"),
                 prob = 0.68, prob_outer = .99,
                 area_method = "scaled height") +
    ggtitle(label = "Battle-Related Deaths", subtitle = "Informative Priors") +
    geom_vline(xintercept = 0, linetype = 1) +
    scale_y_discrete(labels = c("PKO x Violence", "Violence", "PKO"),
                     limits = c("Bint", "Bdt", "Bpko"))
ggsave("figs/dist_plots/dist_int_brd_inform.png", height = 4, width = 12)

