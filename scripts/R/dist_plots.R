library(bayesplot)
library(ggplot2)
color_scheme_set("darkgray")

# additive models
dist_plots_add <- function(model, title = NULL, capt) {
    mcmc_areas(as.matrix(model@stanfit), pars = vars("Bpko", "Bdt"),
               prob = 0.50, prob_outer = 0.90, area_method = "scaled height",
               point_est = "mean") +
    ggtitle(label = title) +
    labs(caption = capt) +
    geom_vline(xintercept = 0, linetype = 1) +
    scale_y_discrete(labels = c("Violence", "PKO"),
                     limits = c("Bdt", "Bpko"))
}

dist_tv_add <- dist_plots_add(total_violence_add_inform,
                              "Healthy Life Expectancy",
                              capt = "Total Violence")
ggsave("figs/dist_plots/dist_add_tv_inform.png", height = 4, width = 12)
dist_osv_add <- dist_plots_add(osv_add_inform, capt = "One-Sided Violence")
ggsave("figs/dist_plots/dist_add_osv_inform.png", height = 4, width = 12)
dist_brd_add <- dist_plots_add(brd_add_inform, capt = "Battle-Related Deaths")
ggsave("figs/dist_plots/dist_add_brd_inform.png", height = 4, width = 12)

# interation models
dist_plots_int <- function(model, title = NULL, capt) {
    mcmc_areas(as.matrix(model@stanfit), pars = vars("Bpko", "Bdt", "Bint"),
               prob = 0.50, prob_outer = 0.90, area_method = "scaled height",
               point_est = "mean") +
    ggtitle(label = title) +
    labs(caption = capt) +
    geom_vline(xintercept = 0, linetype = 1) +
    scale_y_discrete(labels = c("PKO x Violence", "Violence", "PKO"),
                     limits = c("Bint", "Bdt", "Bpko"))
}

dist_tv_int <- dist_plots_int(total_violence_int_inform,
                              "Healthy Life Expectancy",
                              capt = "Total Violence")
ggsave("figs/dist_plots/dist_int_tv_inform.png", height = 4, width = 12)
dist_osv_int <- dist_plots_int(osv_int_inform, capt = "One-Sided Violence")
ggsave("figs/dist_plots/dist_int_osv_inform.png", height = 4, width = 12)
dist_brd_int <- dist_plots_int(brd_int_inform, capt = "Battle-Related Deaths")
ggsave("figs/dist_plots/dist_int_brd_inform.png", height = 4, width = 12)
