################################################################################
#' This script produces the coeffecient plots seen in figures 3 and 4, and saves
#' the .png files as "additive_coef_plot.png" (figure 3) and
#' "interation_coef_plot.png" (figure 4) in the "figs" folder.
###############################################################################

library(bayesplot)
library(patchwork)
library(ggplot2)
color_scheme_set("darkgray")

# define functions
coef_plots_add <- function(model, title, N, WAIC, capt) {
    capt_text <- paste0("Measure of Violence = ", capt, "\n", "N = ", N, "\n",
                        "WAIC = ", WAIC, "\n")
    mcmc_intervals(as.matrix(model@stanfit), regex_pars = "B|sigma", prob = 0.5,
                   prob_outer = 0.5, point_est = "mean", point_size = 3) +
    geom_vline(xintercept = 0, linetype = 2) +
    labs(title = title,
         caption = capt_text) +
    scale_y_discrete(
        labels = c("sigma" = "sigma", "B15" = "2015", "B10" = "2010",
                   "B05" = "2005", "Bef" = "Ethnic Fract.",
                   "Bpol" = "x-Polity", "Btrop" = "Tropical",
                   "Bgini" = "Income Inequality", "Bug" = "Urban Growth",
                   "Bhdi" = "Education", "Bhe" = "Health Exp",
                   "Bbcw" = "Contig. CW", "Bdt" = "Violence", "Bpko" = "PKO"),
        limits = c("sigma", "B15", "B10", "B05", "Bef", "Bpol", "Btrop",
                   "Bgini", "Bug", "Bhdi", "Bhe", "Bbcw", "Bdt", "Bpko")) +
        theme(text = element_text(family = "serif"))
}

coef_plots_int <- function(model, title, N, WAIC, capt) {
    capt_text <- paste0("Measure of Violence = ", capt, "\n", "N = ", N, "\n",
                        "WAIC = ", WAIC, "\n")
    mcmc_intervals(as.matrix(model@stanfit), regex_pars = "B|sigma", prob = 0.5,
                   prob_outer = 0.5, point_est = "mean", point_size = 3) +
    geom_vline(xintercept = 0, linetype = 2) +
    labs(title = title,
         caption = capt_text) +
    scale_y_discrete(
        labels = c("sigma" = "sigma", "B15" = "2015", "B10" = "2010",
                   "B05" = "2005", "Bef" = "Ethnic Fract.",
                   "Bpol" = "x-Polity", "Btrop" = "Tropical",
                   "Bgini" = "Income Inequality", "Bug" = "Urban Growth",
                   "Bhdi" = "Education", "Bhe" = "Health Exp",
                   "Bbcw" = "Contig. CW", "Bint" = "PKO x Violence",
                   "Bdt" = "Violence", "Bpko" = "PKO"),
        limits = c("sigma", "B15", "B10", "B05", "Bef", "Bpol", "Btrop",
                   "Bgini", "Bug", "Bhdi", "Bhe", "Bbcw", "Bint", "Bdt",
                   "Bpko")) +
        theme(text = element_text(family = "serif"))
}


# additive models
total_violence_add_plot <- coef_plots_add(total_violence_add_inform_sd,
                                          title = "Healthy Life Expectancy",
                                          capt = "Total Violence", N = "681",
                                          WAIC = "4083.33")

osv_add_plot <- coef_plots_add(osv_add_inform_sd, title = "",
                               capt = "One-Sided Violence",
                               N = "681", WAIC = "4083.44") +
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())

brd_add_plot <- coef_plots_add(brd_add_inform_sd, title = "",
                               capt = "Battle-Related Deaths", N = "681",
                               WAIC = "4083.89") +
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())

total_violence_add_plot + osv_add_plot + brd_add_plot
ggsave("figs/coef_plots/additive_coef_plot.png", height = 4, width = 12)


# interation models
total_violence_int_plot <- coef_plots_int(total_violence_int_inform_sd,
                                          title = "Healthy Life Expectancy",
                                          capt = "Total Violence", N = "681",
                                          WAIC = "4081.23")

osv_int_plot <- coef_plots_int(osv_int_inform_sd, capt = "One-Sided Violence",
                               title = "", N = "681", WAIC = "4081.77") +
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())


brd_int_plot <- coef_plots_int(brd_int_inform_sd,
                               capt = "Battle-Related Deaths", title = "",
                               N = "681",  WAIC = "4085.89") +
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())


total_violence_int_plot + osv_int_plot + brd_int_plot
ggsave("figs/coef_plots/interation_coef_plot.png", height = 4, width = 12)
