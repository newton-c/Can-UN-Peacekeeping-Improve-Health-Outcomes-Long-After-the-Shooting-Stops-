################################################################################
#' This script produces the coeffecient plots seen in figures 3 and 4, and saves
#' the .png files as "additive_coef_plot.png" (figure 3) and
#' "interation_coef_plot.png" (figure 4) in the "figs" folder.
################################################################################

library(ggplot2)
library(patchwork)
library(rethinking)
#library(ggthemes)

# Additive models -------------------------------------------------------------
theme_set(theme_classic())
coef_plots <- function(data, title, N, WAIC) {
    capt <- paste0(paste0("N = ", N, "WAIC = ", WAIC,
        sep = " "))
    ggplot(data, aes(x = variable, y = value, ymin = lower, ymax = upper)) +
    geom_pointrange(size = 0.125) +
    geom_hline(yintercept = 0, linetype = 2) +
    coord_flip() +
    labs(title = title, caption = capt) +
    ylab("") +
    scale_x_discrete(name = "",
        limits = c("sigma", "2015", "2010", "2005", "Ethnic Fract.", "x-Polity",
        "Tropical", "Income Inequality", "Urban Growth", "Education",
        "Health Exp", "Contig. CW", "Violence", "PKO")) +
        theme(text = element_text(family = "serif"))
}

variable <- c("PKO", "Violence", "Contig. CW", "Health Exp",
    "Education", "Urban Growth", "Income Inequality", "Tropical", "x-Polity",
    "Ethnic Fract.", "2005", "2010", "2015", "sigma")


coef_df <- function(model, n_max) {
    value <- c()
    upper <- c()
    lower <- c()
    for (i in 1:n_max) {
        value[i] <- precis(model)@.Data[[1]][i]
        upper[i] <- precis(model)@.Data[[1]][i] + precis(model)@.Data[[2]][i]
        lower[i] <- precis(model)@.Data[[1]][i] - precis(model)@.Data[[2]][i]
    }
    value <- value[-1] # remove intercept
    upper <- upper[-1]
    lower <- lower[-1]
    return(data.frame(cbind(value, upper, lower)))
}

total_violence_add_df <- coef_df(total_violence_add_inform_sd, 15)
osv_add_df <- coef_df(osv_add_inform_sd, 15)
brd_add_df <- coef_df(brd_add_inform_sd, 15)


total_violence_add_plot <- coef_plots(total_violence_add_df,
                                      title = "Total Violence", N = "681 ",
                                      WAIC = "4083.33")

osv_add_plot <- coef_plots(osv_add_df, title = "One-Sided Violence", N = "681 ",
                           WAIC = "4083.44") +
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())

brd_add_plot <- coef_plots(brd_add_df, title = "Battle-Related Deaths",
                           N = "681 ", WAIC = "4083.89") +
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())

total_violence_add_plot + osv_add_plot + brd_add_plot

ggsave("figs/coef_plots/additive_coef_plot.png", height = 4, width = 12)


# Interation models ------------------------------------------------------------
coef_plots <- function(data, title, N, WAIC) {
    capt <- paste0(paste0("N = ", N, "WAIC = ", WAIC,
        sep = " "))
    ggplot(data, aes(x = variable, y = value, ymin = lower, ymax = upper)) +
    geom_pointrange(size = 0.125) +
    geom_hline(yintercept = 0, linetype = 2) +
    coord_flip() +
    labs(title = title, caption = capt) +
    ylab("") +
    scale_x_discrete(name = "",
        limits = c("sigma", "2015", "2010", "2005", "Ethnic Fract.", "x-Polity",
        "Tropical", "Income Inequality", "Urban Growth", "Education",
        "Health Exp", "Contig. CW", "PKO x Violence", "Violence", "PKO")) +
        theme(text = element_text(family = "serif"))
}

variable <- c("PKO", "Violence", "PKO x Violence", "Contig. CW", "Health Exp",
    "Education", "Urban Growth", "Income Inequality", "Tropical", "x-Polity",
    "Ethnic Fract.", "2005", "2010", "2015", "sigma")



total_violence_int_df <- coef_df(total_violence_int_inform_sd, 16)
ovs_int_df <- coef_df(osv_int_inform_sd, 16)
brd_int_df <- coef_df(brd_int_inform_sd, 16)


total_violence_int_plot <- coef_plots(total_violence_int_df,
                                      title = "Total Violence", N = "681  ",
                                      WAIC = "4081.23")

ovs_int_plot <- coef_plots(ovs_int_df, title = "One-Sided Violence",
                           N = "681  ", WAIC = "4081.77") +
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())

brd_int_plot <- coef_plots(brd_int_df, title = "Battle-Related Deaths",
    N = "681  ",  WAIC = "4085.89") +
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())

total_violence_int_plot + ovs_int_plot + brd_int_plot

ggsave("figs/coef_plots/interation_coef_plot.png", height = 4, width = 12)
