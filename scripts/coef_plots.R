################################################################################
#' This script produces the coeffecient plots seen in figures 3 and 4, and saves
#' the .png files as "additive_coef_plot.png" (figure 3) and
#' "interation_coef_plot.png" (figure 4) in the "figs" folder.
################################################################################

#library(ggplot2)
#library(patchwork)
#library(rethinking)

# Additive models -------------------------------------------------------------
theme_set(theme_bw())
coef_plots <- function (data, title) {
    ggplot(data, aes(x = variable, y = value, ymin = lower, ymax = upper)) +
    geom_pointrange(size = 0.125) +
    geom_hline(yintercept = 0, linetype = 2) +
    coord_flip() +
    labs(title = title) +
    ylab("") +
    scale_x_discrete(name ="",
        limits=c("sigma", "2015", "2010", "2005", "Ethnic Fract.", "x-Polity",
        "Tropical", "Gini", "Urban Growth", "Education",
        "Health Exp","Cont.CW", "Violence", "PKO"))
}

variable <- c("PKO", "Violence", "Cont.CW", "Health Exp",
    "Education", "Urban Growth", "Gini", "Tropical", "x-Polity",
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

T2_M1s_df <- coef_df(T2_M1s, 15) # Total violence
T3_M1s_df <- coef_df(T3_M1s, 15) # One-sided violence
T4_M1s_df <- coef_df(T4_M1s, 15) # Battle-related deaths


T2_M1plot <- coef_plots(T2_M1s_df, title = "Total Violence")
T3_M1plot <- coef_plots(T3_M1s_df, title = "One-sided Violence")
T4_M1plot <- coef_plots(T4_M1s_df, title = "Battle-related Deaths")
T2_M1plot + T3_M1plot + T4_M1plot

ggsave("figs/additive_coef_plot.png", height = 3, width = 12)


# Interation models ------------------------------------------------------------
coef_plots <- function (data, title) {
    ggplot(data, aes(x = variable, y = value, ymin = lower, ymax = upper)) +
    geom_pointrange(size = 0.125) +
    geom_hline(yintercept = 0, linetype = 2) +
    coord_flip() +
    labs(title = title) +
    ylab("") +
    scale_x_discrete(name ="",
        limits=c("sigma", "2015", "2010", "2005", "Ethnic Fract.","x-Polity",
        "Tropical", "Gini", "Urban Growth", "Education",
        "Health Exp","Cont.CW", "PKO x Violence", "Violence", "PKO"))
}

variable <- c("PKO", "Violence", "PKO x Violence", "Cont.CW", "Health Exp",
    "Education", "Urban Growth", "Gini", "Tropical", "x-Polity",
    "Ethnic Fract.", "2005", "2010", "2015", "sigma")



T2_M2s_df <- coef_df(T2_M2s, 16) # Total violence
T3_M2s_df <- coef_df(T3_M2s, 16) # One-sided violence
T4_M2s_df <- coef_df(T4_M2s, 16) # Battle-related deaths


T2_M2plot <- coef_plots(T2_M2s_df, title = "Total Violence")
T3_M2plot <- coef_plots(T3_M2s_df, title = "One-sided Violence")
T4_M2plot <- coef_plots(T4_M2s_df, title = "Battle-related Deaths")
T2_M2plot + T3_M2plot + T4_M2plot

ggsave("figs/interation_coef_plot.png", height = 3, width = 12)
