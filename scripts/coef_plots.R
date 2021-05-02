

# combines plots --------------------------------------------------------------
library(ggplot2)
library(patchwork)
theme_set(theme_bw())
coef_plots <- function (data, title) {
    ggplot(data, aes(x = variable, y = values, ymin = lower, ymax = upper)) +
    geom_pointrange() +
    geom_hline(yintercept = 0, linetype = 2) +
    coord_flip() +
    labs(title = title) +
    ylab("") +
    scale_x_discrete(name ="",
        limits=c("Ethnic Fract.","x-Polity", "Tropical", "Gini",
        "Urban Growth", "Education", "Health Exp","Cont.CW", "Violence", "PKO"))
}

# Additive models -------------------------------------------------------------
variable <- c("PKO", "Violence", "Cont.CW", "Health Exp",
    "Education", "Urban Growth", "Gini", "Tropical", "x-Polity",
    "Ethnic Fract.")

# Total violence.
values <- c(precis(T2_M1s)@.Data[[1]][2],
    precis(T2_M1s)@.Data[[1]][3], precis(T2_M1s)@.Data[[1]][4],
    precis(T2_M1s)@.Data[[1]][5], precis(T2_M1s)@.Data[[1]][6],
    precis(T2_M1s)@.Data[[1]][7], precis(T2_M1s)@.Data[[1]][8],
    precis(T2_M1s)@.Data[[1]][9], precis(T2_M1s)@.Data[[1]][10],
    precis(T2_M1s)@.Data[[1]][11])

lower <- c(precis(T2_M1s)@.Data[[1]][2] - precis(T2_M1s)@.Data[[2]][2],
    precis(T2_M1s)@.Data[[1]][3] - precis(T2_M1s)@.Data[[2]][3],
    precis(T2_M1s)@.Data[[1]][4] - precis(T2_M1s)@.Data[[2]][4],
    precis(T2_M1s)@.Data[[1]][5] - precis(T2_M1s)@.Data[[2]][5],
    precis(T2_M1s)@.Data[[1]][6] - precis(T2_M1s)@.Data[[2]][6],
    precis(T2_M1s)@.Data[[1]][7] - precis(T2_M1s)@.Data[[2]][7],
    precis(T2_M1s)@.Data[[1]][8] - precis(T2_M1s)@.Data[[2]][8],
    precis(T2_M1s)@.Data[[1]][9] - precis(T2_M1s)@.Data[[2]][9],
    precis(T2_M1s)@.Data[[1]][10] - precis(T2_M1s)@.Data[[2]][10],
    precis(T2_M1s)@.Data[[1]][11] - precis(T2_M1s)@.Data[[2]][11])

upper <- c(precis(T2_M1s)@.Data[[1]][2] + precis(T2_M1s)@.Data[[2]][2],
    precis(T2_M1s)@.Data[[1]][3] + precis(T2_M1s)@.Data[[2]][3],
    precis(T2_M1s)@.Data[[1]][4] + precis(T2_M1s)@.Data[[2]][4],
    precis(T2_M1s)@.Data[[1]][5] + precis(T2_M1s)@.Data[[2]][5],
    precis(T2_M1s)@.Data[[1]][6] + precis(T2_M1s)@.Data[[2]][6],
    precis(T2_M1s)@.Data[[1]][7] + precis(T2_M1s)@.Data[[2]][7],
    precis(T2_M1s)@.Data[[1]][8] + precis(T2_M1s)@.Data[[2]][8],
    precis(T2_M1s)@.Data[[1]][9] + precis(T2_M1s)@.Data[[2]][9],
    precis(T2_M1s)@.Data[[1]][10] + precis(T2_M1s)@.Data[[2]][10],
    precis(T2_M1s)@.Data[[1]][11] + precis(T2_M1s)@.Data[[2]][11])

T2_M1df <- data.frame(variable, values, upper, lower)

# One-sided violence
values <- c(precis(T3_M1s)@.Data[[1]][2],
    precis(T3_M1s)@.Data[[1]][3], precis(T3_M1s)@.Data[[1]][4],
    precis(T3_M1s)@.Data[[1]][5], precis(T3_M1s)@.Data[[1]][6],
    precis(T3_M1s)@.Data[[1]][7], precis(T3_M1s)@.Data[[1]][8],
    precis(T3_M1s)@.Data[[1]][9], precis(T3_M1s)@.Data[[1]][10],
    precis(T3_M1s)@.Data[[1]][11])

lower <- c(precis(T3_M1s)@.Data[[1]][2] - precis(T3_M1s)@.Data[[2]][2],
    precis(T3_M1s)@.Data[[1]][3] - precis(T3_M1s)@.Data[[2]][3],
    precis(T3_M1s)@.Data[[1]][4] - precis(T3_M1s)@.Data[[2]][4],
    precis(T3_M1s)@.Data[[1]][5] - precis(T3_M1s)@.Data[[2]][5],
    precis(T3_M1s)@.Data[[1]][6] - precis(T3_M1s)@.Data[[2]][6],
    precis(T3_M1s)@.Data[[1]][7] - precis(T3_M1s)@.Data[[2]][7],
    precis(T3_M1s)@.Data[[1]][8] - precis(T3_M1s)@.Data[[2]][8],
    precis(T3_M1s)@.Data[[1]][9] - precis(T3_M1s)@.Data[[2]][9],
    precis(T3_M1s)@.Data[[1]][10] - precis(T3_M1s)@.Data[[2]][10],
    precis(T3_M1s)@.Data[[1]][11] - precis(T3_M1s)@.Data[[2]][11])

upper <- c(precis(T3_M1s)@.Data[[1]][2] + precis(T3_M1s)@.Data[[2]][2],
    precis(T3_M1s)@.Data[[1]][3] + precis(T3_M1s)@.Data[[2]][3],
    precis(T3_M1s)@.Data[[1]][4] + precis(T3_M1s)@.Data[[2]][4],
    precis(T3_M1s)@.Data[[1]][5] + precis(T3_M1s)@.Data[[2]][5],
    precis(T3_M1s)@.Data[[1]][6] + precis(T3_M1s)@.Data[[2]][6],
    precis(T3_M1s)@.Data[[1]][7] + precis(T3_M1s)@.Data[[2]][7],
    precis(T3_M1s)@.Data[[1]][8] + precis(T3_M1s)@.Data[[2]][8],
    precis(T3_M1s)@.Data[[1]][9] + precis(T3_M1s)@.Data[[2]][9],
    precis(T3_M1s)@.Data[[1]][10] + precis(T3_M1s)@.Data[[2]][10],
    precis(T3_M1s)@.Data[[1]][11] + precis(T3_M1s)@.Data[[2]][11])

T3_M1df <- data.frame(variable, values, upper, lower)

# Battle-related deaths
values <- c(precis(T4_M1s)@.Data[[1]][2],
    precis(T4_M1s)@.Data[[1]][3], precis(T4_M1s)@.Data[[1]][4],
    precis(T4_M1s)@.Data[[1]][5], precis(T4_M1s)@.Data[[1]][6],
    precis(T4_M1s)@.Data[[1]][7], precis(T4_M1s)@.Data[[1]][8],
    precis(T4_M1s)@.Data[[1]][9], precis(T4_M1s)@.Data[[1]][10],
    precis(T4_M1s)@.Data[[1]][11])

lower <- c(precis(T4_M1s)@.Data[[1]][2] - precis(T4_M1s)@.Data[[2]][2],
    precis(T4_M1s)@.Data[[1]][3] - precis(T4_M1s)@.Data[[2]][3],
    precis(T4_M1s)@.Data[[1]][4] - precis(T4_M1s)@.Data[[2]][4],
    precis(T4_M1s)@.Data[[1]][5] - precis(T4_M1s)@.Data[[2]][5],
    precis(T4_M1s)@.Data[[1]][6] - precis(T4_M1s)@.Data[[2]][6],
    precis(T4_M1s)@.Data[[1]][7] - precis(T4_M1s)@.Data[[2]][7],
    precis(T4_M1s)@.Data[[1]][8] - precis(T4_M1s)@.Data[[2]][8],
    precis(T4_M1s)@.Data[[1]][9] - precis(T4_M1s)@.Data[[2]][9],
    precis(T4_M1s)@.Data[[1]][10] - precis(T4_M1s)@.Data[[2]][10],
    precis(T4_M1s)@.Data[[1]][11] - precis(T4_M1s)@.Data[[2]][11])

upper <- c(precis(T4_M1s)@.Data[[1]][2] + precis(T4_M1s)@.Data[[2]][2],
    precis(T4_M1s)@.Data[[1]][3] + precis(T4_M1s)@.Data[[2]][3],
    precis(T4_M1s)@.Data[[1]][4] + precis(T4_M1s)@.Data[[2]][4],
    precis(T4_M1s)@.Data[[1]][5] + precis(T4_M1s)@.Data[[2]][5],
    precis(T4_M1s)@.Data[[1]][6] + precis(T4_M1s)@.Data[[2]][6],
    precis(T4_M1s)@.Data[[1]][7] + precis(T4_M1s)@.Data[[2]][7],
    precis(T4_M1s)@.Data[[1]][8] + precis(T4_M1s)@.Data[[2]][8],
    precis(T4_M1s)@.Data[[1]][9] + precis(T4_M1s)@.Data[[2]][9],
    precis(T4_M1s)@.Data[[1]][10] + precis(T4_M1s)@.Data[[2]][10],
    precis(T4_M1s)@.Data[[1]][11] + precis(T4_M1s)@.Data[[2]][11])

T4_M1df <- data.frame(variable, values, upper, lower)


T2_M1plot <- coef_plots(T2_M1df, title = "Total Violence")
T3_M1plot <- coef_plots(T3_M1df, title = "One-sided Violence")
T4_M1plot <- coef_plots(T4_M1df, title = "Battle-related Deaths")
T2_M1plot + T3_M1plot + T4_M1plot
ggsave("figs/additive_coef_plot.png", height = 3, width = 12)

# Interation models ------------------------------------------------------------
coef_plots <- function (data, title) {
    ggplot(data, aes(x = variable, y = values, ymin = lower, ymax = upper)) +
    geom_pointrange() +
    geom_hline(yintercept = 0, linetype = 2) +
    coord_flip() +
    labs(title = title) +
    ylab("") +
    scale_x_discrete(name ="",
        limits=c("Ethnic Fract.","x-Polity", "Tropical", "Gini",
        "Urban Growth", "Education", "Health Exp","Cont.CW", "PKO x Violence",
        "Violence", "PKO"))
}

variable <- c("PKO", "Violence", "PKO x Violence", "Cont.CW", "Health Exp",
    "Education", "Urban Growth", "Gini", "Tropical", "x-Polity",
    "Ethnic Fract.")

# Total violence.
values <- c(precis(T2_M2s)@.Data[[1]][2],
    precis(T2_M2s)@.Data[[1]][3], precis(T2_M2s)@.Data[[1]][4],
    precis(T2_M2s)@.Data[[1]][5], precis(T2_M2s)@.Data[[1]][6],
    precis(T2_M2s)@.Data[[1]][7], precis(T2_M2s)@.Data[[1]][8],
    precis(T2_M2s)@.Data[[1]][9], precis(T2_M2s)@.Data[[1]][10],
    precis(T2_M2s)@.Data[[1]][11], precis(T2_M2s)@.Data[[1]][12])

lower <- c(precis(T2_M2s)@.Data[[1]][2] - precis(T2_M2s)@.Data[[2]][2],
    precis(T2_M2s)@.Data[[1]][3] - precis(T2_M2s)@.Data[[2]][3],
    precis(T2_M2s)@.Data[[1]][4] - precis(T2_M2s)@.Data[[2]][4],
    precis(T2_M2s)@.Data[[1]][5] - precis(T2_M2s)@.Data[[2]][5],
    precis(T2_M2s)@.Data[[1]][6] - precis(T2_M2s)@.Data[[2]][6],
    precis(T2_M2s)@.Data[[1]][7] - precis(T2_M2s)@.Data[[2]][7],
    precis(T2_M2s)@.Data[[1]][8] - precis(T2_M2s)@.Data[[2]][8],
    precis(T2_M2s)@.Data[[1]][9] - precis(T2_M2s)@.Data[[2]][9],
    precis(T2_M2s)@.Data[[1]][10] - precis(T2_M2s)@.Data[[2]][10],
    precis(T2_M2s)@.Data[[1]][11] - precis(T2_M2s)@.Data[[2]][11],
    precis(T2_M2s)@.Data[[1]][12] - precis(T2_M2s)@.Data[[2]][12])

upper <- c(precis(T2_M2s)@.Data[[1]][2] + precis(T2_M2s)@.Data[[2]][2],
    precis(T2_M2s)@.Data[[1]][3] + precis(T2_M2s)@.Data[[2]][3],
    precis(T2_M2s)@.Data[[1]][4] + precis(T2_M2s)@.Data[[2]][4],
    precis(T2_M2s)@.Data[[1]][5] + precis(T2_M2s)@.Data[[2]][5],
    precis(T2_M2s)@.Data[[1]][6] + precis(T2_M2s)@.Data[[2]][6],
    precis(T2_M2s)@.Data[[1]][7] + precis(T2_M2s)@.Data[[2]][7],
    precis(T2_M2s)@.Data[[1]][8] + precis(T2_M2s)@.Data[[2]][8],
    precis(T2_M2s)@.Data[[1]][9] + precis(T2_M2s)@.Data[[2]][9],
    precis(T2_M2s)@.Data[[1]][10] + precis(T2_M2s)@.Data[[2]][10],
    precis(T2_M2s)@.Data[[1]][11] + precis(T2_M2s)@.Data[[2]][11],
    precis(T2_M2s)@.Data[[1]][12] + precis(T2_M2s)@.Data[[2]][12])

T2_M2df <- data.frame(variable, values, upper, lower)

# One-sided violence
values <- c(precis(T3_M2s)@.Data[[1]][2],
    precis(T3_M2s)@.Data[[1]][3], precis(T3_M2s)@.Data[[1]][4],
    precis(T3_M2s)@.Data[[1]][5], precis(T3_M2s)@.Data[[1]][6],
    precis(T3_M2s)@.Data[[1]][7], precis(T3_M2s)@.Data[[1]][8],
    precis(T3_M2s)@.Data[[1]][9], precis(T3_M2s)@.Data[[1]][10],
    precis(T3_M2s)@.Data[[1]][11], precis(T3_M2s)@.Data[[1]][12])

lower <- c(precis(T3_M2s)@.Data[[1]][2] - precis(T3_M2s)@.Data[[2]][2],
    precis(T3_M2s)@.Data[[1]][3] - precis(T3_M2s)@.Data[[2]][3],
    precis(T3_M2s)@.Data[[1]][4] - precis(T3_M2s)@.Data[[2]][4],
    precis(T3_M2s)@.Data[[1]][5] - precis(T3_M2s)@.Data[[2]][5],
    precis(T3_M2s)@.Data[[1]][6] - precis(T3_M2s)@.Data[[2]][6],
    precis(T3_M2s)@.Data[[1]][7] - precis(T3_M2s)@.Data[[2]][7],
    precis(T3_M2s)@.Data[[1]][8] - precis(T3_M2s)@.Data[[2]][8],
    precis(T3_M2s)@.Data[[1]][9] - precis(T3_M2s)@.Data[[2]][9],
    precis(T3_M2s)@.Data[[1]][10] - precis(T3_M2s)@.Data[[2]][10],
    precis(T3_M2s)@.Data[[1]][11] - precis(T3_M2s)@.Data[[2]][11],
    precis(T3_M2s)@.Data[[1]][12] - precis(T3_M2s)@.Data[[2]][12])

upper <- c(precis(T3_M2s)@.Data[[1]][2] + precis(T3_M2s)@.Data[[2]][2],
    precis(T3_M2s)@.Data[[1]][3] + precis(T3_M2s)@.Data[[2]][3],
    precis(T3_M2s)@.Data[[1]][4] + precis(T3_M2s)@.Data[[2]][4],
    precis(T3_M2s)@.Data[[1]][5] + precis(T3_M2s)@.Data[[2]][5],
    precis(T3_M2s)@.Data[[1]][6] + precis(T3_M2s)@.Data[[2]][6],
    precis(T3_M2s)@.Data[[1]][7] + precis(T3_M2s)@.Data[[2]][7],
    precis(T3_M2s)@.Data[[1]][8] + precis(T3_M2s)@.Data[[2]][8],
    precis(T3_M2s)@.Data[[1]][9] + precis(T3_M2s)@.Data[[2]][9],
    precis(T3_M2s)@.Data[[1]][10] + precis(T3_M2s)@.Data[[2]][10],
    precis(T3_M2s)@.Data[[1]][11] + precis(T3_M2s)@.Data[[2]][11],
    precis(T3_M2s)@.Data[[1]][12] + precis(T3_M2s)@.Data[[2]][12])

T3_M2df <- data.frame(variable, values, upper, lower)

# Battle-related deaths
values <- c(precis(T4_M2s)@.Data[[1]][2],
    precis(T4_M2s)@.Data[[1]][3], precis(T4_M2s)@.Data[[1]][4],
    precis(T4_M2s)@.Data[[1]][5], precis(T4_M2s)@.Data[[1]][6],
    precis(T4_M2s)@.Data[[1]][7], precis(T4_M2s)@.Data[[1]][8],
    precis(T4_M2s)@.Data[[1]][9], precis(T4_M2s)@.Data[[1]][10],
    precis(T4_M2s)@.Data[[1]][11], precis(T4_M2s)@.Data[[1]][12])

lower <- c(precis(T4_M2s)@.Data[[1]][2] - precis(T4_M2s)@.Data[[2]][2],
    precis(T4_M2s)@.Data[[1]][3] - precis(T4_M2s)@.Data[[2]][3],
    precis(T4_M2s)@.Data[[1]][4] - precis(T4_M2s)@.Data[[2]][4],
    precis(T4_M2s)@.Data[[1]][5] - precis(T4_M2s)@.Data[[2]][5],
    precis(T4_M2s)@.Data[[1]][6] - precis(T4_M2s)@.Data[[2]][6],
    precis(T4_M2s)@.Data[[1]][7] - precis(T4_M2s)@.Data[[2]][7],
    precis(T4_M2s)@.Data[[1]][8] - precis(T4_M2s)@.Data[[2]][8],
    precis(T4_M2s)@.Data[[1]][9] - precis(T4_M2s)@.Data[[2]][9],
    precis(T4_M2s)@.Data[[1]][10] - precis(T4_M2s)@.Data[[2]][10],
    precis(T4_M2s)@.Data[[1]][11] - precis(T4_M2s)@.Data[[2]][11],
    precis(T4_M2s)@.Data[[1]][12] - precis(T4_M2s)@.Data[[2]][12])

upper <- c(precis(T4_M2s)@.Data[[1]][2] + precis(T4_M2s)@.Data[[2]][2],
    precis(T4_M2s)@.Data[[1]][3] + precis(T4_M2s)@.Data[[2]][3],
    precis(T4_M2s)@.Data[[1]][4] + precis(T4_M2s)@.Data[[2]][4],
    precis(T4_M2s)@.Data[[1]][5] + precis(T4_M2s)@.Data[[2]][5],
    precis(T4_M2s)@.Data[[1]][6] + precis(T4_M2s)@.Data[[2]][6],
    precis(T4_M2s)@.Data[[1]][7] + precis(T4_M2s)@.Data[[2]][7],
    precis(T4_M2s)@.Data[[1]][8] + precis(T4_M2s)@.Data[[2]][8],
    precis(T4_M2s)@.Data[[1]][9] + precis(T4_M2s)@.Data[[2]][9],
    precis(T4_M2s)@.Data[[1]][10] + precis(T4_M2s)@.Data[[2]][10],
    precis(T4_M2s)@.Data[[1]][11] + precis(T4_M2s)@.Data[[2]][11],
    precis(T4_M2s)@.Data[[1]][12] + precis(T4_M2s)@.Data[[2]][12])

T4_M2df <- data.frame(variable, values, upper, lower)


T2_M2plot <- coef_plots(T2_M2df, title = "Total Violence")
T3_M2plot <- coef_plots(T3_M2df, title = "One-sided Violence")
T4_M2plot <- coef_plots(T4_M2df, title = "Battle-related Deaths")
T2_M2plot + T3_M2plot + T4_M2plot
ggsave("figs/interation_coef_plot.png", height = 3, width = 12)
