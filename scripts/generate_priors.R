
# Additive models (standardized) -----------------------------------------------
var_names_a <- c("PKO years", "Death", "Contig. CW", "Health Exp", "Education",
    "Urban Growth", "Gini", "Tropical", "Polity", "Ethnic Fract.")

m15_44B_a <- c(ma[2], ma[3], ma[4], ma[5], ma[6], ma[7], ma[8], ma[9], ma[10],
    ma[11])
m15_44se_a <- c(ma[13], ma[14], ma[15], ma[16], ma[17], ma[18], ma[19], ma[20],
    ma[21], ma[22])
m15_44sd_a <- sapply(m15_44se_a, function(m15_44se_a) sqrt(178) * m15_44se_a)

f15_44B_a <- c(fa[2], fa[3], fa[4], fa[5], fa[6], fa[7], fa[8], fa[9], fa[10],
    fa[11])
f15_44se_a <- c(fa[13], fa[14], fa[15], fa[16], fa[17], fa[18], fa[19], fa[20],
    fa[21], fa[22])
f15_44sd_a <- sapply(f15_44se_a, function(f15_44se_a) sqrt(178) * f15_44se_a)
#a15_44se_a <- c(0.515, 0.141, 3.25, 2.215, 4.56, 3.065, 26.94, 4.35, 0.28, 1.35)

a15_44B <- data.frame(cbind(m15_44B_a, f15_44B_a, var_names_a))
a15_44B$mean_B <- (as.numeric(a15_44B$m15_44B_a) +
    as.numeric(a15_44B$f15_44B_a)) / -2
View(a15_44B)

a15_44sd <- data.frame(cbind(m15_44sd_a, f15_44sd_a, var_names_a))
a15_44sd$mean_sd <- (as.numeric(a15_44sd$m15_44sd_a) +
    as.numeric(a15_44sd$f15_44sd_a)) / 2
View(a15_44sd)

# Interactive models (standardized) --------------------------------------------
var_names_i <- c("PKO years", "Death", "PKO x Death", "Contig. CW",
    "Health Exp", "Education", "Urban Growth", "Gini", "Tropical", "Polity",
    "Ethnic Fract.")

m15_44B_i <- c(mi[2], mi[3], mi[4], mi[5], mi[6], mi[7], mi[8], mi[9], mi[10],
    mi[11], mi[12])
m15_44se_i <- c(mi[14], mi[15], mi[16], mi[17], mi[18], mi[19], mi[20],
    mi[21], mi[22], mi[23], mi[24])
m15_44sd_i <- sapply(m15_44se_i, function(m15_44se_i) sqrt(178) * m15_44se_i)

f15_44B_i <- c(fi[2], fi[3], fi[4], fi[5], fi[6], fi[7], fi[8], fi[9], fi[10],
    fi[11], fi[12])
f15_44se_i <- c(fi[14], fi[15], fi[16], fi[17], fi[18], fi[19], fi[20],
    fi[21], fi[22], fi[23], fi[24])
f15_44sd_i <- sapply(f15_44se_i, function(f15_44se_i) sqrt(178) * f15_44se_i)
#i15_44se_i <- c(0.515, 0.141, 3.25, 2.215, 4.56, 3.065, 26.94, 4.35, 0.28, 1.35)

i15_44B <- data.frame(cbind(m15_44B_i, f15_44B_i, var_names_i))
i15_44B$mean_B <- (as.numeric(i15_44B$m15_44B_i) +
    as.numeric(i15_44B$f15_44B_i)) / -2
View(i15_44B)

i15_44sd <- data.frame(cbind(m15_44sd_i, f15_44sd_i, var_names_i))
i15_44sd$mean_sd <- (as.numeric(i15_44sd$m15_44sd_i) +
    as.numeric(i15_44sd$f15_44sd_i)) / 2
View(i15_44sd)


# Interactive models (untransformed) ---------------------------------------
# Calculating the average standard deviation of the posterior.
m15_44se_i <- c(0.52, 0.33, 0.08, 2.76, 1.87, 3.70, 2.57, 22.83, 3.78,
    0.24, 1.21)
f15_44se_i <- c(0.60, 0.36, 0.09, 3.77, 2.62, 5.46, 3.58, 31.63, 4.96,
    0.32, 1.50)

m15_44sd_i <- sapply(m15_44se_i, function(m15_44se_i) sqrt(178) * m15_44se_i)
f15_44sd_i <- sapply(f15_44se_i, function(f15_44se_i) sqrt(178) * f15_44se_i)

a15_44sd_i <- data.frame(cbind(m15_44sd_i, f15_44sd_i))
a15_44sd_i$mean_sd <- (a15_44sd_i$m15_44sd_i + a15_44sd_i$f15_44sd_i) / 2
View(a15_44sd_i)

# Calculating the average coeffecient and reversing the direction.
m15_44B_i <- c(-1.43, 0.46, -0.04, 7.83, -2.31, -3.62, 5.92, 53.33, 3.45,
    0.22, 0.95)
f15_44B_i <- c(-1.67, 0.65, -0.06, 12.50, -1.76, -7.24, 8.54, 51.77, 2.93,
    0.05, 0.99)

a15_44B_i <- data.frame(cbind(m15_44B_i, f15_44B_i))
a15_44B_i$mean_B <- (a15_44B_i$m15_44B_i + a15_44B_i$f15_44B_i) / -2
View(a15_44B_i)
