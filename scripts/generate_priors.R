
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


# Additive models (unstandardized) ---------------------------------------------
m15_44B_a_r <- c(ma_r[2], ma_r[3], ma_r[4], ma_r[5], ma_r[6], ma_r[7], ma_r[8],
    ma_r[9], ma_r[10], ma_r[11])
m15_44se_a_r <- c(ma_r[13], ma_r[14], ma_r[15], ma_r[16], ma_r[17], ma_r[18],
    ma_r[19], ma_r[20], ma_r[21], ma_r[22])
m15_44sd_a_r <- sapply(m15_44se_a, function(m15_44se_a) sqrt(178) * m15_44se_a)

f15_44B_a_r <- c(fa_r[2], fa_r[3], fa_r[4], fa_r[5], fa_r[6], fa_r[7], fa_r[8],
    fa_r[9], fa_r[10], fa_r[11])
f15_44se_a_r <- c(fa_r[13], fa_r[14], fa_r[15], fa_r[16], fa_r[17], fa_r[18],
    fa_r[19], fa_r[20], fa_r[21], fa_r[22])
f15_44sd_a_r <- sapply(f15_44se_a_r, function(f15_44se_a_r) sqrt(178) * f15_44se_a_r)


a15_44B_r <- data.frame(cbind(m15_44B_a_r, f15_44B_a_r, var_names_a))
a15_44B_r$mean_B <- (as.numeric(a15_44B_r$m15_44B_a_r) +
    as.numeric(a15_44B_r$f15_44B_a_r)) / -2
View(a15_44B_r)

a15_44sd <- data.frame(cbind(m15_44sd_a, f15_44sd_a, var_names_a))
a15_44sd$mean_sd <- (as.numeric(a15_44sd$m15_44sd_a) +
    as.numeric(a15_44sd$f15_44sd_a)) / 2
View(a15_44sd)

# Interactive models (unstandardized) ------------------------------------------
m15_44B_i <- c(mi_r[2], mi_r[3], mi_r[4], mi_r[5], mi_r[6], mi_r[7], mi_r[8],
    mi_r[9], mi_r[10], mi_r[11], mi_r[12])
m15_44se_i <- c(mi_r[14], mi_r[15], mi_r[16], mi_r[17], mi_r[18], mi_r[19],
    mi_r[20], mi_r[21], mi_r[22], mi_r[23], mi_r[24])
m15_44sd_i <- sapply(m15_44se_i, function(m15_44se_i) sqrt(178) * m15_44se_i)

f15_44B_i <- c(fi_r[2], fi_r[3], fi_r[4], fi_r[5], fi_r[6], fi_r[7], fi_r[8],
    fi_r[9], fi_r[10], fi_r[11], fi_r[12])
f15_44se_i <- c(fi_r[14], fi_r[15], fi_r[16], fi_r[17], fi_r[18], fi_r[19],
    fi_r[20], fi_r[21], fi_r[22], fi_r[23], fi_r[24])
f15_44sd_i <- sapply(f15_44se_i, function(f15_44se_i) sqrt(178) * f15_44se_i)


i15_44B <- data.frame(cbind(m15_44B_i, f15_44B_i, var_names_i))
i15_44B$mean_B <- (as.numeric(i15_44B$m15_44B_i) +
    as.numeric(i15_44B$f15_44B_i)) / -2
View(i15_44B)

i15_44sd <- data.frame(cbind(m15_44sd_i, f15_44sd_i, var_names_i))
i15_44sd$mean_sd <- (as.numeric(i15_44sd$m15_44sd_i) +
    as.numeric(i15_44sd$f15_44sd_i)) / 2
View(i15_44sd)
