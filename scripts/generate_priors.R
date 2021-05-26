
# Additive models (standardized) -----------------------------------------------
var_names_a <- c("PKO years", "Death", "Contig. CW", "Health Exp", "Education",
    "Urban Growth", "Gini", "Tropical", "Polity", "Ethnic Fract.")

mB_a <- c(ma[2], ma[3], ma[4], ma[5], ma[6], ma[7], ma[8], ma[9], ma[10],
    ma[11])
mse_a <- c(ma[13], ma[14], ma[15], ma[16], ma[17], ma[18], ma[19], ma[20],
    ma[21], ma[22])
msd_a <- sapply(mse_a, function(mse_a) sqrt(178) * mse_a)

fB_a <- c(fa[2], fa[3], fa[4], fa[5], fa[6], fa[7], fa[8], fa[9], fa[10],
    fa[11])
fse_a <- c(fa[13], fa[14], fa[15], fa[16], fa[17], fa[18], fa[19], fa[20],
    fa[21], fa[22])
fsd_a <- sapply(fse_a, function(fse_a) sqrt(178) * fse_a)
#ase_a <- c(0.515, 0.141, 3.25, 2.215, 4.56, 3.065, 26.94, 4.35, 0.28, 1.35)

aB <- data.frame(cbind(mB_a, fB_a, var_names_a))
aB$mean_B <- (as.numeric(aB$mB_a) +
    as.numeric(aB$fB_a)) / -2
#View(aB)

asd <- data.frame(cbind(msd_a, fsd_a, var_names_a))
asd$mean_sd <- (as.numeric(asd$msd_a) +
    as.numeric(asd$fsd_a)) / 2
#View(asd)

# Interactive models (standardized) --------------------------------------------
var_names_i <- c("PKO years", "Death", "PKO x Death", "Contig. CW",
    "Health Exp", "Education", "Urban Growth", "Gini", "Tropical", "Polity",
    "Ethnic Fract.")

mB_i <- c(mi[2], mi[3], mi[4], mi[5], mi[6], mi[7], mi[8], mi[9], mi[10],
    mi[11], mi[12])
mse_i <- c(mi[14], mi[15], mi[16], mi[17], mi[18], mi[19], mi[20],
    mi[21], mi[22], mi[23], mi[24])
msd_i <- sapply(mse_i, function(mse_i) sqrt(178) * mse_i)

fB_i <- c(fi[2], fi[3], fi[4], fi[5], fi[6], fi[7], fi[8], fi[9], fi[10],
    fi[11], fi[12])
fse_i <- c(fi[14], fi[15], fi[16], fi[17], fi[18], fi[19], fi[20],
    fi[21], fi[22], fi[23], fi[24])
fsd_i <- sapply(fse_i, function(fse_i) sqrt(178) * fse_i)
#ise_i <- c(0.515, 0.141, 3.25, 2.215, 4.56, 3.065, 26.94, 4.35, 0.28, 1.35)

iB <- data.frame(cbind(mB_i, fB_i, var_names_i))
iB$mean_B <- (as.numeric(iB$mB_i) +
    as.numeric(iB$fB_i)) / -2
#View(iB)

isd <- data.frame(cbind(msd_i, fsd_i, var_names_i))
isd$mean_sd <- (as.numeric(isd$msd_i) +
    as.numeric(isd$fsd_i)) / 2
#View(isd)

ise <- data.frame(cbind(mse_i, fse_i, var_names_i))
ise$mean_se <- (as.numeric(ise$mse_i) +
    as.numeric(ise$fse_i)) / 2
View(ise)

# Additive models (unstandardized) ---------------------------------------------
mB_a_r <- c(ma_r[2], ma_r[3], ma_r[4], ma_r[5], ma_r[6], ma_r[7], ma_r[8],
    ma_r[9], ma_r[10], ma_r[11])
mse_a_r <- c(ma_r[13], ma_r[14], ma_r[15], ma_r[16], ma_r[17], ma_r[18],
    ma_r[19], ma_r[20], ma_r[21], ma_r[22])
msd_a_r <- sapply(mse_a_r, function(mse_a_r) sqrt(178) * mse_a_r)

fB_a_r <- c(fa_r[2], fa_r[3], fa_r[4], fa_r[5], fa_r[6], fa_r[7], fa_r[8],
    fa_r[9], fa_r[10], fa_r[11])
fse_a_r <- c(fa_r[13], fa_r[14], fa_r[15], fa_r[16], fa_r[17], fa_r[18],
    fa_r[19], fa_r[20], fa_r[21], fa_r[22])
fsd_a_r <- sapply(fse_a_r, function(fse_a_r) sqrt(178) * fse_a_r)


aB_r <- data.frame(cbind(mB_a_r, fB_a_r, var_names_a))
aB_r$mean_B <- (as.numeric(aB_r$mB_a_r) +
    as.numeric(aB_r$fB_a_r)) / -2
#View(aB_r)

asd <- data.frame(cbind(msd_a_r, fsd_a_r, var_names_a))
asd$mean_sd <- (as.numeric(asd$msd_a_r) +
    as.numeric(asd$fsd_a_r)) / 2
View(asd)

# Interactive models (unstandardized) ------------------------------------------
mB_i <- c(mi_r[2], mi_r[3], mi_r[4], mi_r[5], mi_r[6], mi_r[7], mi_r[8],
    mi_r[9], mi_r[10], mi_r[11], mi_r[12])
mse_i <- c(mi_r[14], mi_r[15], mi_r[16], mi_r[17], mi_r[18], mi_r[19],
    mi_r[20], mi_r[21], mi_r[22], mi_r[23], mi_r[24])
msd_i <- sapply(mse_i, function(mse_i) sqrt(178) * mse_i)

fB_i <- c(fi_r[2], fi_r[3], fi_r[4], fi_r[5], fi_r[6], fi_r[7], fi_r[8],
    fi_r[9], fi_r[10], fi_r[11], fi_r[12])
fse_i <- c(fi_r[14], fi_r[15], fi_r[16], fi_r[17], fi_r[18], fi_r[19],
    fi_r[20], fi_r[21], fi_r[22], fi_r[23], fi_r[24])
fsd_i <- sapply(fse_i, function(fse_i) sqrt(178) * fse_i)


iB <- data.frame(cbind(mB_i, fB_i, var_names_i))
iB$mean_B <- (as.numeric(iB$mB_i) +
    as.numeric(iB$fB_i)) / -2
View(iB)

isd <- data.frame(cbind(msd_i, fsd_i, var_names_i))
isd$mean_sd <- (as.numeric(isd$msd_i) +
    as.numeric(isd$fsd_i)) / 2
View(isd)

ise <- data.frame(cbind(mse_i, fse_i, var_names_i))
ise$mean_se <- (as.numeric(ise$mse_i) +
    as.numeric(ise$fse_i)) / 2
View(ise)
