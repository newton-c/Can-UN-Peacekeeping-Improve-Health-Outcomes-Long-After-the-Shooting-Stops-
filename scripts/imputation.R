library(DMwR)
library(haven)
library(tidyverse)

dat <- read_dta("data/fixedNAs21Dec2020.dta")

dat$osvlog <- ifelse(dat$osv == 0, 0, log(dat$osv))

s <- as.data.frame(dat[c("intensity", "year", "idps", "xpolity",
    "pko_bt", "dale", "deathsciv_new", "deathstotal_new", "helog", "gini",
    "total_years_since_last_ep", "pkoyearsany", "pkoyears3000", "add_cease",
    "priorpko", "ccode", "pko10000", "ef", "hdi", "urbangrowth", "osvlog",
    "osv")])

# Standardizing variables
s <- s %>%
    mutate(
        "idps" = (idps - mean(idps, na.rm = T)) / sd(idps, na.rm = T),
        "xpolity" = (xpolity - mean(xpolity, na.rm = T)) /
            sd(xpolity, na.rm = T),
        "dale_s" = (dale - mean(dale, na.rm = T)) / sd(dale, na.rm = T),
        "deathsciv_new" = (deathsciv_new - mean(deathsciv_new, na.rm = T)) /
            sd(deathsciv_new, na.rm = T),
        "deathstotal_new" = (deathstotal_new - mean(deathstotal_new,
            na.rm = T)) / sd(deathstotal_new, na.rm = T),
        "helog" = (helog - mean(helog, na.rm = T)) / sd(helog, na.rm = T),
        "gini" = (gini - mean(gini, na.rm = T)) / sd(gini, na.rm = T),
        "ef" = (ef - mean(ef, na.rm = T)) / sd(ef, na.rm = T),
        "hdi" = (hdi - mean(hdi, na.rm = T)) / sd(hdi, na.rm = T),
        "urbangrowth" = (urbangrowth - mean(urbangrowth, na.rm = T)) /
            sd(urbangrowth, na.rm = T),
        "osvlog" = (osvlog - mean(osvlog, na.rm = T)) / sd(osvlog, na.rm = T),
        "osv" = (osv - mean(osv, na.rm = T)) / sd(osv, na.rm = T)
    )

knn_impute <- knnImputation(data = s, k = 15, scale = F, meth = "weighAve")

knn_impute <- knn_impute[c("year", "xpolity", "helog",
                           "gini", "ccode", "ef", "hdi", "urbangrowth")]

colnames(knn_impute) <- c("year", "xpolity_knn", "helog_knn", "gini_knn",
                          "ccode", "ef_knn", "hdi_knn", "urbangrowth_knn")

s <- merge(s, knn_impute, by = c("year", "ccode"), all = T)
rm(list = c('dat', 'knn_impute'))

write_dta(s, "data/standardized_data.dta", version = 13)
