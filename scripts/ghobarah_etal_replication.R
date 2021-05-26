library(dplyr)
library(haven)
library(lmtest)
library(sandwich)

replication <- read_dta("data/Replication_Interactions.dta")

replication$k_total_log <- replication$`_k_total_log`

replication$pko_yearsxdeath91_97 <-
    replication$pko_years * replication$deathn91_97

# standardizing
replication_sd <- replication %>%
    mutate_at(c("dalyp15_44", "pko_years", "deathn91_97",
    "k_total_log", "log_educational_attainment", "contig_civil_war",
    "growth_urban_pop_un", "gini", "polity", "log_vanhanen"),
              ~ (scale(.) %>%
    as.vector))

replication_sd$pko_yearsxdeath91_97 <- replication_sd$pko_years *
    replication_sd$deathn91_97

# Subsets are dat_gndr_ct, so dat_2_1 is a subset where gnrd == 2 and ct == 1.
dat_2_1 <- filter(replication_sd, gndr == 2 & ct == 1)
dat_3_25 <- filter(replication_sd, gndr == 3 & ct == 25)


# male, additive
ma <- lm(dalyp15_44 ~ pko_years + deathn91_97 + contig_civil_war + k_total_log +
    log_educational_attainment + growth_urban_pop_un + gini +
    tropical + polity + log_vanhanen, data = dat_2_1)

# female, additive
fa <- lm(dalyp15_44 ~ pko_years + deathn91_97 + contig_civil_war + k_total_log +
    log_educational_attainment + growth_urban_pop_un + gini +
    tropical + polity + log_vanhanen, data = dat_3_25)

# male, interaction
mi <- lm(dalyp15_44 ~ pko_years + deathn91_97 + pko_yearsxdeath91_97 +
    contig_civil_war + k_total_log + log_educational_attainment +
    growth_urban_pop_un + gini + tropical + polity + log_vanhanen,
    data = dat_2_1)

# female, interaction
fi <- lm(dalyp15_44 ~ pko_years + deathn91_97 + pko_yearsxdeath91_97 +
    contig_civil_war + k_total_log + log_educational_attainment +
    growth_urban_pop_un + gini + tropical + polity + log_vanhanen,
    data = dat_3_25)

ma <- coeftest(ma, vcov = vcovHC(ma, type = "HC1"))
fa <- coeftest(fa, vcov = vcovHC(fa, type = "HC1"))
mi <- coeftest(mi, vcov = vcovHC(mi, type = "HC1"))
fi <- coeftest(fi, vcov = vcovHC(fi, type = "HC1"))

# Unstandardized models --------------------------------------------------------
replication_sd$pko_yearsxdeath91_97 <- replication_sd$pko_years *
    replication_sd$deathn91_97

# Subsets are dat_gndr_ct, so dat_2_1 is a subset where gnrd == 2 and ct == 1.
dat_2_1r <- filter(replication, gndr == 2 & ct == 1)
dat_3_25r <- filter(replication, gndr == 3 & ct == 25)


# male, additive
ma_r <- lm(dalyp15_44 ~ pko_years + deathn91_97 + contig_civil_war +
    k_total_log + log_educational_attainment + growth_urban_pop_un + gini +
    tropical + polity + log_vanhanen, data = dat_2_1r)

# female, additive
fa_r <- lm(dalyp15_44 ~ pko_years + deathn91_97 + contig_civil_war +
    k_total_log + log_educational_attainment + growth_urban_pop_un + gini +
    tropical + polity + log_vanhanen, data = dat_3_25r)

# male, interaction
mi_r <- lm(dalyp15_44 ~ pko_years + deathn91_97 + pko_yearsxdeath91_97 +
    contig_civil_war + k_total_log + log_educational_attainment +
    growth_urban_pop_un + gini + tropical + polity + log_vanhanen,
    data = dat_2_1r)

# female, interaction
fi_r <- lm(dalyp15_44 ~ pko_years + deathn91_97 + pko_yearsxdeath91_97 +
    contig_civil_war + k_total_log + log_educational_attainment +
    growth_urban_pop_un + gini + tropical + polity + log_vanhanen,
    data = dat_3_25r)

ma_r <- coeftest(ma_r, vcov = vcovHC(ma_r, type = "HC1"))
fa_r <- coeftest(fa_r, vcov = vcovHC(fa_r, type = "HC1"))
mi_r <- coeftest(mi_r, vcov = vcovHC(mi_r, type = "HC1"))
fi_r <- coeftest(fi_r, vcov = vcovHC(fi_r, type = "HC1"))
