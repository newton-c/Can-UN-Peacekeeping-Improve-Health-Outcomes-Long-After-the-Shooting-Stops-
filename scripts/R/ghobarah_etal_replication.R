library(dplyr)
library(haven)
library(lmtest)
library(sandwich)
library(stargazer)

replication <- read_dta("data/Replication_Interactions.dta")

replication$k_total_log <- replication$`_k_total_log`

replication$pko_yearsxdeath91_97 <-
    replication$pko_years * replication$deathn91_97


# Subsets are dat_gndr_ct, so dat_2_1 is a subset where gnrd == 2 and ct == 1.
dat_2_1 <- filter(replication, gndr == 2 & ct == 1)
dat_3_25 <- filter(replication, gndr == 3 & ct == 25)

# male, additive 0-4
ma0_4 <- lm(dalyp0 ~ pko_years + deathn91_97 + contig_civil_war +
    k_total_log + log_educational_attainment + growth_urban_pop_un + gini +
    tropical + polity + log_vanhanen, data = dat_2_1)

# female, additive 0-4
fa0_4 <- lm(dalyp0 ~ pko_years + deathn91_97 + contig_civil_war +
    k_total_log + log_educational_attainment + growth_urban_pop_un + gini +
    tropical + polity + log_vanhanen, data = dat_3_25)

# male, interaction 0-4
mi0_4 <- lm(dalyp0 ~ pko_years + deathn91_97 + pko_yearsxdeath91_97 +
    contig_civil_war + k_total_log + log_educational_attainment +
    growth_urban_pop_un + gini + tropical + polity + log_vanhanen,
    data = dat_2_1)

# female, interaction 0-4
fi0_4 <- lm(dalyp0 ~ pko_years + deathn91_97 + pko_yearsxdeath91_97 +
    contig_civil_war + k_total_log + log_educational_attainment +
    growth_urban_pop_un + gini + tropical + polity + log_vanhanen,
    data = dat_3_25)

# male, additive 5-14
ma5_14 <- lm(dalyp5_14 ~ pko_years + deathn91_97 + contig_civil_war +
    k_total_log + log_educational_attainment + growth_urban_pop_un + gini +
    tropical + polity + log_vanhanen, data = dat_2_1)

# female, additive 5-14
fa5_14 <- lm(dalyp5_14 ~ pko_years + deathn91_97 + contig_civil_war +
    k_total_log + log_educational_attainment + growth_urban_pop_un + gini +
    tropical + polity + log_vanhanen, data = dat_3_25)

# male, interaction 5-14
mi5_14 <- lm(dalyp5_14 ~ pko_years + deathn91_97 + pko_yearsxdeath91_97 +
    contig_civil_war + k_total_log + log_educational_attainment +
    growth_urban_pop_un + gini + tropical + polity + log_vanhanen,
    data = dat_2_1)

# female, interaction 5-14
fi5_14 <- lm(dalyp5_14 ~ pko_years + deathn91_97 + pko_yearsxdeath91_97 +
    contig_civil_war + k_total_log + log_educational_attainment +
    growth_urban_pop_un + gini + tropical + polity + log_vanhanen,
    data = dat_3_25)

# male, additive 15-44
ma15_44 <- lm(dalyp15_44 ~ pko_years + deathn91_97 + contig_civil_war +
    k_total_log + log_educational_attainment + growth_urban_pop_un + gini +
    tropical + polity + log_vanhanen, data = dat_2_1)

# female, additive 15-44
fa15_44 <- lm(dalyp15_44 ~ pko_years + deathn91_97 + contig_civil_war +
    k_total_log + log_educational_attainment + growth_urban_pop_un + gini +
    tropical + polity + log_vanhanen, data = dat_3_25)

# male, interaction 15-44
mi15_44 <- lm(dalyp15_44 ~ pko_years + deathn91_97 + pko_yearsxdeath91_97 +
    contig_civil_war + k_total_log + log_educational_attainment +
    growth_urban_pop_un + gini + tropical + polity + log_vanhanen,
    data = dat_2_1)

# female, interaction 15-44
fi15_44 <- lm(dalyp15_44 ~ pko_years + deathn91_97 + pko_yearsxdeath91_97 +
    contig_civil_war + k_total_log + log_educational_attainment +
    growth_urban_pop_un + gini + tropical + polity + log_vanhanen,
    data = dat_3_25)

# male, additive 45-59
ma45_59 <- lm(dalyp45_59 ~ pko_years + deathn91_97 + contig_civil_war +
    k_total_log + log_educational_attainment + growth_urban_pop_un + gini +
    tropical + polity + log_vanhanen, data = dat_2_1)

# female, additive 45-59
fa45_59 <- lm(dalyp45_59 ~ pko_years + deathn91_97 + contig_civil_war +
    k_total_log + log_educational_attainment + growth_urban_pop_un + gini +
    tropical + polity + log_vanhanen, data = dat_3_25)

# male, interaction 45-59
mi45_59 <- lm(dalyp45_59 ~ pko_years + deathn91_97 + pko_yearsxdeath91_97 +
    contig_civil_war + k_total_log + log_educational_attainment +
    growth_urban_pop_un + gini + tropical + polity + log_vanhanen,
    data = dat_2_1)

# female, interaction 45-59
fi45_59 <- lm(dalyp45_59 ~ pko_years + deathn91_97 + pko_yearsxdeath91_97 +
    contig_civil_war + k_total_log + log_educational_attainment +
    growth_urban_pop_un + gini + tropical + polity + log_vanhanen,
    data = dat_3_25)

# male, additive 60plus
ma60plus <- lm(dalyp60plus ~ pko_years + deathn91_97 + contig_civil_war +
    k_total_log + log_educational_attainment + growth_urban_pop_un + gini +
    tropical + polity + log_vanhanen, data = dat_2_1)

# female, additive 60plus
fa60plus <- lm(dalyp60plus ~ pko_years + deathn91_97 + contig_civil_war +
    k_total_log + log_educational_attainment + growth_urban_pop_un + gini +
    tropical + polity + log_vanhanen, data = dat_3_25)

# male, interaction 60plus
mi60plus <- lm(dalyp60plus ~ pko_years + deathn91_97 + pko_yearsxdeath91_97 +
    contig_civil_war + k_total_log + log_educational_attainment +
    growth_urban_pop_un + gini + tropical + polity + log_vanhanen,
    data = dat_2_1)

# female, interaction 60plus
fi60plus <- lm(dalyp60plus ~ pko_years + deathn91_97 + pko_yearsxdeath91_97 +
    contig_civil_war + k_total_log + log_educational_attainment +
    growth_urban_pop_un + gini + tropical + polity + log_vanhanen,
    data = dat_3_25)

ma0_4 <- coeftest(ma0_4, vcov = vcovHC(ma0_4, type = "HC1"))
mi0_4 <- coeftest(mi0_4, vcov = vcovHC(mi0_4, type = "HC1"))
fa0_4 <- coeftest(fa0_4, vcov = vcovHC(fa0_4, type = "HC1"))
fi0_4 <- coeftest(fi0_4, vcov = vcovHC(fi0_4, type = "HC1"))

ma5_14 <- coeftest(ma5_14, vcov = vcovHC(ma5_14, type = "HC1"))
mi5_14 <- coeftest(mi5_14, vcov = vcovHC(mi5_14, type = "HC1"))
fa5_14 <- coeftest(fa5_14, vcov = vcovHC(fa5_14, type = "HC1"))
fi5_14 <- coeftest(fi5_14, vcov = vcovHC(fi5_14, type = "HC1"))

ma15_44 <- coeftest(ma15_44, vcov = vcovHC(ma15_44, type = "HC1"))
mi15_44 <- coeftest(mi15_44, vcov = vcovHC(mi15_44, type = "HC1"))
fa15_44 <- coeftest(fa15_44, vcov = vcovHC(fa15_44, type = "HC1"))
fi15_44 <- coeftest(fi15_44, vcov = vcovHC(fi15_44, type = "HC1"))

ma45_59 <- coeftest(ma45_59, vcov = vcovHC(ma45_59, type = "HC1"))
mi45_59 <- coeftest(mi45_59, vcov = vcovHC(mi45_59, type = "HC1"))
fa45_59 <- coeftest(fa45_59, vcov = vcovHC(fa45_59, type = "HC1"))
fi45_59 <- coeftest(fi45_59, vcov = vcovHC(fi45_59, type = "HC1"))

ma60plus <- coeftest(ma60plus, vcov = vcovHC(ma60plus, type = "HC1"))
mi60plus <- coeftest(mi60plus, vcov = vcovHC(mi60plus, type = "HC1"))
fa60plus <- coeftest(fa60plus, vcov = vcovHC(fa60plus, type = "HC1"))
fi60plus <- coeftest(fi60plus, vcov = vcovHC(fi60plus, type = "HC1"))

stargazer(ma0_4, mi0_4, fa0_4, fi0_4, ma5_14, mi5_14, fa5_14, fi5_14, ma15_44,
          mi15_44, fa15_44, fi15_44, ma45_59, mi45_59, fa45_59, fi45_59,
          ma60plus, mi60plus, fa60plus, fi60plus, type = "text",
          covariate.labels = c("PKO Years", "Civil War Deaths",
                               "PKO Years x Civil War Deaths",
                               "Contiguous Civil War", "Total Health Spending",
                               "Education", "Urban Growth", "Income Gini",
                               "Tropical", "Polity Score",
                               "Ethnic Heterogeneity"),
          column.sep.width = "1pt",
          font.size = "tiny",
          digits = 2,
          out = "figs/table1.txt")
