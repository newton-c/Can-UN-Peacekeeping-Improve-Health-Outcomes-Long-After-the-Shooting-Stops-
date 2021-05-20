library(dplyr)
library(haven)
library(lmtest)
library(sandwich)
library(stargazer)

d <- read_dta("data/hale_data.dta")


# Table 1: Additive models ----------------------------------------------------
model_1 <- lm(hale ~ pko_years + total_violence_1000 + civilwarborder +
    helog_knn + hdi_knn + urbangrowth_knn + gini_knn + tropical + xpolity_knn +
    ef_knn + y05 + y10 + y15, data = d)

model_2 <- lm(hale ~ pko_years + osv_1000 + civilwarborder +
    helog_knn + hdi_knn + urbangrowth_knn + gini_knn + tropical + xpolity_knn +
    ef_knn + y05 + y10 + y15, data = d)

model_3 <- lm(hale ~ pko_years + brd_1000 + civilwarborder +
    helog_knn + hdi_knn + urbangrowth_knn + gini_knn + tropical + xpolity_knn +
    ef_knn + y05 + y10 + y15, data = d)

stargazer(model_1, model_2, model_3, title = "Frequentist Additive Models",
    covariate.labels = c("PKO", "Total Violence", "One-Sided Violence",
        "Battle-Related Deaths", "Cont.CW", "Health Exp", "Education",
        "Urban Growth", "Gini", "Tropical", "x-Polity", "Ethnic Fract.",
        "2005", "2010", "2015"),
    type = "text", out = "figs/appendix_freq_add.txt"
)

# Table 2: Interaction models --------------------------------------------------
model_1 <- lm(hale ~ pko_years + total_violence_1000 + pkoxdeath +
    civilwarborder + helog_knn + hdi_knn + urbangrowth_knn + gini_knn +
    tropical + xpolity_knn + ef_knn + y05 + y10 + y15, data = d)

model_2 <- lm(hale ~ pko_years + osv_1000 + pkoxosv + civilwarborder +
    helog_knn + hdi_knn + urbangrowth_knn + gini_knn + tropical + xpolity_knn +
    ef_knn + y05 + y10 + y15, data = d)

model_3 <- lm(hale ~ pko_years + brd_1000 + pkoxbrd + civilwarborder +
    helog_knn + hdi_knn + urbangrowth_knn + gini_knn + tropical + xpolity_knn +
    ef_knn + y05 + y10 + y15, data = d)

stargazer(model_1, model_2, model_3, title = "Frequentist Interaction Models",
    covariate.labels = c("PKO", "Total Violence", "PKO x Total Violence",
        "One-Sided Violence", "PKO x One-Sided Violence",
        "Battle-Related Deaths", "PKO x Battle-Related Deaths", "Cont.CW",
        "Health Exp", "Education", "Urban Growth", "Gini", "Tropical",
        "x-Polity", "Ethnic Fract.", "2005", "2010", "2015"),
    type = "text", out = "figs/appendix_freq_int.txt"
)


# No outliers
outliers <- 2 * (sd(d$total_violence_1000))
d_no  <- filter(d, total_violence_1000 < outliers)
# Table 3: Additive models, no outliers ---------------------------------------
model_1 <- lm(hale ~ pko_years + total_violence_1000 + civilwarborder +
    helog_knn + hdi_knn + urbangrowth_knn + gini_knn + tropical + xpolity_knn +
    ef_knn + y05 + y10 + y15, data = d_no)

model_2 <- lm(hale ~ pko_years + osv_1000 + civilwarborder +
    helog_knn + hdi_knn + urbangrowth_knn + gini_knn + tropical + xpolity_knn +
    ef_knn + y05 + y10 + y15, data = d_no)

model_3 <- lm(hale ~ pko_years + brd_1000 + civilwarborder +
    helog_knn + hdi_knn + urbangrowth_knn + gini_knn + tropical + xpolity_knn +
    ef_knn + y05 + y10 + y15, data = d_no)

stargazer(model_1, model_2, model_3,
          title = "Frequentist Additive Models (No Outliers)",
    covariate.labels = c("PKO", "Total Violence", "One-Sided Violence",
        "Battle-Related Deaths", "Cont.CW", "Health Exp", "Education",
        "Urban Growth", "Gini", "Tropical", "x-Polity", "Ethnic Fract.",
        "2005", "2010", "2015"),
    type = "text", out = "figs/appendix_freq_add_no.txt"
)

# Table 4: Interaction models, no outliers -------------------------------------
model_1 <- lm(hale ~ pko_years + total_violence_1000 + pkoxdeath +
    civilwarborder + helog_knn + hdi_knn + urbangrowth_knn + gini_knn +
    tropical + xpolity_knn + ef_knn + y05 + y10 + y15, data = d_no)

model_2 <- lm(hale ~ pko_years + osv_1000 + pkoxosv + civilwarborder +
    helog_knn + hdi_knn + urbangrowth_knn + gini_knn + tropical + xpolity_knn +
    ef_knn + y05 + y10 + y15, data = d_no)

model_3 <- lm(hale ~ pko_years + brd_1000 + pkoxbrd + civilwarborder +
    helog_knn + hdi_knn + urbangrowth_knn + gini_knn + tropical + xpolity_knn +
    ef_knn + y05 + y10 + y15, data = d_no)

stargazer(model_1, model_2, model_3,
          title = "Frequentist Interaction Models, (No Outliers)",
    covariate.labels = c("PKO", "Total Violence", "PKO x Total Violence",
        "One-Sided Violence", "PKO x One-Sided Violence",
        "Battle-Related Deaths", "PKO x Battle-Related Deaths", "Cont.CW",
        "Health Exp", "Education", "Urban Growth", "Gini", "Tropical",
        "x-Polity", "Ethnic Fract.", "2005", "2010", "2015"),
    type = "text", out = "figs/appendix_freq_int_no.txt"
)


# Prior generating models: raw -------------------------------------------------
ghobarah_etal_replication <- read_dta("data/Replication_Interactions.dta")

ghobarah_etal_replication$k_total_log <-
    ghobarah_etal_replication$`_k_total_log`

ghobarah_etal_replication$pko_yearsxdeath91_97 <-
    ghobarah_etal_replication$pko_years * ghobarah_etal_replication$deathn91_97

# Subsets are dat_gndr_ct, so dat_2_1 is a subset where gnrd == 2 and ct == 1.
dat_2_1r <- filter(ghobarah_etal_replication, gndr == 2 & ct == 1)
dat_3_25r <- filter(ghobarah_etal_replication, gndr == 3 & ct == 25)


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

stargazer(fa_r, ma_r, fi_r, mi_r, title = "Unstandardized Models",
    covariate.labels = c("PKO", "Battle-Related Deaths",
        "PKO x Battle-Related Deaths", "Cont.CW", "Health Exp",
        "Education", "Urban Growth", "Gini", "Tropical", "Polity",
        "Ethnic Fract."),
    column.labels = c("Female 15-44", "Male 15-44", "Female 15-44",
        "Male 15-44"),
    type = "text", out = "figs/appendix_ghobarah_unst.txt"
)

# Prior generating models: standardized ----------------------------------------
ghobarah_etal <- ghobarah_etal_replication %>%
	mutate_at(c('dalyp15_44', 'pko_years', 'deathn91_97',
    'k_total_log', 'log_educational_attainment', 'contig_civil_war',
    'growth_urban_pop_un', 'gini', 'polity', 'log_vanhanen'), ~(scale(.) %>%
    as.vector))

ghobarah_etal$pko_yearsxdeath91_97 <- ghobarah_etal$pko_years *
    ghobarah_etal$deathn91_97

# Subsets are dat_gndr_ct, so dat_2_1 is a subset where gnrd == 2 and ct == 1.
dat_2_1 <- filter(ghobarah_etal, gndr == 2 & ct == 1)
dat_3_25 <- filter(ghobarah_etal, gndr == 3 & ct == 25)


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

stargazer(fa, ma, fi, mi, title = "Standardized Models",
    covariate.labels = c("PKO", "Battle-Related Deaths",
        "PKO x Battle-Related Deaths", "Cont.CW", "Health Exp",
        "Education", "Urban Growth", "Gini", "Tropical", "Polity",
        "Ethnic Fract."),
    column.labels = c("Female 15-44", "Male 15-44", "Female 15-44",
        "Male 15-44"),
    type = "text", out = "figs/appendix_ghobarah_st.txt"
)

# Mataching -------------------------------------------------------------------
# Table A3: Additive models 
model_1 <- lm(hale ~ pko_years + total_violence_1000 + civilwarborder +
    helog_knn + hdi_knn + urbangrowth_knn + gini_knn + tropical + xpolity_knn +
    ef_knn + y05 + y10 + y15, data = d, weights = cem_weights)

model_2 <- lm(hale ~ pko_years + osv_1000 + civilwarborder +
    helog_knn + hdi_knn + urbangrowth_knn + gini_knn + tropical + xpolity_knn +
    ef_knn + y05 + y10 + y15, data = d, weights = cem_weights)

model_3 <- lm(hale ~ pko_years + brd_1000 + civilwarborder +
    helog_knn + hdi_knn + urbangrowth_knn + gini_knn + tropical + xpolity_knn +
    ef_knn + y05 + y10 + y15, data = d, weights = cem_weights)

stargazer(model_1, model_2, model_3, title = "Matching Additive Models",
    covariate.labels = c("PKO", "Total Violence", "One-Sided Violence",
        "Battle-Related Deaths", "Cont.CW", "Health Exp", "Education",
        "Urban Growth", "Gini", "Tropical", "x-Polity", "Ethnic Fract.",
        "2005", "2010", "2015"),
    type = "text", out = "figs/appendix_freq_add_matching.txt"
)

# Table A4: Interaction models ------------------------------------------------
model_1 <- lm(hale ~ pko_years + total_violence_1000 + pkoxdeath +
    civilwarborder + helog_knn + hdi_knn + urbangrowth_knn + gini_knn +
    tropical + xpolity_knn + ef_knn + y05 + y10 + y15, data = d,
    weights = cem_weights)

model_2 <- lm(hale ~ pko_years + osv_1000 + pkoxosv + civilwarborder +
    helog_knn + hdi_knn + urbangrowth_knn + gini_knn + tropical + xpolity_knn +
    ef_knn + y05 + y10 + y15, data = d, weights = cem_weights)

model_3 <- lm(hale ~ pko_years + brd_1000 + pkoxbrd + civilwarborder +
    helog_knn + hdi_knn + urbangrowth_knn + gini_knn + tropical + xpolity_knn +
    ef_knn + y05 + y10 + y15, data = d, weights = cem_weights)

model_4 <- lm(hale ~ pko_years + total_violence_1000 + pkoxdeath +
    civilwarborder + helog_knn + hdi_knn + urbangrowth_knn + gini_knn +
    tropical + xpolity_knn + ef_knn + y05 + y10 + y15, data = d_no,
    weights = cem_weights)

model_5 <- lm(hale ~ pko_years + osv_1000 + pkoxosv + civilwarborder +
    helog_knn + hdi_knn + urbangrowth_knn + gini_knn + tropical + xpolity_knn +
    ef_knn + y05 + y10 + y15, data = d_no, weights = cem_weights)

model_6 <- lm(hale ~ pko_years + brd_1000 + pkoxbrd + civilwarborder +
    helog_knn + hdi_knn + urbangrowth_knn + gini_knn + tropical + xpolity_knn +
    ef_knn + y05 + y10 + y15, data = d_no, weights = cem_weights)

stargazer(model_1, model_2, model_3, title = "Matching Interaction Models",
    covariate.labels = c("PKO", "Total Violence", "PKO x Total Violence",
        "One-Sided Violence", "PKO x One-Sided Violence",
        "Battle-Related Deaths", "PKO x Battle-Related Deaths", "Cont.CW",
        "Health Exp", "Education", "Urban Growth", "Gini", "Tropical",
        "x-Polity", "Ethnic Fract.", "2005", "2010", "2015"),
    type = "text", out = "figs/appendix_freq_int_matching_no.txt"
)

