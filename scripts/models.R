library(haven)
library(modelsummary)
library(tidyverse)
library(Zelig)


m.out <- read_dta("data/matched_data.dta")
m.out$idps_per100000 <- m.out$idps_per1000 / 100
m.out$osv_per10000 <- m.out$osv / 10000
m.out$year <- as.factor(m.out$year)
no_outliers_matched <- filter(m.out, deathstotal_and_osv_10000 < 2.5 )
additional_row <- tibble::tribble(~name, ~model1, ~model2, ~model3, ~model4,
    "Outliers Omitted: ", "No", "No", "Yes", "Yes")

m.weights <- as.vector(m.out$weights)
no.weights <- as.vector(no_outliers_matched$weights)

cm <- c(
    'pkoyearsany' = 'Years with PKO',
    'deathstotal_and_osv_10000' = 'Total Violence',
    'osv_per10000' = 'One-Sided Violence',
    'deathstotal_new' = 'Battle-Related Deaths',
    'pkoyearsany:deathstotal_and_osv_10000' = 'Total Violence x Years with PKO',
    'pkoyearsany:osv_per10000' = 'One-Sided Violence x Years with PKO',
    'pkoyearsany:deathstotal_new' = 'Battle-Related Deaths x Years with PKO',
    'priorpko' = "Previous PKO",
    'idps_per100000' = 'Internally Displaced Persons',
    'helog_knn' = 'Health Expend. (log)',
    'hdi_knn' = 'Education (HDI)',
    'civilwarborder' = 'Borderding Civil Wars',
    'urbangrowth_knn' = 'Urban Growth',
    'gini_knn' = 'Gini',
    'tropical' = 'Tropical',
    'xpolity_knn' = 'x-Polity',
    'ef_knn' = 'Ethnic Fractionalization',
    'year2005' = '2005',
    'year2010' = '2010',
    'year2015' = '2015',
    '(Intercept)' = 'Constant'
    )


# Total violence
table_2 = list(
    `(21)` = zelig(dale ~ pkoyearsany + deathstotal_and_osv_10000 +
        idps_per100000 + helog_knn + hdi_knn + civilwarborder +
        urbangrowth_knn + gini_knn + tropical + xpolity_knn + ef_knn +
        priorpko + year, data = m.out, weights = m.weights,
        model = "ls", cite = FALSE) %>%
    from_zelig_model(),

    `(22)` = zelig(dale ~ pkoyearsany + deathstotal_and_osv_10000 +
        idps_per100000 + pkoyearsany:deathstotal_and_osv_10000 + helog_knn +
        hdi_knn + civilwarborder + urbangrowth_knn + gini_knn + tropical +
        xpolity_knn + ef_knn + priorpko + year, data = m.out,
        weights = m.weights, model = "ls", cite = FALSE) %>%
    from_zelig_model(),

    `(23)` = zelig(dale ~ deathstotal_and_osv_10000 + idps_per100000 +
        helog_knn + hdi_knn + civilwarborder + urbangrowth_knn + gini_knn +
        tropical + xpolity_knn + ef_knn + priorpko + year +
        pkoyearsany, data = no_outliers_matched, weights = no.weights,
        model = "ls", cite = FALSE) %>%
    from_zelig_model(),

    `(24)` = zelig(dale ~ pkoyearsany + pkoyearsany:deathstotal_and_osv_10000 +
        deathstotal_and_osv_10000 + idps_per100000 + helog_knn + hdi_knn +
        civilwarborder + urbangrowth_knn + gini_knn + tropical + xpolity_knn +
        ef_knn + priorpko + year, data = no_outliers_matched,
        weights = no.weights, model = "ls", cite = FALSE) %>%
    from_zelig_model()
)

modelsummary(table_2, stars = T,
    title = "TABLE 2: CEM regressions – Total fatalities",
    add_rows = additional_row, coef_map = cm, fmt = "%.2f",
    gof_omit = "AIC|Dev|DF|Sigma|Stat|P|Log",
    output = "figs/table2.tex"
         )



# osv
table_3 = list(
    `(25)` = zelig(dale ~ pkoyearsany + osv_per10000 +
        idps_per100000 + helog_knn + hdi_knn + civilwarborder +
        urbangrowth_knn + gini_knn + tropical + xpolity_knn + ef_knn +
        priorpko + year, data = m.out, weights = m.weights,
        model = "ls", cite = FALSE) %>%
    from_zelig_model(),

    `(26)` = zelig(dale ~ pkoyearsany + osv_per10000 +
        idps_per100000 + pkoyearsany:osv_per10000 + helog_knn +
        hdi_knn + civilwarborder + urbangrowth_knn + gini_knn + tropical +
        xpolity_knn + ef_knn + priorpko + year, data = m.out,
        weights = m.weights, model = "ls", cite = FALSE) %>%
    from_zelig_model(),

    `(27)` = zelig(dale ~ osv_per10000 + idps_per100000 +
        helog_knn + hdi_knn + civilwarborder + urbangrowth_knn + gini_knn +
        tropical + xpolity_knn + ef_knn + priorpko + year +
        pkoyearsany, data = no_outliers_matched, weights = no.weights,
        model = "ls", cite = FALSE) %>%
    from_zelig_model(),

    `(28)` = zelig(dale ~ pkoyearsany + pkoyearsany:osv_per10000 +
        osv_per10000 + idps_per100000 + helog_knn + hdi_knn +
        civilwarborder + urbangrowth_knn + gini_knn + tropical + xpolity_knn +
        ef_knn + priorpko + year, data = no_outliers_matched,
        weights = no.weights, model = "ls", cite = FALSE) %>%
    from_zelig_model()
)

modelsummary(table_3, stars = T,
    add_rows = additional_row, coef_map = cm, fmt = "%.2f",
    gof_omit = "AIC|Dev|DF|Sigma|Stat|P|Log",
    output = "figs/table3.tex"
         )

# Battle-Related Deaths
table_4 = list(
    `(29)` = zelig(dale ~ pkoyearsany + deathstotal_new +
        idps_per100000 + helog_knn + hdi_knn + civilwarborder +
        urbangrowth_knn + gini_knn + tropical + xpolity_knn + ef_knn +
        priorpko + year, data = m.out, weights = m.weights,
        model = "ls", cite = FALSE) %>%
    from_zelig_model(),

    `(30)` = zelig(dale ~ pkoyearsany + deathstotal_new +
        idps_per100000 + pkoyearsany:deathstotal_new + helog_knn +
        hdi_knn + civilwarborder + urbangrowth_knn + gini_knn + tropical +
        xpolity_knn + ef_knn + priorpko + year, data = m.out,
        weights = m.weights, model = "ls", cite = FALSE) %>%
    from_zelig_model(),

    `(31)` = zelig(dale ~ deathstotal_new + idps_per100000 +
        helog_knn + hdi_knn + civilwarborder + urbangrowth_knn + gini_knn +
        tropical + xpolity_knn + ef_knn + priorpko + year +
        pkoyearsany, data = no_outliers_matched, weights = no.weights,
        model = "ls", cite = FALSE) %>%
    from_zelig_model(),

    `(32)` = zelig(dale ~ pkoyearsany + pkoyearsany:deathstotal_new +
        deathstotal_new + idps_per100000 + helog_knn + hdi_knn +
        civilwarborder + urbangrowth_knn + gini_knn + tropical + xpolity_knn +
        ef_knn + priorpko + year, data = no_outliers_matched,
        weights = no.weights, model = "ls", cite = FALSE) %>%
    from_zelig_model()
)

modelsummary(table_4, stars = T,
    add_rows = additional_row, coef_map = cm, fmt = "%.2f",
    gof_omit = "AIC|Dev|DF|Sigma|Stat|P|Log",
    output = "figs/table4.tex"
         )

# Fewer covariates
table_5 = list(
    `(1)` = zelig(dale ~ pkoyearsany + deathstotal_and_osv_10000 +
        helog_knn + tropical + ef_knn +
        priorpko + year, data = m.out,
        model = "ls", cite = FALSE) %>%
    from_zelig_model(),

    `(2)` = zelig(dale ~ pkoyearsany + deathstotal_and_osv_10000 +
        hdi_knn + tropical + ef_knn +
        priorpko + year, data = m.out,
        model = "ls", cite = FALSE) %>%
    from_zelig_model(),

    `(3)` = zelig(dale ~ pkoyearsany + deathstotal_and_osv_10000 +
        pkoyearsany:deathstotal_and_osv_10000 +
        helog_knn + tropical + ef_knn +
        priorpko + year, data = m.out,
        model = "ls", cite = FALSE) %>%
    from_zelig_model(),

    `(4)` = zelig(dale ~ pkoyearsany + deathstotal_and_osv_10000 +
        pkoyearsany:deathstotal_and_osv_10000 +
        hdi_knn + tropical + ef_knn +
        priorpko + year, data = m.out,
        model = "ls", cite = FALSE) %>%
    from_zelig_model(),

    `(5)` = zelig(dale ~ pkoyearsany + deathstotal_and_osv_10000 +
        pkoyearsany:deathstotal_and_osv_10000 +
        helog_knn + tropical +
        priorpko + year, data = m.out,
        model = "ls", cite = FALSE) %>%
    from_zelig_model(),

    `(6)` = zelig(dale ~ pkoyearsany + deathstotal_and_osv_10000 +
        pkoyearsany:deathstotal_and_osv_10000 +
        hdi_knn + tropical +
        priorpko + year, data = m.out,
        model = "ls", cite = FALSE) %>%
    from_zelig_model()
)

modelsummary(table_5, stars = T,
    add_rows = additional_row, coef_map = cm, fmt = "%.2f",
         )

table_6 = list(
    `(21)` = zelig(dale ~ pkoyearsany + deathstotal_and_osv_10000 +
        idps_per100000 + helog_knn + hdi_knn + civilwarborder +
        urbangrowth_knn + gini_knn + tropical + xpolity_knn + ef_knn +
        priorpko + year, data = m.out,
        model = "ls", cite = FALSE) %>%
    from_zelig_model(),

    `(22)` = zelig(dale ~ pkoyearsany + deathstotal_and_osv_10000 +
        idps_per100000 + pkoyearsany:deathstotal_and_osv_10000 + helog_knn +
        hdi_knn + civilwarborder + urbangrowth_knn + gini_knn + tropical +
        xpolity_knn + ef_knn + priorpko + year, data = m.out,
        model = "ls", cite = FALSE) %>%
    from_zelig_model(),

    `(23)` = zelig(dale ~ deathstotal_and_osv_10000 + idps_per100000 +
        helog_knn + hdi_knn + civilwarborder + urbangrowth_knn + gini_knn +
        tropical + xpolity_knn + ef_knn + priorpko + year +
        pkoyearsany, data = no_outliers_matched,
        model = "ls", cite = FALSE) %>%
    from_zelig_model(),

    `(24)` = zelig(dale ~ pkoyearsany + pkoyearsany:deathstotal_and_osv_10000 +
        deathstotal_and_osv_10000 + idps_per100000 + helog_knn + hdi_knn +
        civilwarborder + urbangrowth_knn + gini_knn + tropical + xpolity_knn +
        ef_knn + priorpko + year, data = no_outliers_matched,
        model = "ls", cite = FALSE) %>%
    from_zelig_model()
)

modelsummary(table_6, stars = T,
    title = "TABLE 2: CEM regressions – Total fatalities",
    add_rows = additional_row, coef_map = cm, fmt = "%.2f",
    gof_omit = "AIC|Dev|DF|Sigma|Stat|P|Log"
         )


# Only conflict or peacekeeping -----------------------------------------------

con_pko <- filter(m.out, deathstotal_and_osv_10000 > 25)
no_out <- filter(no_outliers_matched, pkoyearsany > 0 |
    deathstotal_and_osv_10000 > 0)
wcon_pko <- as.vector(con_pko$weights)
wno_out <- as.vector(no_out$weights)

# Total violence
table_6 = list(
    `(21)` = zelig(dale ~ pkoyearsany + deathstotal_and_osv_10000 +
        idps_per100000 + helog_knn + hdi_knn + civilwarborder +
        urbangrowth_knn + gini_knn + tropical + xpolity_knn + ef_knn +
        year, data = con_pko, weights = wcon_pko,
        model = "ls", cite = FALSE) %>%
    from_zelig_model(),

    `(22)` = zelig(dale ~ pkoyearsany + deathstotal_and_osv_10000 +
        idps_per100000 + pkoyearsany:deathstotal_and_osv_10000 + helog_knn +
        hdi_knn + civilwarborder + urbangrowth_knn + gini_knn + tropical +
        xpolity_knn + ef_knn + year, data = con_pko,
        weights = wcon_pko, model = "ls", cite = FALSE) %>%
    from_zelig_model(),

    `(23)` = zelig(dale ~ deathstotal_and_osv_10000 + idps_per100000 +
        helog_knn + hdi_knn + civilwarborder + urbangrowth_knn + gini_knn +
        tropical + xpolity_knn + ef_knn + year +
        pkoyearsany, data = no_out, weights = wno_out,
        model = "ls", cite = FALSE) %>%
    from_zelig_model(),

    `(24)` = zelig(dale ~ pkoyearsany + pkoyearsany:deathstotal_and_osv_10000 +
        deathstotal_and_osv_10000 + idps_per100000 + helog_knn + hdi_knn +
        civilwarborder + urbangrowth_knn + gini_knn + tropical + xpolity_knn +
        ef_knn + year, data = no_out,
        weights = wno_out, model = "ls", cite = FALSE) %>%
    from_zelig_model()
)

modelsummary(table_6, stars = T,
    title = "TABLE 6: Relevant Cases",
    fmt = "%.2f", coef_map = cm,
    gof_omit = "AIC|Dev|DF|Sigma|Stat|P|Log"
         )

int1 <- 48.7 + (-5.32 * mean(con_pko$ef_knn)) +
    (0.28 * mean(con_pko$xpolity_knn)) -3.53 +
    (0.02 * mean(con_pko$gini_knn)) +
    (0.79 * mean(con_pko$urbangrowth_knn)) + (20.86 * mean(con_pko$hdi_knn)) +
    (0.01 * mean(con_pko$helog_knn))

p1 <- ggplot(data = subset(con_pko, deathstotal_and_osv_10000 == 0), # PKO, no deaths
    aes(x = pkoyearsany, y = dale)) +
    geom_abline(slope = .26, intercept = int1) +
    geom_point(alpha = .4) +
    ylab("Healthy Life Expectancy") +
    xlab("Years with a\nPeacekeeping Operation") +
    theme(text = element_text(family = "serif"))


p2 <- ggplot(data = subset(con_pko, pkoyearsany == 0), # Deaths, no PKO
    aes(x = deathstotal_new, y = dale)) +
    geom_abline(slope = -0.38, intercept = int1) +
    geom_point(alpha = .4) +
    ylab("Healthy Life Expectancy") +
    xlab("Total Violence") +
    theme(text = element_text(family = "serif"))

    int1 <- 48.7 + (-5.32 * mean(con_pko$ef_knn)) +
        (0.28 * mean(con_pko$xpolity_knn)) -3.53 +
        (0.02 * mean(con_pko$gini_knn)) +
        (0.79 * mean(con_pko$urbangrowth_knn)) + (20.86 * mean(con_pko$hdi_knn)) +
        (0.01 * mean(con_pko$helog_knn) - (-.35 * .28))

p3 <- ggplot(data = con_pko, # PKO x Deaths
    aes(x = pkoyearsany, y = dale)) +
    geom_abline(slope = .41, intercept = int3) +
    geom_point(size = log(con_pko$deathstotal_and_osv_10000 + 1), alpha = .4) +
    ylab("Healthy Life Expectancy") +
    xlab("Years with a\nPeacekeeping Operation") +
    theme(text = element_text(family = "serif"))

p1 + p2 + p3
