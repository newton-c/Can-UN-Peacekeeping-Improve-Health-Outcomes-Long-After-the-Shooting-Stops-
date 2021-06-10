
theme_set(theme_classic())

m23 <- zelig(dale ~ deathstotal_and_osv_10000 + idps_per100000 +
    helog_knn + hdi_knn + civilwarborder + urbangrowth_knn + gini_knn +
    tropical + xpolity_knn + ef_knn + priorpko + year +
    pkoyearsany, data = no_outliers_matched, weights = no.weights,
    model = "ls", cite = FALSE) %>%
    setx(pkoyearsany = seq(0, 10, by = 1),
        deathstotal_and_osv_10000 =
            mean(no_outliers_matched$deathstotal_and_osv_10000, na.rm = TRUE),
        idps_per100000 = mean(no_outliers_matched$idps_per100000, na.rm = TRUE),
        helog_knn = mean(no_outliers_matched$helog_knn, na.rm = TRUE),
        hdi_knn = mean(no_outliers_matched$hdi_knn, na.rm = TRUE),
        civilwarborder = 0,
        urbangrowth_knn = mean(no_outliers_matched$urbangrowth_knn,
            na.rm = TRUE),
        gini_knn = mean(no_outliers_matched$gini_knn, na.rm = TRUE),
        tropical = 1,
        xpolity_knn = mean(no_outliers_matched$xpolity_knn, na.rm = TRUE),
        ef_knn = mean(no_outliers_matched$ef_knn, na.rm = TRUE),
        priorpko = 0,
        year = 2010
        ) %>%
    sim() %>%
    zelig_qi_to_df()

sims_m23 <- qi_slimmer(m23)

ggplot(sims_m23, aes(pkoyearsany, qi_ci_median)) +
    geom_ribbon(aes(ymin = qi_ci_min, ymax = qi_ci_max), alpha = 0.3) +
    geom_line() +
    ylab("Healthy Life Expectancy") +
    xlab("Years with a Peacekeeping Operation") +
    theme(text = element_text(family = "serif"))




sim_pko_years <- function(pko, violence) {
        qi_slimmer(zelig(dale ~ deathstotal_and_osv_10000 + idps_per100000 +
            helog_knn + hdi_knn + civilwarborder + urbangrowth_knn + gini_knn +
            tropical + xpolity_knn + ef_knn + priorpko + year +
            pkoyearsany, data = m.out, weights = m.weights,
            model = "ls", cite = FALSE) %>%
        setx(pkoyearsany = pko,
            deathstotal_and_osv_10000 = violence,
            idps_per100000 = mean(no_outliers_matched$idps_per100000,
                na.rm = TRUE),
            helog_knn = mean(no_outliers_matched$helog_knn, na.rm = TRUE),
            hdi_knn = mean(no_outliers_matched$hdi_knn, na.rm = TRUE),
            civilwarborder = 0,
            urbangrowth_knn = mean(no_outliers_matched$urbangrowth_knn,
                na.rm = TRUE),
            gini_knn = mean(no_outliers_matched$gini_knn, na.rm = TRUE),
            tropical = 1,
            xpolity_knn = mean(no_outliers_matched$xpolity_knn, na.rm = TRUE),
            ef_knn = mean(no_outliers_matched$ef_knn, na.rm = TRUE),
            priorpko = 0,
            year = seq(2000, 2015, by = 5)
            ) %>%
        sim() %>%
        zelig_qi_to_df())
}

View(m.out %>% filter(deathstotal_and_osv_10000 > 0) %>%
    mutate(dt = mean(deathstotal_and_osv_10000)) %>% select(dt) %>% unique())

p0v0 <- sim_pko_years(pko = 0, violence = 0) # no pko, no violence
p10v0 <- sim_pko_years(pko = 10, violence = .28) # 10 yrs pko no violence
p0vMean <- sim_pko_years(pko = 0, violence = 0) # pko, mean violence
p10vMean <- sim_pko_years(pko = 10, violence = .28) # 10 yrs pko mean violence
m.out <- m.out %>%
    group_by(year) %>%
    mutate(dale_yearly_mean = mean(dale))
    
ggplot() +
    geom_point(m.out, mapping = aes(x = year, y = dale_yearly_mean)) +
    geom_point(p0v0, mapping = aes(x = year, y = qi_ci_median),
        color = "blue") +
    geom_line(p0v0, mapping = aes(x = year, y = qi_ci_median,
        group = pkoyearsany), color = "blue", alpha = .7) +
    geom_point(p10v0, mapping = aes(x = year, y = qi_ci_median),
        color = "blue") +
    geom_line(p10v0, mapping = aes(x = year, y = qi_ci_median,
        group = pkoyearsany), color = "blue", alpha = .7) +
    geom_point(p0vMean, mapping = aes(x = year, y = qi_ci_median),
        color = "orange") +
    geom_line(p0vMean, mapping = aes(x = year, y = qi_ci_median,
        group = pkoyearsany), color = "orange", alpha = .7) +
    geom_point(p10vMean, mapping = aes(x = year, y = qi_ci_median),
        color = "orange") +
    geom_line(p10vMean, mapping = aes(x = year, y = qi_ci_median,
        group = pkoyearsany), color = "orange", alpha = .7)




ggplot() +
    geom_point(subset(sims_m23_00, pkoyearsany == 0),
        mapping = aes(year, qi_ci_median), color = "deepskyblue",
        shape = 15, size = 3) +
    geom_point(subset(sims_m23_00, pkoyearsany == 5),
        mapping = aes(year, qi_ci_median), color = "dodgerblue2",
        shape = 17, size = 3) +
    geom_point(subset(sims_m23_00, pkoyearsany == 10),
        mapping = aes(year, qi_ci_median), color = "dodgerblue4", size = 3) +
    geom_point(subset(sims_m23_05, pkoyearsany == 0),
        mapping = aes(year, qi_ci_median), color = "deepskyblue",
        shape = 15, size = 3) +
    geom_point(subset(sims_m23_05, pkoyearsany == 5),
        mapping = aes(year, qi_ci_median), color = "dodgerblue2",
        shape = 17, size = 3) +
    geom_point(subset(sims_m23_05, pkoyearsany == 10),
        mapping = aes(year, qi_ci_median), color = "dodgerblue4", size = 3) +
    geom_point(subset(sims_m23_10, pkoyearsany == 0),
        mapping = aes(year, qi_ci_median), color = "deepskyblue",
        shape = 15, size = 3) +
    geom_point(subset(sims_m23_10, pkoyearsany == 5),
        mapping = aes(year, qi_ci_median), color = "dodgerblue2",
            shape = 17, size = 3) +
    geom_point(subset(sims_m23_10, pkoyearsany == 10),
        mapping = aes(year, qi_ci_median), color = "dodgerblue4", size = 3) +
    geom_point(subset(sims_m23_15, pkoyearsany == 0),
        mapping = aes(year, qi_ci_median), color = "deepskyblue",
            shape = 15, size = 3) +
    geom_point(subset(sims_m23_15, pkoyearsany == 5),
        mapping = aes(year, qi_ci_median), color = "dodgerblue2",
        shape = 17, size = 3) +
    geom_point(subset(sims_m23_15, pkoyearsany == 10),
        mapping = aes(year, qi_ci_median), color = "dodgerblue4", size = 3) +
    geom_line(subset(sims_m23_all, pkoyearsany == 0),
        mapping = aes(year, qi_ci_median, group = pkoyearsany),
        color = "deepskyblue") +
    geom_line(subset(sims_m23_all, pkoyearsany == 5),
        mapping = aes(year, qi_ci_median, group = pkoyearsany),
        color = "dodgerblue2") +
    geom_line(subset(sims_m23_all, pkoyearsany == 10),
        mapping = aes(year, qi_ci_median, group = pkoyearsany),
        color = "dodgerblue4") +
    ylab("Healthy Life Expectancy") +
    xlab("Year") +
    theme(text = element_text(family = "serif"))

ggsave("figs/simulated_year.png", height = 3.5, width = 6)

# 10 years peacekeeping with CI
ggplot() +
geom_point(subset(sims_m23_00, pkoyearsany == 10),
    mapping = aes(year, qi_ci_median), color = "dodgerblue4", size = 3) +
geom_point(subset(sims_m23_05, pkoyearsany == 10),
    mapping = aes(year, qi_ci_median), color = "dodgerblue4",size = 3) +
geom_point(subset(sims_m23_10, pkoyearsany == 10),
    mapping = aes(year, qi_ci_median), color = "dodgerblue4", size = 3) +
geom_point(subset(sims_m23_15, pkoyearsany == 10),
    mapping = aes(year, qi_ci_median), color = "dodgerblue4", size = 3) +
geom_line(subset(sims_m23_all, pkoyearsany == 10),
    mapping = aes(year, qi_ci_median, group = pkoyearsany),
    color = "dodgerblue4") +
geom_line(subset(sims_m23_all, pkoyearsany == 10),
    mapping = aes(year, qi_ci_max, group = pkoyearsany),
    color = "grey", linetype = 2) +
geom_line(subset(sims_m23_all, pkoyearsany == 10),
    mapping = aes(year, qi_ci_min, group = pkoyearsany),
    color = "grey", linetype = 2) +
ylab("Healthy Life Expectancy") +
xlab("Year") +
theme(text = element_text(family = "serif"))
