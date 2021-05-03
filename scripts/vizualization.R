library(DiagrammeR)
library(haven)
library(tidyverse)
library(ggmap)
library(maps)
library(mapdata)
library(countrycode)
library(patchwork)


theme_set(theme_classic())

# Map --------------------------------------------------------------------------
dat <- read_dta("data/standardized_data.dta")
world_map <- map_data("worldHires")
world_map$ccode <- countrycode(world_map$region, "country.name", "gwn")
dat_all <- dat %>%
    select(ccode, dale, year, pkoyearsany, intensity) %>%
    right_join(world_map, by = "ccode") %>%
    filter(!is.na(year))
capitals <- read_csv("data/capitals.csv")
capitals <- capitals[-c(2)]
capitals$ccode <- countrycode(capitals$Country, "country.name", "gwn")

dat_all <- right_join(capitals, dat_all, by = "ccode")
dat_all$pko <- ifelse(dat_all$pkoyearsany > 0, 1, NA)
dat_all$intensity <- ifelse(dat_all$intensity > 1, 1, NA)

dat_all$pko_war_shape <- ifelse(is.na(dat_all$intensity) &
        is.na(dat_all$pko), 0,
    ifelse(dat_all$intensity == 1 & is.na(dat_all$pko), 1,
    ifelse(is.na(dat_all$intensity) & dat_all$pko == 1, 2,
    ifelse(dat_all$intensity == 1 & dat_all$pko == 1, 3, NA))))

table(dat_all$pko_war_shape)
dat_all$dale_c <- dat_all$dale - mean(dat_all$dale, na.rm = T)

ggplot() +
    geom_polygon(data = dat_all,
                 aes(x = long,
                     y = lat,
                     group = group,
                     fill = dale), color = "black", size = .1) +
    scale_fill_gradient(low = "black", high = "white") +
    geom_point(data = dat_all,
               aes(x = Longitude,
                   y = Latitude,
                   color = as.factor(pko_war_shape),
                   shape = as.factor(pko_war_shape)),
                   fill = as.factor(dat_all$pko_war_shape),
                    size = dat_all$pko_war_shape / dat_all$pko_war_shape) +
    coord_fixed(1.3) +
    scale_color_manual(breaks = c(0, 1, 2, 3),
                       values = c('white', 'orange', 'green', 'pink'),
                       labels = c('', 'War', 'Peacekeeping', 'War and \nPeacekeeping '),
                       limits = c(1, 2, 3)) +
    scale_shape_manual(breaks = c(0, 1, 2, 3),
                       values = c(4, 17, 3, 19),
                       labels = c('', 'War', 'Peacekeeping', 'War and \nPeacekeeping '),
                       limits = c(1, 2, 3)) +
    labs(x = "", y = "", fill = "Healthy Life \nExpectancy",
        color = NULL, shape = NULL) +
    facet_wrap(~year, nrow = 2) +
    theme(text = element_text(family = 'serif'),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "right",
        legend.key.size = unit(.5, 'cm')
    )

ggsave("figs/map.png", height = 8, width = 12)


# Chad -------------------------------------------------------------------------
s <- read_dta("data/standardized_data.dta")

#  Change in DALE for Chad, pre and post PKO deployment.
s <- s %>%
    group_by(year) %>%
    mutate("dale_cent" = dale - mean(dale)) # relative to yearly DALE average

# Dot and line plot.
chad <- ggplot() +
    geom_line(subset(s, ccode == 483 & year > 2005),
        mapping = aes(x = year, y = dale_cent), size = 1, alpha = .9,
        linetype = 1, color = 'darkblue') +
    geom_point(subset(s, ccode == 483 & year > 2005),
        mapping = aes(x = year, y = dale_cent), size = 3, alpha = 1,
        color = 'darkblue') +
    geom_line(subset(s, ccode == 483 & year < 2010),
        mapping = aes(x = year, y = dale_cent), size = 1, alpha = .9,
        linetype = 1, color = 'darkred') +
    geom_point(subset(s, ccode == 483 & year < 2010),
        mapping = aes(x = year, y = dale_cent), size = 3, alpha = 1,
        color = 'darkred') +
    geom_vline(aes(xintercept = 2007), linetype = 2) +
    xlab('Year') +
    ylab("Healthy Life Expectancy\n(compared to global average)") +
    theme(text = element_text(family = "serif"),
        axis.text = element_text(family = "serif")) +
    annotate(geom = 'text', x = 2007.5, y = -16.0, hjust = 0,
        label = 'Peacekeeping deployed', family = "serif")

ggsave("figs/chad.png", chad, height = 4, width = 6)


# DAGs -------------------------------------------------------------------------
grViz("
    digraph {
        graph [ranksep = 0.2]
        node [shape = plaintext]
            W [label = 'War']
            Y [label = 'Life Expectancy']
            P [label = 'Peacekeeping']
        edge [minlen = 4]
            W -> Y
            P -> W
            P -> Y
        {rank = same; P; Y}
}
")

# adding economics
grViz("
    digraph {
        graph [ranksep = 0.2]
        node [shape = plaintext]
            W [label = 'War']
            Y [label = 'Life Expectancy']
            P [label = 'Peacekeeping']
            E [label = 'Economy']
        edge [minlen = 4]
            W -> Y [label = '  -  ']
            P -> W [label = '  -  ']
            P -> Y[label = '  +  ']
            W -> E [label = '  -  ']
            E -> Y [label = '  -  ']
        {rank = same; P; Y}
        {rank = same; W; E}
}
")


# All conflicts with and without pkoyears --------------------------------------
ggplot() +
    geom_point(subset(s, intensity > 0 & pkoyearsany == 0),
        mapping = aes(x = deathstotal_new, y = dale_s,
        size = osv/10000), color = "darkred", alpha = .3) +
    geom_smooth(subset(s, intensity > 0 & pkoyearsany == 0),
        mapping = aes(x = deathstotal_new, y = dale_s), method = "lm",
        color = "darkred") +
    geom_point(subset(s, intensity > 0 & pkoyearsany > 0),
        mapping = aes(x = deathstotal_new, y = dale_s,
        size = osv/10000), color = "darkblue", alpha = .3) +
    geom_smooth(subset(s, intensity > 0 & pkoyearsany > 0),
        mapping = aes(x = deathstotal_new, y = dale_s), method = "lm")

ggplot() +
    geom_point(subset(s, deathstotal_new > 0 & pkoyearsany == 0),
        mapping = aes(x = deathstotal_new, y = dale_s), color = "darkred") +
    geom_point(subset(s, deathstotal_new > 0 & pkoyearsany > 0),
        mapping = aes(x = deathstotal_new, y = dale_s), color = "darkblue")


only_pkos <- filter(no_outliers_matched, pkoyearsany > 0)

# Model 22 slopes plotted with data --------------------------------------------
int1 <- 45.13 + 4.01 + (-4.78 * mean(m.out$ef_knn)) +
    (0.16 * mean(m.out$xpolity_knn)) + (-0.04 * mean(m.out$gini_knn)) +
    (-0.5 * mean(m.out$urbangrowth_knn)) + (14.47 * mean(m.out$hdi_knn)) +
    (1.74 * mean(m.out$helog_knn))

p1 <- ggplot(data = subset(m.out, deathstotal_new == 0), # PKO, no deaths
    aes(x = pkoyearsany, y = dale)) +
    geom_abline(slope = .23, intercept = int1) +
    geom_point(alpha = .4) +
    ylab("Healthy Life Expectancy") +
    xlab("Years with a\nPeacekeeping Operation") +
    theme(text = element_text(family = "serif"))


p2 <- ggplot(data = subset(m.out, pkoyearsany == 0), # Deaths, no PKO
    aes(x = deathstotal_new, y = dale)) +
    geom_abline(slope = -0.38, intercept = int1) +
    geom_point(alpha = .4) +
    ylab("Healthy Life Expectancy") +
    xlab("Total Violence") +
    theme(text = element_text(family = "serif"))

int3 <- 45.13 + 4.01 + (-4.78 * mean(m.out$ef_knn)) +
    (0.16 * mean(m.out$xpolity_knn)) + (-0.04 * mean(m.out$gini_knn)) +
    (-0.5 * mean(m.out$urbangrowth_knn)) + (14.47 * mean(m.out$hdi_knn)) +
    (1.74 * mean(m.out$helog_knn) - 0.38)

p3 <- ggplot(data = m.out, # PKO x Deaths
    aes(x = pkoyearsany, y = dale)) +
    geom_abline(slope = .38, intercept = int3) +
    geom_point(size = log(m.out$deathstotal_new + 1), alpha = .4) +
    ylab("Healthy Life Expectancy") +
    xlab("Years with a\nPeacekeeping Operation") +
    theme(text = element_text(family = "serif"))

p1 + p2 + p3

ggsave("figs/model22plot.png", height = 3, width = 8)


ggplot() +
    geom_point(subset(m.out, pkoyearsany > 0),
        mapping = aes(x = deathstotal_new, y = dale, size = pkoyearsany),
        alpha = .4, shape = 25,
        fill = "darkblue") +
    geom_point(subset(m.out, pkoyearsany == 0),
        mapping = aes(x = deathstotal_new, y = dale), alpha = .4,
        fill = "darkred") +
#    geom_smooth(subset(m.out, pkoyearsany > 0),
#        mapping = aes(x = deathstotal_new, y = dale), method = "loess",
#        color = "darkblue") +
#    geom_smooth(subset(m.out, pkoyearsany == 0),
#        mapping = aes(x = deathstotal_new, y = dale), method = "loess",
#        color = "darkred") +
    ylab("Healthy Life Expectancy") +
    xlab("Total Violence") +
    theme(text = element_text(family = "serif")) +
    annotate(geom = "text", x = 67, y = 44.5, hjust = 0,
        label = "Ethiopia", family = "serif") +
    annotate(geom = "text", x = 68, y = 53.2, hjust = 0,
            label = "Afghanistan", family = "serif") +
    annotate(geom = "text", x = 1, y = 32.3, hjust = 0,
        label = "Haiti", family = "serif")


# 3D plot.
library(rayshader)
s$deathstotal_new_uc <- s$deathstotal_new + .4

p <- ggplot() +
    geom_point(s,
        mapping = aes(x = pkoyearsany, y = dale_s,
        fill = deathstotal_new_uc))

par(mfrow = c(1, 2))

plot_gg(p, width = 5, height = 5, raytrace = FALSE, preview = TRUE)
plot_gg(p, width = 5, height = 5, multicore = TRUE, scale = 250,
        zoom = 0.7, theta = 10, phi = 30, windowsize = c(800, 800))
Sys.sleep(0.2)

#render_snapshot(clear = TRUE)
