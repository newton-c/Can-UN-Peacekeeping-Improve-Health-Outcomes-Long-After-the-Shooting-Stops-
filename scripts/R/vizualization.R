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

# greyscale map
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
            fill = "white",
            size = 2) +
    coord_fixed(1.3) +
    scale_color_manual(breaks = c(0, 1, 2, 3),
        values = c("white", "black", "black", "black"),
        labels = c("", "War", "Peacekeeping", "War and \nPeacekeeping "),
        limits = c(1, 2, 3)) +
    scale_shape_manual(breaks = c(0, 1, 2, 3),
        values = c(4, 22, 21, 24),
        labels = c("", "War", "Peacekeeping", "War and \nPeacekeeping "),
        limits = c(1, 2, 3)) +
    labs(x = "", y = "", fill = "Healthy Life \nExpectancy",
        color = NULL, shape = NULL) +
    facet_wrap(~year, nrow = 2) +
    theme(text = element_text(family = "serif"),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "right",
        legend.key.size = unit(.5, "cm")
    )

ggsave("figs/map_bw.png", height = 8, width = 12)

# shapes with color map
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
            fill = ifelse(dat_all$pko_war_shape == 1, "red",
                          ifelse(dat_all$pko_war_shape == 2, "blue",
                                 ifelse(dat_all$pko_war_shape == 3, "purple",
                                        "white")
                                 )
                          ),
            size = 2) +
    coord_fixed(1.3) +
    scale_color_manual(breaks = c(0, 1, 2, 3),
        values = c("white", "black", "black", "black"),
        labels = c("", "War", "Peacekeeping", "War and \nPeacekeeping "),
        limits = c(1, 2, 3)) +
    scale_shape_manual(breaks = c(0, 1, 2, 3),
        values = c(4, 22, 21, 24),
        labels = c("", "War", "Peacekeeping", "War and \nPeacekeeping "),
        limits = c(1, 2, 3)) +
    labs(x = "", y = "", fill = "Healthy Life \nExpectancy",
        color = NULL, shape = NULL) +
    facet_wrap(~year, nrow = 2) +
    theme(text = element_text(family = "serif"),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "right",
        legend.key.size = unit(.5, "cm")
    )

ggsave("figs/map_color.png", height = 8, width = 12)


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

