library(ggplot2)
library(dplyr)
library(grid)
library(scales)
library(ggthemes)

load("platovetabulky/tabulky_jenurednici.Rda")
load("platovetabulky/tabulky2015_vsechny.Rda")


# pbtools::loadcustomthemes(pbtools::themecolours, 'Helvetica')

# All tables in 2015
ggplot(ttall, aes(years, value/1e3, colour=tridanum, group=tridanum)) +
  geom_step() +
  scale_y_continuous(labels=comma, limits=c(0,50)) +
  scale_colour_continuous(breaks=c(1,16)) +
  facet_wrap(~tablelong) +
  theme(strip.text=element_text(size=14),
        text=element_text(size=13)) +
  guides(color=guide_colourbar(ticks = F, nbin = 16))

# Urednik tables by year
ggplot(data = uredset, aes(x=years, y=value/1e3, colour = tridanum, group = grp)) +
  geom_step(aes(linetype = trida1az5)) +
  facet_wrap(~ tabulka) +
  scale_y_continuous(labels=comma, limits=c(0,60),
                     breaks = c(0,10,20,30,40,50,60)) +
  # scale_colour_gradient2_tableau(breaks=c(1,16)) +
  theme(strip.text=element_text(size=14),
        text=element_text(size=13)) +
  guides(color=guide_colourbar(ticks = F, nbin = 16)) +
  scale_linetype_manual(values = c("TRUE" = "dotted", "FALSE" = "solid"))

library(dplyr)

# How much pay progression is there in each grade in the three points in time?

uredset %>%
  mutate(tridanum = as.factor(tridanum)) %>%
  group_by(tabulka, tridanum) %>%
  summarise(narust = max(value)/min(value)-1) %>%
  ggplot(aes(tridanum, narust)) + geom_col() + facet_wrap(~ tabulka) +
  scale_y_continuous(labels = percent) +
  theme_hc()

# How much difference between lowest and highest grade at identical levels of experience?

uredset %>%
  filter(!trida1az5) %>%
  mutate(stupen = as.factor(years)) %>%
  group_by(tabulka, years) %>%
  summarise(rozdil = max(value)/min(value)-1) %>%
  ggplot(aes(years, rozdil)) + geom_col() + facet_wrap(~ tabulka)
