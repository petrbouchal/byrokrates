library(pbtools)
source('./platovetabulky/readdata.R')

loadcustomthemes(themecolours, 'Helvetica')

ggplot(ttall, aes(years, value/1e3, colour=tridanum, group=tridanum)) +
  geom_step() +
  scale_y_continuous(labels=comma, limits=c(0,50)) +
  scale_colour_gradient2_tableau(breaks=c(1,16)) +
  facet_wrap(~tablelong) +
  theme(strip.text=element_text(size=14),
        text=element_text(size=13)) +
  guides(color=guide_colourbar(ticks = F, nbin = 16))
