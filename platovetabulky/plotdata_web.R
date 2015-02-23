source('./platovetabulky/readdata.R')

library("pbtools")
library(rCharts)

rr <- rPlot(value ~ years | tablelong, color = 'tridanum', data=ttall, type='line')
rr$facet(rows=3)
rr

# htmlwidgets

library("htmlwidgets")
library("metricsgraphics")

# Sources
# https://github.com/hrbrmstr/metricsgraphics
# http://hrbrmstr.github.io/metricsgraphics/
# http://rud.is/b/2015/02/01/new-release-0-7-of-metricsgraphics-htmlwidget-grids-rollovers/

lapply(1:7, function(x) {
  mjs_plot(rnorm(10000, mean=x/2, sd=x), width=250, height=250, linked=TRUE) %>%
    mjs_line(interpolate = 'step')
    mjs_labs(x_label=sprintf(x))
}) -> plots


mjs_grid(plots)
