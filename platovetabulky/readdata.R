# library(pbtools)
library(reshape2)
library(ggplot2)
library(stringr)
library(grid)
library(scales)
library(ggthemes)
library(XML)
library(car)

p2015 = readHTMLTable('./platovetabulky/kupnisila_tabulky20150215.html',header = TRUE)
p2016a = readHTMLTable('./platovetabulky/kupnisila_tabulky2016a.html',header = T)
p2016b = readHTMLTable('./platovetabulky/kupnisila_tabulky2016b.html',header = T)
# source: http://kupnisila.cz/platove-tridy-tabulky/

# View(p2016b[[1]])

tableorder <- c(4,3,9,7,5,8,6,2,1)

uredtables <- list(p2015[[9]], p2016a[[1]], p2016b[[1]])

uredyears <- c(2014, 2015, 2016)

tableise <- function(data) {

  stopifnot(is.list(data) & is.data.frame(data[[1]]))

  for (i in 1:length(data)) {

    tt <- data[[i]]

    # print(paste0('Table number', i))

    if (uredyears[i]==2014) {
      tridynums <- c(1:16)
      } else {
      tridynums <- c(5:16)
      }

    print(tridynums)
    names(tt) <- c('stupen','praxe',paste0('trida',tridynums))

    if(uredyears[[i]] == 2014) {
      tt <- tt[-1,]
    } else {
      tt <- tt[-c(1,2),]
    }

    # print(names(tt))

    ttm <- melt(tt, id.vars=c('stupen','praxe'),variable.name = 'trida')
    ttm$tabulka <- paste('Tabulka', uredyears[i])
    if(i==1) {
      ttall <- ttm
    } else {
      ttall <- rbind(ttall, ttm)
    }
  }
  ttall
}

uredset <- tableise(uredtables)

# Process ttall

for (i in 1:9) {
  tt <- p2015[[i]]
  # print(paste0('Table number', i))
  names(tt) <- c('stupen','praxe',paste0('trida',c(1:(length(tt)-2))))
  tt <- tt[-1,]
  # print(names(tt))
  ttm <- melt(tt, id.vars=c('stupen','praxe'),variable.name = 'trida')
  ttm$tabulka <- paste('Tabulka', tableorder[i])
  if(i==1) {
    ttall <- ttm
  } else {
    ttall <- rbind(ttall, ttm)
  }
  # ttall
}

cleanup <- function(data) {
  data$value <- str_replace_all(data$value, ' ','')
  data$value <- as.numeric(data$value)
  data$stupen <- as.numeric(as.character(data$stupen))
  data$years <- as.numeric(str_match(data$praxe, '[0-9]+'))
  data$tridanum <- as.numeric(str_match(data$trida,'[0-9]+'))
  data$tabulka <- as.factor(data$tabulka)
  data$years[data$praxe=="nad 32 let"] <- 35
  data$grp <- paste0(data$trida, "_", data$tabulka)
  data
}

ttall <- cleanup(ttall)

ttall <- ttall %>% group_by(tabulka, trida) %>% mutate(years = lag(years, default = 0))

ttall$tablelong <- recode(ttall$tabulka,
                          "'Tabulka 4' = 'Úředníci, strážníci aj.';
                          'Tabulka 3' = 'Sociální pracovníci';
                          'Tabulka 9' = 'Učitelé';
                          'Tabulka 7' = 'Lékaři v sociálním zabezpečení';
                          'Tabulka 5' = 'Lékaři v obraně';
                          'Tabulka 8' = 'Lékaři v záchranné službě aj.';
                          'Tabulka 6' = 'Další zdravotničtí';
                          'Tabulka 2' = 'Ostatní zdravotničtí';
                          'Tabulka 1' = 'Ostatní'")

ttall$tablelong <- as.factor(ttall$tablelong)
ttall$tablelong <- factor(ttall$tablelong, levels(ttall$tablelong)[c(9,8,7,4,3,2,1,6,5)])

# Process uredset

uredset <- cleanup(uredset)
uredset$trida1az5 <- ifelse(uredset$tridanum < 5, T, F)

# save(uredset, file = "platovetabulky/tabulky_jenurednici.Rda")
# save(ttall, file = "platovetabulky/tabulky2015_vsechny.Rda")
