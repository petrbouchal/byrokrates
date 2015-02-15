library(pbtools)
library(XML)
library(car)

x = readHTMLTable('./platovetabulky/kupnisila_tabulky20150215.html',header = TRUE)
t1 <- x[[1]]
# source: http://kupnisila.cz/platove-tridy-tabulky/

tableorder <- c(4,3,9,7,5,8,6,2,1)

for (i in 1:9) {
  tt <- x[[i]]
  print(paste0('Table number', i))
  names(tt) <- c('stupen','praxe',paste0('trida',c(1:(length(tt)-2))))
  tt <- tt[-1,]
  print(names(tt))
  ttm <- melt(tt, id.vars=c('stupen','praxe'),variable.name = 'trida')
  ttm$table <- paste('Tabulka', tableorder[i])
  if(i==1) {
    ttall <- ttm
  } else {
    ttall <- rbind(ttall, ttm)
  }
  ttall
}

# Process

ttall$value <- str_replace_all(ttall$value, ' ','')
ttall$value <- as.numeric(ttall$value)
ttall$stupen <- as.numeric(as.character(ttall$stupen))

ttall$years <- as.numeric(str_match(ttall$praxe, '[0-9]+'))

ttall$tridanum <- as.numeric(str_match(ttall$trida,'[0-9]+'))

ttall <- ttall %>% group_by(table, trida) %>% mutate(years = lag(years, default = 0))

ttall$tablelong <- recode(ttall$table,
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
