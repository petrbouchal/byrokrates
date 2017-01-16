library(pbtools)

dq <- read.csv('./data-input/nkuszu_dataquality.csv')

dq <- dq[dq$X!='',]
names(dq)[1] <- 'Udaj'
names(dq) <- str_replace(names(dq),'X','')
names(dq) <- str_replace(names(dq),'\\.','')

dqm <- melt(dq, id.vars = 'Udaj')
dqm$value <- as.character(dqm$value)
dqm$value <- str_trim(dqm$value)
dqm$value[dqm$value=='x'] <- 'Chybí'
dqm$value[dqm$value=='č'] <- 'Částečně'
dqm$value[dqm$value==''] <- NA
dqm$value[is.na(dqm$value)] <- 'Uvedeno'

kapmin <- read.csv('./data-input/KapitolyMinisterstva.txt')
dqm <- merge(dqm,kapmin, by.x='variable',by.y='KapNum')

dqm$value <- factor(dqm$value, levels = c('Chybí','Částečně','Uvedeno'))

dqm_udaj <- dqm %>% group_by(Udaj, value) %>%
  summarise(count=n())
dqm_kapitola <- dqm %>%
  cast(KapAbb + KapName + variable + Ministerstvo ~ value,fun.aggregate = 'length') %>%
  mutate(counterror=Chybí + Částečně,
         countmissing=Chybí) %>%
  melt(id.vars = c('KapAbb', 'KapName', 'variable',
                   'counterror','countmissing','Ministerstvo'),
       variable_name = 'outcome') %>%
  arrange(counterror)

dqm_kapitola$KapAbb <- factor(dqm_kapitola$KapAbb,
                                 levels = dqm_kapitola$KapAbb[order(dqm_kapitola$counterror,
                                                                    dqm_kapitola$countmissing)])

loadcustomthemes(mycols = themebasecolours, 'Helvetica')

 ggplot(dqm_udaj, aes(x=Udaj, y=count, fill=value)) +
   geom_bar(position='stack', stat='identity') +
   coord_flip()

ggplot(dqm_kapitola, aes(x=KapAbb, y=value, fill=outcome, alpha=Ministerstvo),
       colour='white') +
  geom_bar(position='fill', stat='identity') +
  scale_fill_manual(values = c('red','orange','lightgreen')) +
  scale_y_continuous(expand=c(0,0), labels=percent) +
  scale_alpha_discrete(range = c(0.5,1), guide='none') +
  theme(panel.gri=element_blank()) +
  coord_flip()



