## Query: select "<a href=""" || joburl || """>" || jobtitle || "</a>" as Pozice, dept as Ministerstvo from 'data' where datetime = (select max(datetime) from data) order by Ministerstvo
## Date query: select max(datetime) as date from data

library(shiny)
library(RCurl)

url_data <- 'https://api.morph.io/petrbouchal/GovJobsCZ/data.csv?key=N4S7F3oGM4jPyicp%2B2mx&query=select%20%22%3Ca%20href%3D%22%22%22%20%7C%7C%20joburl%20%7C%7C%20%22%22%22%3E%22%20%7C%7C%20jobtitle%20%7C%7C%20%22%3C%2Fa%3E%22%20as%20Pozice%2C%20dept%20as%20Ministerstvo%20from%20%27data%27%20where%20datetime%20%3D%20(select%20max(datetime)%20from%20data)%20order%20by%20Ministerstvo'
url_date <- 'https://api.morph.io/petrbouchal/GovJobsCZ/data.csv?key=N4S7F3oGM4jPyicp%2B2mx&query=select%20max(datetime)%20as%20date%20from%20data'

shinyApp(
  ui = fluidPage(
    fluidRow(
      textOutput('counttext'),
      dataTableOutput('data')
    )
  ),
  server=function(input, output) {
    tmpFile <- tempfile()
    tmpFile <- getURL(url_data)
#     download.file(url_data, destfile = tmpFile)
    data <- read.csv(text=tmpFile, fileEncoding = "UTF-8")
    tmpFile2 <- tempfile()
    tmpFile2 <- getURL(url_date)
    date <- read.csv(text=tmpFile2)
    datum <- strptime(date$date, '%Y-%m-%d')
    datum <- strftime(date$date, '%d. %m. %Y')
    deptcount <- length(unique(data$Ministerstvo))
    jobcount <- length(unique(data$Pozice))
    output$counttext <- renderText(paste0(' Nalezeno ',jobcount,' nabidek od ',
                                   deptcount,' uradu. Naposledy zkontrolovano ', datum,'.'))
    output$data <- renderDataTable(data,options = list(lengthChange=F))
  }
)
