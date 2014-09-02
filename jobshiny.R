## Query: select "<a href=""" || joburl || """>" || jobtitle || "</a>" as Odkaz, dept as Ministerstvo from 'data' limit 10
## Link : https://api.morph.io/petrbouchal/GovJobsCZ/data.csv?key=N4S7F3oGM4jPyicp%2B2mx&query=select%20%22%3Ca%20href%3D%22%22%22%20%7C%7C%20joburl%20%7C%7C%20%22%22%22%3E%22%20%7C%7C%20jobtitle%20%7C%7C%20%22%3C%2Fa%3E%22%20as%20Odkaz%2C%20dept%20as%20Ministerstvo%20from%20%27data%27%20limit%20200

library(shiny)

shinyApp(
  ui = fluidPage(
    fluidRow(
      dataTableOutput('x')
        )
      ),
  server=function(input, output) {
    output$x <- renderDataTable(data.frame(html = '<a href=\"http://www.google.com\">Google</a>',blah = 'blah' ))
  }
  )
