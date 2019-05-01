library(shiny)

ui <- fluidPage ("Hello World")

server <- function(input,output){}

shinyApp(server=server, ui=ui)