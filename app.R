library(shiny)
source('ui.r', local = TRUE)
source('server.r')


shinyApp(
  ui = ui,
  server = server
)