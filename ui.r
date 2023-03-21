library(shiny)

source('server.r')

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput("select", "Select Input", 
                  choices = c("DCCM", "dDCCM", "Solvent")
      ),
      conditionalPanel(condition = "input.select == 'DCCM'",
                       fileInput("file_wt", "Upload WT Data"),
                       textInput('delta','Enter your level delta value'),
                       textInput('resnum','Enter modified residue number'),
                       actionButton("perform_dccm", "Perform")
      ),
      conditionalPanel(condition = "input.select == 'dDCCM'",
                       fileInput("file_dif", "Upload WT Data"),
                       fileInput("file_mod", "Upload PTM Data"),
                       textInput('delta_dif','Enter your level delta value'),
                       textInput('resnum','Enter modified residue number'),
                       actionButton("perform_ddccm", "Perform")
      ),
      conditionalPanel(condition = "input.select == 'Solvent'",
                       fileInput("file_solv", "Upload Coordinate File"),
                       textInput('bw','Enter your bandwith value'),
                       textInput('lowl', 'Enter low KDE limits'),
                       textInput('highl', 'Enter high KDE limits'),
                       textInput('bins', 'Number of Bins'),
                       sliderInput("slider", "Adjust KDE Levels", min = 0, max = 1, value = 1),
                       actionButton("perform_solv", "Perform")
      )
    ),
    mainPanel(
      plotOutput("plot")
    )
  )
)