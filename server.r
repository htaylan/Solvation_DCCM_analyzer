library("ggplot2")
library("lattice")
require("mgcv")
library("MASS")
library('viridis')
library('plot3D')
library(gdata)
require(plotrix)
require(grid)
library(bio3d)

server <- function(input, output) {
  
  options(shiny.maxRequestSize=30*1024^2)
  
  toListen <- reactive({
    list(input$file_solv,input$perform_solv)
  })  
  
  observeEvent(input$perform_solv, {
    data1 = read.csv(input$file_solv$datapath, sep = " ", header=TRUE)
    nbins=as.numeric(input$bins)
    lowxy= as.numeric(input$lowl)
    highxy=as.numeric(input$highl)
    limits=c(lowxy,highxy,lowxy,highxy)
    bw=as.numeric(input$bw)
    x.bin=seq(lowxy,highxy,length=nbins)
    y.bin=seq(lowxy,highxy,length=nbins)
    lvls= c(0.1,0.25,0.5,0.75,0.9)
    lvls = as.numeric(input$slider) * lvls
    kernel_calculated = kde2d(data1$X,data1$Y, n=nbins, h=bw, lims=limits)
    output$plot <- renderPlot({
      lvl= max(kernel_calculated$z) *lvls
      contour(x.bin,y.bin,kernel_calculated$z,xlab="X-Axis", ylab="Y-Axis", drawlabels = FALSE,
              xaxt='n', levels = lvl, col =  magma(length(lvls)))
    })
  })
  
  observeEvent(input$perform_dccm, {
    traj_wt = read.dcd(input$file_wt$datapath)
    delta = as.numeric(input$delta)
    resnum = as.numeric(input$resnum)
    dcplot1 = dccm(traj_wt)
    x <- 1:nrow(dcplot1)
    y <- 1:ncol(dcplot1)
    
    #lvl=c(-0.75,-0.50,-0.25,0,0.25,0.50,0.75)
    
    level = seq(-1,1,delta)
    
    mycols = c('red', 'red','orange','white','white','blue','purple', 'purple')
    
    output$plot <- renderPlot({
      
      filled.contour(x, y, dcplot1, col=mycols, #cols=mycols,
                     plot.title = title(xlab = "Residue No.", ylab = "Residue No."),
                     plot.axes = { axis(1, seq(0, max(x), by = 20))
                       axis(2, seq(0, max(x), by = 20)) 
                       lines(c(resnum,resnum), c(0,max(x)), lty=2, lwd=2, col = "black")
                       lines(c(0,max(x)),c(0,max(x)),  lty=2, lwd=2, col = "black")},
                     levels=level,key.axes = axis(4, seq(-1, 1, by = delta)))  
    })
  })
  
  observeEvent(input$perform_ddccm, {
    traj_wt = read.dcd(input$file_dif$datapath)
    traj_mod = read.dcd(input$file_mod$datapath)
    delta = as.numeric(input$delta_dif)
    resnum = as.numeric(input$resnum)
    dcplot1 = dccm(traj_wt)
    dcplot2 = dccm(traj_mod)
    x <- 1:nrow(dcplot1)
    y <- 1:ncol(dcplot1)
    diff=dcplot1-dcplot2
    #lvl=c(-0.75,-0.50,-0.25,0,0.25,0.50,0.75)
    
    level = seq(-1,1,delta)
    
    mycols = c('red', 'red','orange','white','white','blue','purple', 'purple')
    
    output$plot <- renderPlot({
      
      filled.contour(x, y, diff, col=mycols, #cols=mycols,
                     plot.title = title(xlab = "Residue No.", ylab = "Residue No."),
                     plot.axes = { axis(1, seq(0, max(x), by = 20))
                       axis(2, seq(0, max(x), by = 20)) 
                       lines(c(resnum,resnum), c(0,max(x)), lty=2, lwd=2, col = "black")
                       lines(c(0,max(x)),c(0,max(x)),  lty=2, lwd=2, col = "black")},
                     levels=level,key.axes = axis(4, seq(-1, 1, by = delta)))  
    })
  })
}

#shinyApp(ui, server)