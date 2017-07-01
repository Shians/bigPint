library(plotly)
library(hexbin)
library(htmlwidgets)
library(tidyr)
library(shiny)
library(dplyr)
library(data.table)
library(ggplot2)

# Create dataframe
set.seed(1)
datSel <- data.frame(ID = paste0("ID", 1:100), A = c(10, rnorm(99)), B = c(10, rnorm(99)), stringsAsFactors = FALSE
)

ui <- shinyUI(fluidPage(
  sidebarLayout(
    position = "left",
    sidebarPanel(
      actionButton("goButton", "Go!"),
      width = 3
    ),
    mainPanel(
      verbatimTextOutput("info"), #Testing
      verbatimTextOutput("info2"), #Testing
      verbatimTextOutput("info3"), #Testing
      verbatimTextOutput("info4"), #Testing
      plotlyOutput("hexPlot"),
      plotlyOutput("scatterPlot"),
      width = 9
    )
  )
))

server <- shinyServer(function(input, output, session) {
  
  # Each time the user clicks the goButton, we move to next row of dataframe
  idNum <- reactiveValues(x=0)
  observeEvent(input$goButton, idNum$x <- idNum$x + 1)
  currID <- eventReactive(idNum$x, {as.character(datSel[idNum$x,]$ID)})
  currNum <- eventReactive(currID(), {unname(unlist(datSel[which(datSel$ID == currID()), -1]))})
  
  # Testing
  output$info <- renderPrint({currID()})
  output$info2 <- renderPrint({currNum()})
  
  # Create plotlyHex object, and then add orange point with the x and y value of the current gene (row) selected
  output$hexPlot <- renderPlotly({
    
    minVal = min(datSel[,-1])
    maxVal = max(datSel[,-1])
    maxRange = c(minVal, maxVal)
    xbins= 10
    buffer = (maxRange[2]-maxRange[1])/(xbins/2)
    x <- unlist(datSel[,(2)])
    y <- unlist(datSel[,(3)])
    h <- hexbin(x=x, y=y, xbins=xbins, shape=1, IDs=TRUE, xbnds=maxRange, ybnds=maxRange)
    hexdf <- data.frame (hcell2xy (h),  hexID = h@cell, counts = h@count)
    attr(hexdf, "cID") <- h@cID
    p <- ggplot(hexdf, aes(x=x, y=y, fill = counts, hexID=hexID)) + geom_hex(stat="identity") + geom_abline(intercept = 0, color = "red", size = 0.25) + coord_cartesian(xlim = c(maxRange[1]-1*buffer, maxRange[2]+buffer), ylim = c(maxRange[1]-1*buffer, maxRange[2]+buffer)) + coord_equal(ratio = 1)
    plotlyHex <- ggplotly(p)
    
    # Use onRender() function to draw x and y values of selected row as orange point
    plotlyHex %>% onRender("
     function(el, x, data) {
     noPoint = x.data.length;
     Shiny.addCustomMessageHandler('points', function(drawPoints) {
     if (x.data.length > noPoint){
     Plotly.deleteTraces(el.id, x.data.length-1);
     }
     var Traces = [];
     var trace = {
     x: drawPoints.valX,
     y: drawPoints.valY,
     mode: 'markers',
     marker: {
     color: 'orange',
     size: 7
     },
     hoverinfo: 'none'
     };
     Traces.push(trace);
     Plotly.addTraces(el.id, Traces);
     });}")
  })
  
  # Create plotlyScatter object, and then add orange point with the x and y value of the current gene (row) selected
  output$scatterPlot <- renderPlotly({
    
    x <- unlist(datSel[,(2)])
    y <- unlist(datSel[,(3)])
    
    p2 <- qplot(x, y)
    plotlyScatter <- ggplotly(p2)
    
    # Use onRender() function to draw x and y values of selected row as orange point
    plotlyScatter %>% onRender("
     function(el, x, data) {
     noPoint = x.data.length;
     Shiny.addCustomMessageHandler('points', function(drawPoints) {
     if (x.data.length > noPoint){
     Plotly.deleteTraces(el.id, x.data.length-1);
     }
     var Traces = [];
     var trace = {
     x: drawPoints.valX,
     y: drawPoints.valY,
     mode: 'markers',
     marker: {
     color: 'orange',
     size: 7
     },
     hoverinfo: 'none'
     };
     Traces.push(trace);
     Plotly.addTraces(el.id, Traces);
     });}")
  })
  
  # Send message with x and y value to both onRender() functions called above (one for the scatterplot the other for the hexagon binning plot)
  observe({
    valX <- currNum()[1]
    valY <- currNum()[2]
    output$info3 <- renderPrint({valX})
    output$info4 <- renderPrint({valY})
    session$sendCustomMessage(type = "points", message=list(valX=valX, valY=valY))
  })
})

shinyApp(ui, server)