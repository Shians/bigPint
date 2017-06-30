library(plotly)
library(GGally)
library(hexbin)
library(htmlwidgets)
library(tidyr)
library(shiny)
library(dplyr)
library(data.table)
library(ggplot2)
library(tibble)

set.seed(1)

# Create data and subsets of data based on user selection of pairs
dat <- data.frame(ID = paste0("ID", 1:100), A.1 = c(10, rnorm(99)), A.2 = c(10, rnorm(99)), B.1 = c(6, rnorm(99)), B.2 = c(6, rnorm(99)), C.1 = c(-6, rnorm(99)), C.2 = c(-6, rnorm(99)), C.3 = c(-6, rnorm(99)), stringsAsFactors = FALSE
)
datCol <- colnames(dat)[-which(colnames(dat) %in% "ID")]
myPairs <- unique(sapply(datCol, function(x) unlist(strsplit(x,"[.]"))[1]))

metrics <- list()
for (i in 1:(length(myPairs)-1)){
  for (j in (i+1):length(myPairs)){
    metrics[[paste0(myPairs[i],"vs",myPairs[j])]] <- data.frame(ID = paste0("ID", 1:100), pValAdj = runif(100, 0, 1), logFC = runif(100, 0, 6), AveExp = runif(100, 0, 60))
  }
}

myMetrics <- colnames(metrics[[1]])[-which(colnames(metrics[[1]]) %in% "ID")]

ui <- shinyUI(fluidPage(
  #titlePanel("title panel"),
  sidebarLayout(
    position = "left",
    sidebarPanel(
      selectizeInput("selPair", "Pairs:", choices = myPairs, multiple = TRUE, options = list(maxItems = 2)),
      selectInput("selMetric", "Metric:", choices = myMetrics),
      selectInput("selOrder", "Order:", choices = c("Increasing", "Decreasing")),
      numericInput("binSize", "Hexagon size:", value = 10),
      actionButton("goButton", "Go!"),
      width = 3
    ),
    mainPanel(
      verbatimTextOutput("info"),
      verbatimTextOutput("info2"),
      plotlyOutput("scatMatPlot", width = "100%", height = "400px", inline=FALSE),
      plotlyOutput("boxPlot"),
      width = 9
    )
  )
))

server <- shinyServer(function(input, output, session) {
  
  geneNum <- reactiveValues(x=0)
  observeEvent(input$goButton, geneNum$x <- geneNum$x + 1)
  observeEvent(input$selPair, geneNum$x <- 0)
  observeEvent(input$selMetric, geneNum$x <- 0)
  observeEvent(input$selOrder, geneNum$x <- 0)
  observeEvent(input$binSize, geneNum$x <- 0) 

  # Create data subset based on two letters user chooses
  datSel <- eventReactive(input$selPair, {
    validate(need(length(input$selPair) == 2, "Select a pair."))
    sampleIndex <- reactive(which(sapply(colnames(dat), function(x) unlist(strsplit(x,"[.]"))[1]) %in% c(input$selPair[1], input$selPair[2])))
    dat[,c(1, sampleIndex())]
  }, ignoreNULL = FALSE)
  
  metricDF <- eventReactive(c(input$selPair, input$selMetric, input$selOrder), {
    metricDF <- metrics[[paste0(input$selPair[1], "vs", input$selPair[2])]]
    if (!is.null(metricDF[[input$selMetric]])){
      metricDF <- metricDF[order(metricDF[[input$selMetric]]),]
      if (input$selOrder == "Decreasing"){
        metricDF <- metricDF[order(-metricDF[[input$selMetric]]),]
      }
    }
    metricDF
  })
  
  currMetric <- eventReactive(geneNum$x, {metricDF()[geneNum$x, ]})
  currID <- eventReactive(currMetric(), {as.character(currMetric()$ID)})
  currGene <- eventReactive(currID(), {unname(unlist(datSel()[which(datSel()$ID == currID()), -1]))})
  
  output$info2 <- renderPrint({ currMetric() })
  
  # Create background Plotly graph with hex binning all 100 rows of the two user-selected columns
  ggPS <- eventReactive(c(datSel(), input$binSize), {
    
    sampleIndex1 <- which(sapply(colnames(datSel()), function(x) unlist(strsplit(x,"[.]"))[1]) %in% c(input$selPair[1]))
    sampleIndex2 <- which(sapply(colnames(datSel()), function(x) unlist(strsplit(x,"[.]"))[1]) %in% c(input$selPair[2]))
    
    minVal = min(datSel()[,-1])
    maxVal = max(datSel()[,-1])
    maxRange = c(minVal, maxVal)
    xbins= input$binSize
    buffer = (maxRange[2]-maxRange[1])/(xbins/2)
    x <- c()
    y <- c()
    for (i in 1:length(sampleIndex1)){
      for (j in 1:length(sampleIndex2)){
        x <- c(x, unlist(datSel()[,(sampleIndex1[i])]))
        y <- c(y, unlist(datSel()[,(sampleIndex2[j])]))
      }
    }
    
    h <- hexbin(x=x, y=y, xbins=xbins, shape=1, IDs=TRUE, xbnds=maxRange, ybnds=maxRange)
    hexdf <- data.frame (hcell2xy (h),  hexID = h@cell, counts = h@count)
    attr(hexdf, "cID") <- h@cID
    p <- ggplot(hexdf, aes(x=x, y=y, fill = counts, hexID=hexID)) +
      geom_hex(stat="identity") + geom_abline(intercept = 0, color = "red", size = 0.25) +
      coord_cartesian(xlim = c(maxRange[1]-1*buffer, maxRange[2]+buffer),
                      ylim = c(maxRange[1]-1*buffer, maxRange[2]+buffer)) +
      coord_equal(ratio = 1) +
      labs(x = input$selPair[1], y = input$selPair[2])
    ggPS <- ggplotly(p)
    ggPS
  })
  
  # Output hex bin plot created just above
  output$scatMatPlot <- renderPlotly({
    # Use onRender() function to draw x and y values of selected row as orange point
    ggPS() %>% onRender("
      function(el, x, data) {
      noPoint = x.data.length;
      Shiny.addCustomMessageHandler('points', function(drawPoints) {
      if (x.data.length > noPoint){
      Plotly.deleteTraces(el.id, x.data.length-1);
      }
      var Traces = [];
      var trace = {
      x: drawPoints.geneX,
      y: drawPoints.geneY,
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
  
  observe({
    #print(geneNum$x)
    # Get x and y values of selected row
    sampleIndex1 <- which(sapply(colnames(datSel()), function(x) unlist(strsplit(x,"[.]"))[1]) %in% c(input$selPair[1]))
    sampleIndex2 <- which(sapply(colnames(datSel()), function(x) unlist(strsplit(x,"[.]"))[1]) %in% c(input$selPair[2]))
    
    geneX <- c()
    geneY <- c()
    for (i in 1:length(sampleIndex1)){
      for (j in 1:length(sampleIndex2)){
        geneX <- c(geneX, currGene()[sampleIndex1[i]-1])
        geneY <- c(geneY, currGene()[sampleIndex2[j]-1])
      }
    }
    # Send x and y values of selected row into onRender() function
    session$sendCustomMessage(type = "points", message=list(geneX=geneX, geneY=geneY))
  })
  
   output$boxPlot <- renderPlotly({
     nVar = reactive(ncol(datSel()))
     colNms <- reactive(colnames(datSel()[, c(2:nVar())]))
     #boxDat <- reactive(datSel()[, c(1:nVar())] %>% gather(key, val, -c(ID)))
     
     boxDat <- eventReactive(datSel(), {
       boxDat <- datSel()[, c(1:nVar())] %>% gather(key, val, -c(ID))
       colnames(boxDat) <- c("ID", "Sample", "Count")
       boxDat
     })
                             
     
     output$info <- renderPrint({str(boxDat())})
     
     BP <- reactive(ggplot(boxDat(), aes(x = Sample, y = Count)) + geom_boxplot())
     ggBP <- reactive(ggplotly(BP()))

     observe({
       session$sendCustomMessage(type = "lines", currGene())
     })
  
     ggBP() %>% onRender("
       function(el, x, data) {
  
       noPoint = x.data.length;
  
       function range(start, stop, step){
       var a=[start], b=start;
       while(b<stop){b+=step;a.push(b)}
       return a;
       };
  
       Shiny.addCustomMessageHandler('lines',
       function(drawLines) {
  
       if (x.data.length > noPoint){
       Plotly.deleteTraces(el.id, x.data.length-1);
       }
  
       var dLength = drawLines.length
  
       var Traces = [];
       var traceLine = {
       x: range(1, dLength, 1),
       y: drawLines,
       mode: 'lines',
       line: {
       color: 'orange',
       width: 1
       },
       opacity: 0.9,
       }
       Traces.push(traceLine);
       Plotly.addTraces(el.id, Traces);
       })
       }")})
  })

shinyApp(ui, server)