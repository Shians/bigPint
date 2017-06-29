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
myPairs <- c("A", "B", "C", "D")

ui <- shinyUI(fluidPage(
  titlePanel("title panel"),
  sidebarLayout(
    position = "left",
    sidebarPanel(
      selectizeInput("selPair", "Pairs:", choices = myPairs, multiple = TRUE,
                     options = list(maxItems = 2)),
      actionButton("goButton", "Go!"),
      width = 3
    ),
    mainPanel(
      verbatimTextOutput("info"),
      plotlyOutput("scatMatPlot")
    )
  )
))

server <- shinyServer(function(input, output, session) {
  
  geneNum <- reactiveValues(x=0)
  observeEvent(input$goButton, geneNum$x <- geneNum$x + 1)
  observeEvent(input$selPair, geneNum$x <- 0)
  
  # Create data and subsets of data based on user selection of pairs
  dat <- data.frame(
    ID = paste0("ID", 1:10000), A = c(4, rnorm(9999)),
    B = c(4, rnorm(9999)), C = c(4, rnorm(9999)), D = c(4, rnorm(9999)),
    stringsAsFactors = FALSE
  )
  
  # Create data subset based on two letters user chooses
  datSel <- eventReactive(input$selPair, {
    validate(need(length(input$selPair) == 2, "Select a pair."))
    dat[c("ID", input$selPair)]
  }, ignoreNULL = FALSE)
  
  # Create background Plotly graph with hex binning all 100 rows of the two user-selected columns
  ggPS <- eventReactive(datSel(), {
    minVal = min(datSel()[,-1])
    maxVal = max(datSel()[,-1])
    maxRange = c(minVal, maxVal)
    xbins=7
    buffer = (maxRange[2]-maxRange[1])/xbins/2
    x = unlist(datSel()[input$selPair[1]])
    y = unlist(datSel()[input$selPair[2]])
    h <- hexbin(x=x, y=y, xbins=xbins, shape=1, IDs=TRUE,
                xbnds=maxRange, ybnds=maxRange)
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
  
  # Output ID of selected row
  output$info <- renderPrint({ datSel()$ID[req(geneNum$x)] })
  
  # Output hex bin plot created just above
  output$scatMatPlot <- renderPlotly({
    # Use onRender() function to draw x and y values of seleced row as orange point
    ggPS() %>% onRender("
      function(el, x, data) {
      noPoint = x.data.length;
      Shiny.addCustomMessageHandler('points', function(drawPoints) {
      if (x.data.length > noPoint){
      Plotly.deleteTraces(el.id, x.data.length-1);
      }
      var Traces = [];
      var trace = {
      x: drawPoints.slice(0, drawPoints.length/2),
      y: drawPoints.slice(drawPoints.length/2, drawPoints.length),
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
    currGene <- datSel()[geneNum$x, -1]
    # Send x and y values of selected row into onRender() function
    session$sendCustomMessage(type = "points", unname(unlist(currGene)))
  })
  })

shinyApp(ui, server)