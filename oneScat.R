# This script creates hexbin background based on only those samples and genes. In other words, uses (50,000 * 12) instead of (50,000 * 48)

library(plotly)
library(GGally)
library(hexbin)
library(htmlwidgets)
library(tidyr)
library(shiny)
library(edgeR)
library(EDASeq)
library(dplyr)
library(data.table)
library(ggplot2)
library(DESeq2)
library(tibble)

dds <- readRDS("/Users/lindz/bigPint/tblshoot/data_limma.Rds")
rld <- readRDS("/Users/lindz/bigPint/tblshoot/topGenes_limma.Rds")

myPairs <- levels(dds[[2]]$group)

ui <- shinyUI(fluidPage(
  titlePanel("title panel"),
  
  sidebarLayout(position = "left",
    sidebarPanel(
      selectizeInput("selPair", "Pairs:", choices = myPairs, multiple = TRUE, options = list(maxItems = 2, minItems = 2)),
      actionButton("goButton", "Go!"),
      verbatimTextOutput("test1"),
      width = 3
    ),
    mainPanel(
      fluidRow(
        #splitLayout(cellWidths = c("50%", "50%"), plotlyOutput("scatMatPlot"), plotlyOutput("boxPlot"))
        plotlyOutput("test2")
      )
    )
  )
))

server <- shinyServer(function(input, output, session) {
  
  pairNum <- reactive(input$selPair)
  
  # Change group1 and group2 as needed
  group1 =reactive(pairNum()[1])
  group2 =reactive(pairNum()[2])
  
  output$test1 <- renderPrint({
    str(group1())
  })
  
  sampleIndex <- reactive(which(sapply(colnames(dds[[1]]), function(x) unlist(strsplit(x,"[.]"))[1]) %in% c(group1(), group2())))
  
  bindataSel <- eventReactive(sampleIndex(), {
    bindataSel <- as.data.frame(dds[[1]])[, sampleIndex()]
    bindataSel <- rownames_to_column(bindataSel, "ID")
    setDT(bindataSel, keep.rownames = TRUE)[]
    colnames(bindataSel)[1] <- "ID"
    bindataSel$ID <- as.character(bindataSel$ID)
    as.data.frame(bindataSel) #15314*13
  })
  
  ggPS <- eventReactive(bindataSel(), {
    
    sampleIndex1 <- which(sapply(colnames(bindataSel()), function(x) unlist(strsplit(x,"[.]"))[1]) %in% c(group1()))
    sampleIndex2 <- which(sapply(colnames(bindataSel()), function(x) unlist(strsplit(x,"[.]"))[1]) %in% c(group2()))
    
    #res <- results(dds, contrast=c("treatment",group1(),group2()))
    #degIndex <- reactive(which(res@listData$padj<0.05))
    #resSort <- reactive(res[ order(res[,6]), rm.NA=TRUE])
    resSort <- rownames(rld[[paste0(group1(),"vs",group2())]])
    
    minVal = min(bindataSel()[,-1])
    maxVal = max(bindataSel()[,-1])
    maxRange = c(minVal, maxVal)
    xbins=10
    buffer = (maxRange[2]-maxRange[1])/xbins/2 # Because usually shows at least half of hex?
    
    x = unlist(bindataSel()[,(sampleIndex1)])
    y = unlist(bindataSel()[,(sampleIndex2)])
    h <- hexbin(x=x, y=y, xbins=xbins, shape=1, IDs=TRUE, xbnds=maxRange, ybnds=maxRange)
    hexdf <- data.frame (hcell2xy (h),  hexID = h@cell, counts = h@count)
    attr(hexdf, "cID") <- h@cID
    p <- ggplot(hexdf, aes(x=x, y=y, fill = counts, hexID=hexID)) + geom_hex(stat="identity") + geom_abline(intercept = 0, color = "red", size = 0.25) + coord_cartesian(xlim = c(maxRange[1]-1*buffer, maxRange[2]+buffer), ylim = c(maxRange[1]-1*buffer, maxRange[2]+buffer)) + coord_equal(ratio=1)
    
    ggPS <- ggplotly(p)
    
    myLength <- length(ggPS[["x"]][["data"]])
    for (i in 1:myLength){
      hexHover = ggPS[["x"]][["data"]][[i]]$text
      if (!is.null(hexHover) && grepl("hexID", hexHover)){
        ggPS[["x"]][["data"]][[i]]$text <- strsplit(hexHover, "<")[[1]][1]
        ggPS[["x"]][["data"]][[i]]$t2 <- hexHover
        ggPS[["x"]][["data"]][[i]]$hoverinfo <- "text"
      }
    }
    ggPS})
  
  
  
  output$scatMatPlot <- renderPlotly({  
    
    datInput <- eventReactive(input$goButton, {
      print(input$goButton)
      # Chose particular gene
      g <- resSort[input$goButton]
      currGene <- bindataSel[which(bindataSel$ID==g),]
      currGene1 <- unname(unlist(currGene[,sampleIndex1]))
      currGene2 <- unname(unlist(currGene[,sampleIndex2]))
      len <- length(currGene1)
      
      c1 <- c()
      c2 <- c()
      
      k=1
      for (i in 1:len){
        for (j in 1:len){
          c1[k] <- currGene1[i]
          c2[k] <- currGene2[j]
          k <- k+1
        }
      }
      c(c1, c2)
    })
    
    observe({
      session$sendCustomMessage(type = "points", datInput())
    })
    
    
    ggPS() %>% onRender("
      function(el, x, data) {
      
      noPoint = x.data.length;
      
      Shiny.addCustomMessageHandler('points',
      function(drawPoints) {
      
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
      size: 3
      },
      hoverinfo: 'none'
      };
      Traces.push(trace);
      Plotly.addTraces(el.id, Traces);
      });
      }" )
  })
  
  output$boxPlot <- renderPlotly({
    nVar = reactive(ncol(bindataSel()))
    colNms <- reactive(colnames(bindataSel()[, c(2:nVar())]))
    boxDat <- reactive(bindataSel()[, c(1:nVar())] %>% gather(key, val, -c(ID)))
    BP <- reactive(ggplot(boxDat(), aes(x = key, y = val)) + geom_boxplot())
    ggBP <- reactive(ggplotly(BP()))
    
    datInput2 <- eventReactive(input$goButton, {
      #print(input$goButton)
      # Chose particular gene
      g <- resSort[input$goButton]
      as.numeric(unname(unlist(bindataSel[which(bindataSel$ID==g),])))[-1]
    })
    
    output$test2 <- renderPrint({
      str(datInput2())
    })
    
    observe({
      session$sendCustomMessage(type = "lines", datInput2())
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