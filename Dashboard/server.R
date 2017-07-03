library(shinydashboard)

shinyServer(function(input, output, session){
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
  
  output$hexPlot <- renderPlotly({
    
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
    
    my_breaks <- c(2, 4, 6, 8, 20, 1000)    
    p <- reactive(ggplot(hexdf, aes(x=x, y=y, fill = counts, hexID=hexID)) + geom_hex(stat="identity") + geom_abline(intercept = 0, color = "red", size = 0.25) + labs(x = input$selPair[1], y = input$selPair[2]) + coord_fixed(xlim = c(-0.5, (maxRange[2]+buffer)), ylim = c(-0.5, (maxRange[2]+buffer))) + theme(aspect.ratio=1) + scale_fill_gradient(name = "count", trans = "log", breaks = my_breaks, labels = my_breaks, guide="legend"))
    
    plotlyHex <- reactive(ggplotly(p(), height = 400, width = 400))
    
    # Use onRender() function to draw x and y values of selected row as orange point
    plotlyHex() %>% onRender("
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
  
  
  output$scatterPlot <- renderPlotly({
    
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
    tempDF <- data.frame(x=x, y=y)
    
    #ggplot(mtcars, aes(x=wt, y=qsec)) + geom_point(alpha=0.5)
    
    p2 <- reactive(ggplot(tempDF, aes(x=x, y=y)) + geom_point(alpha = input$alpha) + geom_abline(intercept = 0, color = "red", size = 0.25) + labs(x = input$selPair[1], y = input$selPair[2]) +  coord_fixed(xlim = c(-0.5, (maxRange[2]+buffer)), ylim = c(-0.5, (maxRange[2]+buffer))) + theme(aspect.ratio=1))
    
    plotlyScatter <- reactive(ggplotly(p2(), height = 400, width = 400))
    
    # Use onRender() function to draw x and y values of selected row as orange point
    plotlyScatter() %>% onRender("
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
    
    boxDat <- eventReactive(datSel(), {
      boxDat <- datSel()[, c(1:nVar())] %>% gather(key, val, -c(ID))
      colnames(boxDat) <- c("ID", "Sample", "Count")
      boxDat
    })
    
    BP <- reactive(ggplot(boxDat(), aes(x = Sample, y = Count)) + geom_boxplot())
    ggBP <- reactive(ggplotly(BP(), width=600))
    
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