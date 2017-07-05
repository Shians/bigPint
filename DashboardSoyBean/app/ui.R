library(shinydashboard)
library(shiny)
library(plotly)
library(hexbin)
library(htmlwidgets)
library(dplyr)
library(tidyr)

load("/Users/lindz/RNASeqVisualization/data/bindataL120.Rda")
dat <- bindata
rm(bindata)
datCol <- colnames(dat)[-which(colnames(dat) %in% "ID")]
myPairs <- unique(sapply(datCol, function(x) unlist(strsplit(x,"[.]"))[1]))
load("/Users/lindz/RNASeqVisualization/data/metrics.Rda")
metrics[[1]] <- metrics[[1]][which(metrics[[1]]$PValue<0.000000001),]
metrics[[1]] <- metrics[[1]][which(metrics[[1]]$ID %in% dat$ID),]
dat = dat[which(dat$ID %in% metrics[[1]]$ID),]
myMetrics <- colnames(metrics[[1]])[-which(colnames(metrics[[1]]) %in% "ID")]
values <- reactiveValues(x=0, selPair=NULL, selMetric=NULL, selOrder=NULL)

sidebar <- dashboardSidebar(
  hr(),
  sidebarMenu(id="tabs",
    menuItem("Binned scatterplot", tabName="hexPlot", selected=TRUE), # icon=icon("line-chart"),
    menuItem("Scatterplot", tabName = "scatterPlot"), # icon=icon("table")
    menuItem("Parallel coordinates", tabName = "boxPlot") # icon = icon("file-text-o")
  )#,
  #hr(),
  # fluidRow(
  #   column(1),
  #   column(10,
      # selectizeInput("selPair", "Pairs:", choices = myPairs, multiple = TRUE, options = list(maxItems = 2)),
      # selectInput("selMetric", "Metric:", choices = myMetrics),
      # selectInput("selOrder", "Order:", choices = c("Increasing", "Decreasing")),
      # actionButton("goButton", "Plot case!")
  #   )
  # )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "hexPlot",
      fluidRow(
        column(width = 4, 
         box(width = NULL, status = "primary", title = "Plot metrics", solidHeader = TRUE,
              selectizeInput("selPair1", "Pairs:", choices = myPairs, multiple = TRUE, options = list(maxItems = 2)),
              selectInput("selMetric1", "Metric:", choices = myMetrics),
              selectInput("selOrder1", "Order:", choices = c("Increasing", "Decreasing")),
              numericInput("binSize", "Hexagon size:", value = 10),
              actionButton("goButton1", "Plot case!"))),
        column(width = 8,
          box(width = NULL, plotlyOutput("hexPlot"), collapsible = FALSE, background = "black", title = "Binned scatterplot", status = "primary", solidHeader = TRUE))),
      fluidRow(
        column(width = 8, offset = 4,
          box(width = NULL, verbatimTextOutput("info1"), collapsible = TRUE, title = "Observation metrics", status = "primary", solidHeader = TRUE)))),

    tabItem(tabName = "scatterPlot",
      fluidRow(
        column(width = 4, 
           box(width = NULL, status = "primary", title = "Plot metrics", solidHeader = TRUE,
             selectizeInput("selPair2", "Pairs:", choices = myPairs, multiple = TRUE, options = list(maxItems = 2)),
             selectInput("selMetric2", "Metric:", choices = myMetrics),
             selectInput("selOrder2", "Order:", choices = c("Increasing", "Decreasing")),
             sliderInput("alpha", "Alpha level:", min=0, max=1, value=1, step=0.01),
             actionButton("goButton2", "Plot case!"))),
        column(width = 8,
           box(width = NULL, plotlyOutput("scatterPlot"), collapsible = FALSE, background = "black", title = "Scatterplot", status = "primary", solidHeader = TRUE))),
      fluidRow(
        column(width = 8, offset = 4,
          box(width = NULL, verbatimTextOutput("info2"), collapsible = TRUE, title = "Observation metrics", status = "primary", solidHeader = TRUE)))),    
    
    tabItem(tabName = "boxPlot",
      fluidRow(
        column(width = 4, 
           box(width = NULL, status = "primary", title = "Plot metrics", solidHeader = TRUE,
             selectizeInput("selPair3", "Pairs:", choices = myPairs, multiple = TRUE, options = list(maxItems = 2)),
             selectInput("selMetric3", "Metric:", choices = myMetrics),
             selectInput("selOrder3", "Order:", choices = c("Increasing", "Decreasing")),
             actionButton("goButton3", "Plot case!"))),
        column(width = 8,
           box(width = NULL, plotlyOutput("boxPlot"), collapsible = FALSE, background = "black", title = "Parallel coordinate plot", status = "primary", solidHeader = TRUE))),
      fluidRow(
        column(width = 8, offset = 4,
          box(width = NULL, verbatimTextOutput("info3"), collapsible = TRUE, title = "Observation metrics", status = "primary", solidHeader = TRUE))))
  )
)

dashboardPage(
  dashboardHeader(title = "Overlaying cases"),
  sidebar,
  body
)