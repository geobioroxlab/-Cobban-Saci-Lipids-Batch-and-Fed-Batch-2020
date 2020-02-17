library(shiny)
library(vegan)
library(dplyr)
library(ggplot2)
library(plotly)
library(OneR)
library(ggfortify)
library(ape)
setwd(".")
#Setup NMDS based on dataframe
load("dataframefornmds.rda")
dataframeForRDAExperiments <- subset(dataframeforNMDS, select = c(Temp:dO))
groupNames <- c("Sampler" = "Sampler", "Shaking Speed" ="RPM", "Temperature" = "Temp", "pH" = "pH", "dO" = "dO", "Rates" = "RateBins", "RI" = "RIBins")


ui <- fluidPage(
  
  headerPanel("GDGT-Grouping Analysis"),
  sidebarPanel(
    conditionalPanel(condition = "input.plotType == 'NMDS' || input.plotType == 'PCA'",
                     selectInput('color', 'Color', choices = groupNames, selected = "Sampler")),  
    sliderInput("slider", label = h3("GDGT Comparison Range"), min = 0, max = 8, value = c(0, 8)),
    selectInput('plotType', 'Plot Type', choices = c("NMDS", "RDA", "PCA"), selected = "NMDS"),  
    conditionalPanel(
    condition = "input.plotType == 'RDA'",
    checkboxGroupInput('ordination', 'Ordination Type', choices = c("Shaking Speed" = "RPM", "Temperature" = "Temp", "pH" = "pH", "dO" = "dO"), selected = c("RPM", "Temp"))
    ),
    conditionalPanel(
    condition = "input.color == 'RateBins' || input.color == 'RIBins'",
    sliderInput("BinSlider", label = h3("Number of Bins"), min = 2, max = 20, value = 10))
    ),
  
  
  mainPanel(
   conditionalPanel(condition = "input.plotType == 'NMDS'",
    plotlyOutput('trendPlot', height = "600px")),
   conditionalPanel(condition = "input.plotType == 'RDA'",
         plotOutput("RDAPlot", height = "600px" )),
   conditionalPanel(condition ="input.plotType == 'PCA'",
          plotlyOutput('PCAPlot', height = "600px"))
   
   )
 )

server <- function(input, output) {

  output$trendPlot <- renderPlotly({
    GDGT1 <- paste("GDGT.", input$slider[1], sep = "")
    GDGT2 <- paste("GDGT.", input$slider[2], sep = "")
    NMDS_data <- reactive(dataframeforNMDS[which(colnames(dataframeforNMDS) == GDGT1):which(colnames(dataframeforNMDS)==GDGT2)])
    myData <- NMDS_data()
    dataframeForRDAExperiments$RateBins <- bin(dataframeForRDAExperiments$Rate, nbins = input$BinSlider, method = "length")
    dataframeForRDAExperiments$RIBins <- bin(dataframeForRDAExperiments$RI, nbins = input$BinSlider, method = "length")
    GDGTnmds<-metaMDS(myData, distance = "bray", k = 2)
    NMDS_xy <- data.frame(GDGTnmds$points)
    
    
    
    df <- dataframeForRDAExperiments
    selectedType = as.character(input$color)
    p <- ggplot(NMDS_xy, aes(x = MDS1, y = MDS2, color = as.factor(df[,selectedType]))) + geom_point(aes(text=sprintf("Sampler: %s<br>RPM: %s<br>Temperature: %s<br> pH: %s<br>dO: %s", df$Sampler, df$RPM, df$Temp, df$pH, df$dO))) + labs(color =selectedType)
    ggplotly(p, tooltip = "text")%>% 
      layout(height = input$plotHeight, autosize=TRUE)
  }
    )
  output$PCAPlot <- renderPlotly({
    GDGT1 <- paste("GDGT.", input$slider[1], sep = "")
    GDGT2 <- paste("GDGT.", input$slider[2], sep = "")
    NMDS_data <- reactive(dataframeforNMDS[which(colnames(dataframeforNMDS) == GDGT1):which(colnames(dataframeforNMDS)==GDGT2)])
    myData <- NMDS_data()
    dataframeForRDAExperiments$RateBins <- bin(dataframeForRDAExperiments$Rate, nbins = input$BinSlider, method = "length")
    dataframeForRDAExperiments$RIBins <- bin(dataframeForRDAExperiments$RI, nbins = input$BinSlider, method = "length")
    df <- dataframeForRDAExperiments
    selectedType = as.character(input$color)
    dataframeForRDAExperiments$group <- as.factor((dataframeForRDAExperiments[,selectedType]))
    PCAData <- prcomp(myData)
    pca_out <- as.data.frame(PCAData$x)
    pca_out$group <- dataframeForRDAExperiments$group
    p<-ggplot(pca_out,aes(x=PC1,y=PC2,color=group ))+geom_point(aes(text=sprintf("Sampler: %s<br>RPM: %s<br>Temperature: %s<br> pH: %s<br>dO: %s", df$Sampler, df$RPM, df$Temp, df$pH, df$dO))) + labs(colour =selectedType)
    ggplotly(p, tooltip = "text")
  })
  
  output$RDAPlot <- renderPlot({
    GDGT1 <- paste("GDGT.", input$slider[1], sep = "")
    GDGT2 <- paste("GDGT.", input$slider[2], sep = "")
    NMDS_data <- reactive(dataframeforNMDS[which(colnames(dataframeforNMDS) == GDGT1):which(colnames(dataframeforNMDS)==GDGT2)])
      inputString <- ""
      for (i in input$ordination){
      inputString <- paste(inputString, i,"+", sep ="")
   }
  inputString <- substring(inputString, 0, nchar(inputString)-1)
     doString <- sprintf("capscale(NMDS_data()~%s,dataframeForRDAExperiments, dist = 'bray', na.action = na.exclude)",inputString)
     print(doString)
     RDACap <-eval(parse(text = doString))
     plot(RDACap)
     })
  }


shinyApp(ui, server)
#SamplerPlot<- ggplot(NMDS_xy, aes(MDS1, MDS2, color=dataframeForRDAExperiments$Sampler))+labs(color=) + geom_point() + theme_bw()
