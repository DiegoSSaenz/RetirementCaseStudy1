library(shiny)
library(dplyr)
library(ggplot2)

source('ex1.R')

## Net income plotting function

plotNet <- function(inDat, retireAge, ladder, tspRate, rothRate, taxAcctRate, ret) {
  
  plotDat <- ex1(inDat, retireAge, ladder, tspRate, rothRate, taxAcctRate, ret)  
  ggplot(plotDat %>% filter(variable == "net" | variable == "avail"), aes(age, value)) +
    geom_line() +
    facet_grid(variable~., scales="free") +
    labs(title=paste("Retire at", retireAge, "Case Study"))
  
}

# plotNet(retireAge=75, ladder=.04, tspRate=.04, rothRate=.12, taxAcctRate=0, ret=.05)

shinyServer(    
  function(input, output) {
    
    output$contents <- renderTable({
      
#       # input$file1 will be NULL initially. After the user selects
#       # and uploads a file, it will be a data frame with 'name',
#       # 'size', 'type', and 'datapath' columns. The 'datapath'
#       # column will contain the local filenames where the data can
#       # be found.
#       
      inFile <- input$file1
      
      if (is.null(inFile))
        return(NULL)
      
      read.csv(inFile$datapath, header=input$header, sep=input$sep, 
               quote=input$quote)
      
      inDat <- read.csv(inFile$datapath, header=input$header, sep=input$sep, 
                        quote=input$quote)
      
    })

    output$img1 <- renderPlot({
      
      inFile <- input$file1
      
      if (is.null(inFile))
        return(NULL)
      
      plotNet(inDat=read.csv(inFile$datapath, header=input$header, sep=input$sep, 
                             quote=input$quote),
              retireAge=input$retireAge,
              ladder=input$ladder/100,
              tspRate=input$tspRate/100,
              rothRate=input$rothRate/100,
              taxAcctRate=input$taxAcctRate/100,
              ret=input$ret/100)})

  }
)