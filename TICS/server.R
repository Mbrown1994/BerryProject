# Packages used for this analysis
library(shinythemes)
library(tidyverse)
library(magrittr)
library(shiny)
library(caret)
library(DT)
library(dplyr)
library(tree)
library(ggplot2)
library(readr)

# Read in the Data
library(readr)
TICS <- read_csv("TICS.csv")
reason <- c(4, 3, 3, 4, 2, 2, 2, 1, 1, 1, 3, 1)
names(reason) <- c("Short Shots", "Light Color", "Mixed Pallet", "Contamination", "Mixed Product In a Box", "Voids", "Blurred Print", "Wrong Count", "Out of Round", "Incorrect quantity shipped", "Flash", "Fit")


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  # Create the table for the Data tab
  output$table <- renderDataTable({
    specificDepartment <- unlist(input$specificDepartment)
    specificComplaint <- unlist(input$specificComplaint)
    selectedCols <- unlist(input$selectedCols)
    
    # Filter
    TICS %>% filter(Department %in% specificDepartment,
                         Reason %in% specificComplaint) %>% select(selectedCols)

    })
  
  # Allow the user to download the data
  output$Download <- downloadHandler(
    filename = function (){
      paste("TICS.csv")
    },
    content = function(file){
      write_csv(
        TICS %>% 
          filter(Department %in% input$specificDepartment,
                 Reason %in% input$specificComplaint) %>%
          select(input$selectedCols),
        file)
    }
  )
  
  output$ParetoChart <- renderPlot({
    if (input$DataChoice == "TIC reoccurance"){
      pareto.chart(reason, xlab = "Defect", ylab = "Frequency", col=heat.colors(length(TICS)),
                   cumperc = seq(0,15, by=2),
                   ylab2 = "Cumulative Percentage",
                   main = "Complaints from Different Customers")}
    else if (input$DataChoice == "TIC Dollars"){
      p <- ggplot(TICS, aes(x = Reason, y = Credit)) + 
        geom_bar(stat = 'identity', fill = 'steel blue') + scale_x_discrete(guide = guide_axis(angle = 60)) +
        theme(axis.text.x=element_text(size=15))
      p}
    
    
  })
  
  output$MoldChart <- renderPlot({
    if (input$machinechoice == "Mold"){
     graph_data = filter(TICS, Department == "MLD")
     p <- ggplot(graph_data, aes(x = Mold, y = Credit)) +
       geom_bar(stat = 'identity', fill = 'steel blue') + scale_x_discrete(guide = guide_axis(angle = 60)) +
       theme(axis.text.x = element_text(size=15))
     p}
    else if (input$machinechoice == "Machine"){
      graph_data = filter(TICS, Department == "MLD")
      p <- ggplot(graph_data, aes(x = Line, y = Credit)) +
        geom_bar(stat = 'identity', fill = 'steel blue') + scale_x_discrete(guide = guide_axis(angle = 60)) +
        theme(axis.text.x = element_text(size=15))
      p}
     })
  
  output$PrintChart <- renderPlot({
    if (input$printchoice == "TIC Amount"){
    graph_data = filter(TICS, Department == "DEC")
    p <- ggplot(graph_data, aes(x = Line, y = Credit)) +
      geom_bar(stat = 'identity', fill = 'steel blue') + scale_x_discrete(guide = guide_axis(angle = 60)) +
      theme(axis.text.x = element_text(size=15))
    p}
    else if (input$printchoice == "TIC Count"){
      graph_data = filter(TICS, Department == "DEC")
      p <- ggplot(graph_data, aes(x = Line)) +
        geom_bar(fill = 'steel blue') + scale_x_discrete(guide = guide_axis(angle = 60)) +
        theme(axis.text.x = element_text(size=15))
      p}
    else if (input$printchoice == "TIC Reason"){
      graph_data = filter(TICS, Department == "DEC")
      p <- ggplot(graph_data, aes(x = Reason)) +
        geom_bar(fill = 'steel blue') + scale_x_discrete(guide = guide_axis(angle = 60)) +
        theme(axis.text.x = element_text(size=15))
      p}
 })
  
  
  
  
})


