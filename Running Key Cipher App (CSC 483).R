#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#library(readr)
#Stream1Data <- read_csv("College NKU d Senior Year/Fall Semester/CSC 483 (Cryptology)/Final Project/Stream 1 Dictionary (CSC 483).csv")
#Stream2Data <- read_csv("College NKU d Senior Year/Fall Semester/CSC 483 (Cryptology)/Final Project/Stream 2 Dictionary (CSC 483).csv")

library(shiny)
library(dplyr)
library(stringr)
library(ggplot2)
library(DT)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Cryptology Project"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      textAreaInput(inputId = "cipherText",
                    label = "Cipher Text",
                    width = '200px',
                    height = '200px',
                    placeholder = "Paste Cipher Text")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      verbatimTextOutput("stream1Text"),
      verbatimTextOutput("stream2Text"),
      verbatimTextOutput("selected")
    )
  ), 

  DT::dataTableOutput("stream1"), 
  DT::dataTableOutput("stream2")
)


#######################################################################################################################


server <- function(input, output) {
    

#######################################################################################################################    
  
  stream1 <- data.frame(row.names = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26))
  
  output$stream1 <- DT::renderDataTable({
   
      cipherString = str_remove_all(input$cipherText, "[[:space:][:punct:][:digit:]]") %>% str_to_upper()
      dictionary <- data.frame(Stream1Data)
      cipher <- strsplit(cipherString, "")
      
      for (item in cipher) {
        column <- data.frame(dictionary[item])
        stream1 <- merge(stream1, column, by = "row.names", sort = FALSE)
      }
      stream1[-1]
      
    }, server = FALSE, selection= list(target = 'cell'), rownames = TRUE)
####################################################################################################################### 
  output$stream1Text <- renderPrint({
      selected = input$stream1_cells_selected  
     
      cipherString = str_remove_all(input$cipherText, "[[:space:][:punct:][:digit:]]") %>% str_to_upper()
      dictionary <- data.frame(Stream1Data)
      cipher <- strsplit(cipherString, "")
      
      for (item in cipher) {
        column <- data.frame(dictionary[item])
        stream1 <- merge(stream1, column, by = "row.names", sort = FALSE)
      }
      stream1=select(stream1, c(-1))
      
      
      if (length(selected)) {
        cat('stream1:\n\n')
        cat(stream1[selected], sep = '')
        }
      
    })
####################################################################################################################### 
    
  stream2 <- data.frame(row.names = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26))
    
    output$stream2 <- DT::renderDataTable({
      
     
      
      cipherString = str_remove_all(input$cipherText, "[[:space:][:punct:][:digit:]]") %>% str_to_upper()
      dictionary <- data.frame(Stream2Data)
      cipher <- strsplit(cipherString, "")
      
      for (item in cipher) {
        column <- data.frame(dictionary[item])
        stream2 <- merge(stream2, column, by = "row.names", sort = FALSE)
      }
      stream2=select(stream2, c(-1))
      
  }, server = FALSE, selection = list(target = 'cell'), rownames = TRUE)
    
  
####################################################################################################################### 
output$stream2Text <- renderPrint({
  selected = input$stream2_cells_selected  
  
  cipherString = str_remove_all(input$cipherText, "[[:space:][:punct:][:digit:]]") %>% str_to_upper()
  dictionary <- data.frame(Stream2Data)
  cipher <- strsplit(cipherString, "")
  
  for (item in cipher) {
    column <- data.frame(dictionary[item])
    stream2 <- merge(stream2, column, by = "row.names", sort = FALSE)
  }
  stream2=select(stream2, c(-1))

  if (length(selected)) {
    cat('stream2:\n\n')
    cat(stream2[selected], sep = '')
  }
  
})
###################################################################################################
}


# Run the application 
shinyApp(ui = ui, server = server)
