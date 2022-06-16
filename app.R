#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
library(plot3D)

# read csv it as a matrix
df <- read.csv('matrix.csv', header = F)#[,-1]

# create a list of elements to use it as column and row names in the matrix
# Var$ can be typed with the name of any variable, e.g., "temperature" & 
# "sun_light" as long it is the same in the csv.
Var1 <- as.list(df[1,2:ncol(df)]) # a list that excludes the first column
Var2 <- as.list(df[2:nrow(df),1]) # a list that excludes the first row
m <- as.matrix(df[2:nrow(df),2:ncol(df)]) # save an object as a matrix

# Change the matrix column & row names using the variable's values  
colnames(m) <- Var1
row.names(m) <- Var2

# read the matrix as a dataframe
mtable <- as.data.frame(as.table(m)) # change format from matrix to df
colnames(mtable)[3] <- "SC" # rename column 3; "SC" stands for system capacity

# function to extract the system capacity from a matrix/table
getval <- function(c1, c2, data=mtable){
  syscap <- data$SC[data$Var1 == c1 & data$Var2 == c2]
  return(syscap)
}

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Getting the system capacity from interactions"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("X",
                      "Value of X:",
                      min = 0.1,
                      max = 1,
                      value = 0.5,
                      step = 0.1),
            sliderInput("Y",
                        "Value of Y:",
                        min = 0,
                        max = 27,
                        value = 15,
                        step = 3)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           #plotOutput("matrixPlot"),
           plotlyOutput("matrixPlot"),
           #textOutput("Sys"),
           verbatimTextOutput("Sys")
         )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    # output$matrixPlot <- renderPlot({
    #     # plot matrix from data of excel/csv file from ui.R
    #     persp3D(z=m,theta=45, phi=45, axes=TRUE)
    
    output$matrixPlot <- renderPlotly({

        #plot_ly(z = m,type = "heatmap") # can be a contour
        plot_ly() %>% 
          add_trace(data = mtable,  
                    x=mtable$Var1, 
                    y=mtable$Var2, 
                    z=mtable$SC, 
                    intensity=mtable$SC,
                    colorscale = 'Viridis',
                    type="mesh3d")
      
        })
    
    S = reactive({input$X})
    C = reactive({input$Y})
    # output$Sys <- renderText({
    #    paste0('If the value of X is ', S(),
    #           ' & the value of Y is ', C(),
    #           ' , then the system capaciy is: ', print(getval(S(),C())))
      output$Sys <- renderText({
        paste0('If the value of X is ', S(),
               ' & the value of Y is ', C(),
               ', then the system capaciy is: ', print(getval(S(),C())))    
        })
}

# Run the application 
shinyApp(ui = ui, server = server)
