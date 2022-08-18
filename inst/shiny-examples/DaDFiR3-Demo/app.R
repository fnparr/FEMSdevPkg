library(shiny)
library(shinythemes)
library(zoo)
# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("cerulean"),
                img(src = "actus-logo.png", height = 77, width = 220, 
                    style="float:right; padding-right:25px"),
                img(src="Logo_Weiss.png",height = 80, width = 100),
                        
    # Application title
    titlePanel("DaDFiR3 Demo Application"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        selectInput(inputId = "dataSetName",label = "Choose a Dataset",
                    choices = c("Steady Rates", "Increasing Rates", 
                                "Decreasing Rates", "Recovering Rates")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("ratesPlot")
        )
))


# Define server logic required to draw a histogram
server <- function(input, output) {
  
    output$ratesPlot <- renderPlot({
      
      if(input$dataSetName == "Steady Rates"){
         plotDataPath <- "../../extdata/UST5Y_steadyRates.csv"
      }
      else if (input$dataSetName == "Increasing Rates"){
         plotDataPath <- "../../extdata/UST5Y_risingRates.csv"
      }
      else if (input$dataSetName == "Decreasing Rates"){
         plotDataPath <- "../../extdata/UST5Y_fallingRates.csv"
      }
      else if (input$dataSetName == "Recovering Rates"){
         plotDataPath <- "../../extdata/UST5Y_recoveringRates.csv"
      }
      
      plot.zoo(read.csv.zoo(
        file = plotDataPath, header = TRUE),
        main="5 year US Treasury Bill Interest Rate", xlab = "Year",
        ylab = "Annual %ge interest")
      

    })
}

# Run the application 
shinyApp(ui = ui, server = server)
