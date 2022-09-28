library(shiny)
library(shinythemes)
library(zoo)
library(FEMSdevPkg)
library(xts)
library(ggplot2)

shinycssloaders::withSpinner(
  plotOutput("plot")
)


# Define UI for application that visualizes cashflows for a exmaple bond
ui <- fluidPage(theme = shinytheme("cerulean"),
                
                #top images 
                img(src = "actus-logo.png", height = 77, width = 220, style="float:right; padding-right:25px"),
                img(src="Logo_Weiss.png",height = 80, width = 100),

                # inputs for contract terms of Bond
                navbarPage("DaDFiR3 Demo Application", #navbar App title
                           tabPanel("RF Scenario Vis", #first Tab title
                                    sidebarLayout(
                                      #dataset choice input
                                      selectInput(inputId = "dataSetName",label = "Choose a Dataset", 
                                                  choices = c("Steady Rates", "Increasing Rates", "Decreasing Rates", "Recovering Rates")),
                                      #output plot of the respective rate scenario
                                      mainPanel(
                                        plotOutput("ratesPlot")
                                      )#main panel close
                                    )#sidebar close
                                  ),#tabpanel close
                           
                           #second tabpanel Cashflow Exmaple
                           tabPanel("Cashflow Example for a Bond",
                                    sidebarLayout(
                                      sidebarPanel(width = 3,
                                                   
                                                   #inputs for Bond Contract Terms
                                                    dateInput(inputId = "issueDate",label = "Choose Issuedate",value = "2013-12-31"),
                                                    numericInput(inputId = "nominal",label = "Choose the nominal",value = 10000,min = 0,max = 10000000,step = 1000),
                                                    numericInput(inputId = "coupon",label = "Choose the Couponrate",value = 0.02,min = 0,max = 0.15,step = 0.001),
                                                   selectInput(inputId = "rfScenarioBond",label = "Choose the Risk Factor Scenario",choices = c("increasing Rates","decreasing Rates","steady Rates","recovering Rates"))
                                                  ),#sidebar panel close
                                      
                                                  # Show a plot of the generated cashflows
                                                  mainPanel(width = 8,
                                                    shinycssloaders::withSpinner(
                                                    plotOutput("cashFlowPlot", width = "800px", height="500px")),#spinnerclose,
                                                    dataTableOutput("CFDF")
                                                  )#main panel close
                                                )#sidebar close
                                              ),#tabpanel close
                           #inputs for the toybank
                           tabPanel("Analysis of a ToyBank",
                                    sidebarLayout(
                                      sidebarPanel(width = 3,
                                        selectInput("ptfFile","Choose your Portfolio",choices = c("BondPortfolio","MixedPortfolio"),selected = "BondPortfolio"),
                                        selectInput(inputId = "analysisType",label = "Choose the analysis type",
                                                    choices =  c("monthly income","cumulative income","monthly liquidity change","accumulated liquidity")),
                                        selectInput(inputId = "rfScenario",label = "Choose the Risk Factor Scenario",choices = c("increasing Rates","decreasing Rates","steady Rates","recovering Rates"))
                                          ),#sidebarpanel Close
                                   
                                    mainPanel(
                                              shinycssloaders::withSpinner(
                                              plotOutput("CFPlot"))#spinner close
                                       )#main panel close
                                      )#sidebarLayout close
                                     )#tab panel close
                                    )#navbarPAGE close
                                   )#fluid Page close

# Define server logic required to create the cash flows of a simple bond
server <- function(input, output) {
  
  #reactive creation of the example bond
  pam1 <- reactive({bond(as.character(input$issueDate), maturity = "5 years", nominal = input$nominal,
               coupon = input$coupon, couponFreq = "1 years", role = "RPA")})
  
  #read the data files from inside of the package
  falling_fp <- system.file("extdata","UST5Y_fallingRates.csv", package = "FEMSdevPkg")
  rising_fp <-  system.file("extdata","UST5Y_risingRates.csv", package = "FEMSdevPkg")
  steady_fp <-  system.file("extdata","UST5Y_steadyRates.csv", package = "FEMSdevPkg")
  recovering_fp <-  system.file("extdata","UST5Y_recoveringRates.csv", package = "FEMSdevPkg")
  #create ACTUS ReferenceIndex
  rfx_falling <- sampleReferenceIndex(rxdfp = falling_fp,rfID = "UST5Y_fallingRates", 
                                      moc = "YC_EA_AAA",base = 100)
  rfx_rising <- sampleReferenceIndex(rising_fp,"UST5Y_risingRates",
                                     "YC_EA_AAA",100)
  rfx_steady <- sampleReferenceIndex(steady_fp,"UST5Y_steadyRates",
                                     "YC_EA_AAA",100)
  rfx_recovering <- sampleReferenceIndex(recovering_fp,"UST5Y_steadyRates",
                                         "YC_EA_AAA",100)
  
  #reactive creation of the events for the bond
  observe({
    if(input$rfScenarioBond == "increasing Rates"){
      evs1 <- reactive({generateEventSeries(contract = pam1(), list(rfx_rising),serverURL = "https://demo.actusfrf.org:8080/" )})
    }
    if(input$rfScenarioBond == "decreasing Rates"){
      evs1 <- reactive({generateEventSeries(contract = pam1(), list(rfx_falling),serverURL = "https://demo.actusfrf.org:8080/" )})
    }
    if(input$rfScenarioBond == "steady Rates"){
      evs1 <- reactive({generateEventSeries(contract = pam1(), list(rfx_steady),serverURL = "https://demo.actusfrf.org:8080/" )})
    }
    if(input$rfScenarioBond == "recovering Rates"){
      evs1 <- reactive({generateEventSeries(contract = pam1(), list(rfx_recovering),serverURL = "https://demo.actusfrf.org:8080/" )})
    }
  #creation of the desired plot
  output$cashFlowPlot <- renderPlot({
    cashflowPlot(evs1())
  })
  
  #optional data table of the event list
  output$CFDF <- renderDataTable({
    evs1()$events_df
    })
  
  })#observe close
  
  # reading of the different Rate scenario datafiles
  # local dm data file not available - commented out 
  # steadyRates <- as.data.frame(read.zoo(file = "data/5yTreasuryBill_steadyRates", index.column = "index"))[2:6001,]
  
  steadyRates <-      read.csv(system.file("extdata","UST5Y_steadyRates.csv", package = "FEMSdevPkg"))
  increasingRates <-  read.csv(system.file("extdata","UST5Y_risingRates.csv", package = "FEMSdevPkg"))
  decreasingRates <-  read.csv(system.file("extdata","UST5Y_fallingRates.csv", package = "FEMSdevPkg"))
  recoveringRates <-  read.csv(system.file("extdata","UST5Y_recoveringRates.csv", package = "FEMSdevPkg"))
  
  # local dm data file not available - commented out 
  # recoveringRates <- as.data.frame(read.zoo(file = "data/5yTreasuryBill_recoveringRates", index.column = "index"))[2:6001,]
  
  #desired plot outbut based on selection of input with label: "dataSetName"
  output$ratesPlot <- renderPlot({
    
    if(input$dataSetName == "Steady Rates"){
      data1 <- xts(x = as.numeric(steadyRates[,2]),order.by = as.Date(steadyRates[,1]))
      plot.zoo(data1,main = "5 year Treasury Bill", xlab = "Year" , ylab = "Interest Rate in %")
    }
    
    if(input$dataSetName == "Increasing Rates"){
      data2 <- xts(x = as.numeric(increasingRates[,2]),order.by = as.Date(increasingRates[,1]))
      plot.zoo(data2, main = "5 year Treasury Bill", xlab = "Year" , ylab = "Interest Rate in %")
    }
    
    if(input$dataSetName == "Decreasing Rates"){
      data3 <- xts(x = as.numeric(decreasingRates[,2]),order.by = as.Date(decreasingRates[,1]))
      plot.zoo(data3,  main = "5 year Treasury Bill", xlab = "Year" , ylab = "Interest Rate in %")
    }
    
    if(input$dataSetName == "Recovering Rates"){
      data4 <- xts(x = as.numeric(recoveringRates[,2]),order.by = as.Date(recoveringRates[,1]))
      plot.zoo(data4,  main = "5 year Treasury Bill", xlab = "Year" , ylab = "Interest Rate in %")
    }
    
  })#renderplot close 
  
  #read the data files from inside of the package
  falling_fp <- system.file("extdata","UST5Y_fallingRates.csv", package = "FEMSdevPkg")
  rising_fp <-  system.file("extdata","UST5Y_risingRates.csv", package = "FEMSdevPkg")
  steady_fp <-  system.file("extdata","UST5Y_steadyRates.csv", package = "FEMSdevPkg")
  recovering_fp <-  system.file("extdata","UST5Y_recoveringRates.csv", package = "FEMSdevPkg")
  #create ACTUS ReferenceIndex
  rfx_falling <- sampleReferenceIndex(rxdfp = falling_fp,rfID = "UST5Y_fallingRates", 
                                      moc = "YC_EA_AAA",base = 100)
  rfx_rising <- sampleReferenceIndex(rising_fp,"UST5Y_risingRates",
                                     "YC_EA_AAA",100)
  rfx_steady <- sampleReferenceIndex(steady_fp,"UST5Y_steadyRates",
                                     "YC_EA_AAA",100)
  rfx_recovering <- sampleReferenceIndex(recovering_fp,"UST5Y_steadyRates",
                                     "YC_EA_AAA",100)
  observe({
    if(input$ptfFile == "BondPortfolio"){
      cdfn <- system.file("extdata","BondPortfolio.csv",package = "FEMSdevPkg")
    }
    if(input$ptfFile == "MixedPortfolio"){
      cdfn <- system.file("extdata","BondPortfolio.csv",package = "FEMSdevPkg")
    }
    rfdfn <- system.file("extdata","RiskFactors.csv",package = "FEMSdevPkg")
    
    #create the portfolio with the respective files
    ptf   <-  samplePortfolio(cdfn,rfdfn)
    

  #create eventSeries for the selected contract
  if(input$rfScenario == "decreasing Rates"){
    plotlist <- reactive(simulatePortfolio(ptf, "https://demo.actusfrf.org:8080/", list(rfx_falling),
                                           rfx_falling$riskFactorID))
  }
  
  if(input$rfScenario == "increasing Rates"){
    plotlist <- reactive(simulatePortfolio(ptf, "https://demo.actusfrf.org:8080/", list(rfx_rising),
                                           rfx_rising$riskFactorID))
  }
  if(input$rfScenario == "steady Rates"){
    plotlist <- reactive(simulatePortfolio(ptf, "https://demo.actusfrf.org:8080/", list(rfx_steady),
                                             rfx_rising$riskFactorID))
  }
  if(input$rfScenario == "recovering Rates"){
    plotlist <- reactive(simulatePortfolio(ptf, "https://demo.actusfrf.org:8080/", list(rfx_recovering),
                                             rfx_rising$riskFactorID))
  }
  
  output$CFPlot <- renderPlot({
    plotlist()[[input$analysisType]]
    })
  
  })#observe close
  
  
}#server close



# Run the application 
shinyApp(ui = ui, server = server)
