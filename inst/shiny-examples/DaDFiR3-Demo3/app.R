library(shiny)
library(shinythemes)
library(FEMSdevBase)
library(ggplot2)

shinycssloaders::withSpinner(
  plotOutput("plot")
)

# 
# Define UI for application that visualizes cashflows for a example bond
ui <- fluidPage(
  theme = shinytheme("cerulean"),
  
  #top images 
  img(src = "actus-logo.png", height = 77, width = 220,
      style="float:right; padding-right:25px"),
  img(src="Logo_Weiss.png",height = 80, width = 100),
  
  # Title and bar with tabs
  navbarPage("
             DaDFiR3 Demo",   #navbar App title
             tabPanel("Interest Rate Scenarios", #first Tab title
                      sidebarLayout(
                        #dataset choice input
                        selectInput(inputId = "dataSetName",
                                    label = "Choose a Dataset", 
                                    choices = c("Steady Rates", 
                                                "Increasing Rates",
                                                "Decreasing Rates", 
                                                "Recovering Rates")),
                        #output plot of the respective rate scenario
                        mainPanel( plotOutput("ratesPlot")
                        )  #main panel close
                      )  #sidebar close
             ),  #tabpanel close
             #second tabpanel Loan Contract Cashflow
             tabPanel("Loan Contract Cashflow",
                      sidebarLayout(
                        sidebarPanel(width = 3,
                                     #inputs for Loan Contract Terms
                                     selectInput(inputId = "contractType",
                                                 label = "Loan contract type",
                                                 choices = c("PAM","ANN"),
                                                 selected = "PAM"
                                     ),
                                     dateInput(inputId = "issueDate", 
                                               label = "Choose Issuedate",
                                               value = "2020-12-31"
                                     ),
                                     sliderInput(inputId = "maturity",
                                                 label = "Choose maturity",
                                                 min = 1,max = 10,
                                                 value = 5,
                                                 step = 1
                                     ),
                                     numericInput(inputId = "nominal",
                                                  label = "Set loan amount",
                                                  value = 10000,min = 0, 
                                                  max = 10000000, step = 1000
                                     ),
                                     numericInput(inputId = "coupon",
                                                  label = "Interest rate",
                                                  value = 0.02,min = 0,max = 0.05,
                                                  step = 0.005
                                     ),
                                     selectInput(inputId = "paymentFreq",
                                                 label = "Payment frequency",
                                                 choices = c("3 months", "6 months", "1 year")
                                     ),
                                     selectInput(inputId = "rateResetFreq",
                                                 label = "Rate reset frequency",
                                                 choices = c("Fixed rate","1 month", 
                                                             "3 months", "6 months",
                                                             "1 year" ) 
                                     ),
                                     numericInput(inputId = "rateResetSpread",
                                                  label = "Rate reset spread",
                                                  value = 0.05, min = 0, max = 0.05,
                                                  step = 0.005
                                     ),
                                     selectInput(inputId = "rfScenarioBond", 
                                                 label = "Interest rate scenario",
                                                 choices = c("increasing Rates",
                                                             "decreasing Rates",
                                                             "steady Rates",
                                                             "recovering Rates")
                                     )
                        ),   #sidebar panel close
                        
                        # Show a plot of the generated cashflows
                        mainPanel(width = 8,
                                  shinycssloaders::withSpinner(
                                    plotOutput("cashFlowPlot", 
                                               width = "800px",
                                               height="500px")
                                  ),     #spinnerclose,
                                  dataTableOutput("CFDF")
                        )    #main panel close
                      ) #sidebar close
             ),   #tabpanel close
             #inputs for the modelbank
             tabPanel("Portfolio Analysis",
                      sidebarLayout(
                        sidebarPanel(width = 3,
                                     selectInput("ptfFile","Choose your Portfolio",
                                                 choices = c("BondPortfolio",
                                                             "MortgagePortfolio"),
                                                 selected = "BondPortfolio"
                                     ),
                                     selectInput(
                                       inputId = "analysisType",
                                       label = "Choose the analysis type",
                                       choices =  c("monthly income",
                                                    "cumulative income",
                                                    "monthly liquidity change",
                                                    "accumulated liquidity")
                                     ),
                                     selectInput(
                                       inputId = "rfScenario",
                                       label = "Choose the Risk Factor Scenario",
                                       choices = c("increasing Rates",
                                                   "decreasing Rates",
                                                   "steady Rates",
                                                   "recovering Rates")
                                     )
                        ),   #sidebarpanel Close
                        
                        mainPanel(width = 8 ,shinycssloaders::withSpinner(
                          plotOutput("CFPlot")
                        ),   #spinner close
                        column(
                          dataTableOutput("portfolioDF"),
                          width = 12) #column close
                        )   #main panel close
                      )   #sidebarLayout close
                      
             ),   #tab panel close
             tabPanel("Uploaded Portfolio Analysis",
                      sidebarLayout(
                        sidebarPanel(width = 3,
                                     fileInput(inputId = "uploadedFile",
                                               label = "Your csv file of ACTUS PAM Contracts",
                                               accept = ".csv",
                                               buttonLabel = "Browse...",
                                               placeholder = "No file selected"
                                     ),
                                     selectInput(
                                       inputId = "analysisTypeCus",
                                       label = "Choose the analysis type",
                                       choices =  c("monthly income",
                                                    "cumulative income",
                                                    "monthly liquidity change",
                                                    "accumulated liquidity")
                                     ),
                                     selectInput(
                                       inputId = "rfScenarioCus",
                                       label = "Choose the Risk Factor Scenario",
                                       choices = c("increasing Rates",
                                                   "decreasing Rates",
                                                   "steady Rates",
                                                   "recovering Rates")
                                     )
                        ),#sidebarpanel close
                        mainPanel(
                          textOutput("warning"),
                          tags$head(tags$style("#warning{color: red;
                                 font-size: 15px;
                                 font-style: bold;
                                 }"
                          )),
                          plotOutput("CFPlotCus"),
                          column(
                            dataTableOutput("portfolioDF_u"),
                            width = 12)#column close
                        )  #mainPanel close
                      )  #sidebarlayout close
             ),  #tab panel close
             tabPanel("Help",
                      sidebarLayout(
                        sidebarPanel(
                          textInput("serverUrl","Specify your server URL",value = "https://demo.actusfrf.org:8080/")
                        ),
                        mainPanel(
                          h2("Interest Rate Scenarios"),
                          h4("In this tab you can view the four predefined interest rate scenarios. 
                             Please note that these scenarios are simple linear projections. 
                             We aim to include the posibility to upload your own risk factor scenarios.", style ="color:black"),
                          h2("Loan Contract Cashflow"),
                          h4("In this tab one can specify the terms for a single ACTUS loan contract. 
                             A cashflow plot and an event list for the specific contract is then generated.", style ="color:black"),
                          h4("ANN = Mortgage / PAM = Bond", style = "color:black"),
                          h2("Portfolio Analysis"),
                          h4("In this tab one can carry out analytics on predefined Portfolios. Income and liquidity metrics are 
                          shown in the plot and the respective contracts of the portfolio are displayed in the DataTable", style ="color:black"),
                          h2("Uploaded Portfolio Analysis"),
                          h4("You can upload a data file from your workstation specifying a portfolio 
                                        of loan contracts and request ACTUS contract simulation and analysis.",
                             "The uploaded file must be .csv format and patterned on files:",
                             tags$a("BondPortfolio.csv", 
                                    href="https://github.com/fnparr/FEMSdevBase/tree/main/inst/extdata/BondPortfolio.csv", target = "_blank"),
                             ",  and ",
                             tags$a("AnnuityPortfolio.csv", 
                                    href="https://github.com/fnparr/FEMSdevBase/tree/main/inst/extdata/AnnuityPortfolio.csv", target = "_blank"),
                             " - with any variable rate setting based on Market Object Code YC_EA_AAA.",
                             "For a more detailed explanation of each contract term, consult the ",
                             tags$a("ACTUS Data Dictionary",     href="https://www.actusfrf.org/dictionary", target = "_blank"), style ="color:black"),
                          h2("Specification of ServerURL"),
                          h4("In case you want to use a local installation of an ACTUS server, you can specify the serverURL in the sidebar of the Help Tab. By default, the ServerURL is set to:", 
                             tags$a("https://demo.actusfrf.org:8080/",href = "https://demo.actusfrf.org:8080/", target = "_blank") ,"(public actus server)", style = "color:black"),
                          h4(tags$b("IMPORTANT:"),"If you use a docker version of actus-server your serverURL must be: host.docker.internal:PORT/)", style = "color:black"), 
                          h4("(Use host.docker.internal:8083/ as default)", style = "color:black"),
                          h2("Contact"),
                          h4("Please note that this App is a Work in Progress. 
                             If you encounter any error messages or other malfunctions, 
                             please contact the developers via:", style ="color:black"),h4("info@dadfir3.ch",  style ="color:blue"),
                          h4("If you have any suggestions for new features or improvments
                             of the app, we appreciate your inputs.",style ="color:black")
                        ) #main panel close
                      )#sidebarLayout close
             )  #tab panel close
  )   #navbarPAGE closet
)   #fluid Page close


# Define server logic required to create the cash flows of a simple bond
server <- function(input, output) {
  
  #reactive creation of the example bond
  # set the ACTUS serverURL
  serverURL <- reactive(input$serverUrl)
  #   serverURL = "https://demo.actusfrf.org:8080/"
  #   serverURL <- "http://ractus.ch:8080/"
  
  #read the data files from inside of the package
  falling_fp <- system.file("extdata","UST5Y_fallingRates.csv", 
                            package = "FEMSdevBase")
  rising_fp <-  system.file("extdata","UST5Y_risingRates.csv", 
                            package = "FEMSdevBase")
  steady_fp <-  system.file("extdata","UST5Y_steadyRates.csv", 
                            package = "FEMSdevBase")
  recovering_fp <-  system.file("extdata","UST5Y_recoveringRates.csv", 
                                package = "FEMSdevBase")
  #create ACTUS ReferenceIndex
  rfx_falling <- sampleReferenceIndex(rxdfp = falling_fp,
                                      rfID = "UST5Y_fallingRates", 
                                      moc = "YC_EA_AAA",base = 100 )
  rfx_rising <- sampleReferenceIndex(rising_fp,"UST5Y_risingRates",
                                     "YC_EA_AAA",100 )
  rfx_steady <- sampleReferenceIndex(steady_fp,"UST5Y_steadyRates",
                                     "YC_EA_AAA",100 )
  rfx_recovering <- sampleReferenceIndex(recovering_fp,"UST5Y_steadyRates",
                                         "YC_EA_AAA",100 )
  #reactive creation of the example bond
  observe({
    cnt1 <- reactive({loan(input$contractType, as.character(input$issueDate),
                           maturity = paste(input$maturity, "years"), 
                           nominal = input$nominal, coupon = input$coupon,
                           paymentFreq = input$paymentFreq, role = "RPA",
                           rateResetFreq = input$rateResetFreq,
                           rateResetSpread = input$rateResetSpread )
    })
    
    #reactive creation of the events for the bond
    if(input$rfScenarioBond == "increasing Rates"){
      evs1 <- reactive({generateEventSeries(contract = cnt1(), 
                                            list(rfx_rising),
                                            serverURL()  
      )
      })
    }
    if(input$rfScenarioBond == "decreasing Rates"){
      evs1 <- reactive({generateEventSeries(contract = cnt1(), 
                                            list(rfx_falling),
                                            serverURL()  
      )
      })
    }
    if(input$rfScenarioBond == "steady Rates"){
      evs1 <- reactive({generateEventSeries(contract = cnt1(), 
                                            list(rfx_steady),
                                            serverURL()  
      )
      })
    }
    if(input$rfScenarioBond == "recovering Rates"){
      evs1 <- reactive({generateEventSeries(contract = cnt1(), 
                                            list(rfx_recovering),
                                            serverURL()  
      )
      })
    }
    #creation of the desired plot
    output$cashFlowPlot <- renderPlot({
      cashflowPlot(evs1())
    })
    
    #optional data table of the event list
    output$CFDF <- renderDataTable({
      evs1()$events_df
    })
  })   #observe close
  
  # reading of the different Rate scenario datafiles
  steadyRates <-      read.csv(system.file("extdata","UST5Y_steadyRates.csv", 
                                           package = "FEMSdevBase"))
  increasingRates <-  read.csv(system.file("extdata","UST5Y_risingRates.csv", 
                                           package = "FEMSdevBase"))
  decreasingRates <-  read.csv(system.file("extdata","UST5Y_fallingRates.csv", 
                                           package = "FEMSdevBase"))
  recoveringRates <-  read.csv(system.file("extdata","UST5Y_recoveringRates.csv",
                                           package = "FEMSdevBase"))
  
  # aggregate to monthly for quick nice looking plots 
  monthlySteadyRates     <- monthlyAverageRate(steadyRates)
  monthlyIncreasingRates <- monthlyAverageRate(increasingRates)
  monthlyDecreasingRates <- monthlyAverageRate(decreasingRates)
  monthlyRecoveringRates <- monthlyAverageRate(recoveringRates)
  
  #desired plot outbut based on selection of input with label: "dataSetName"
  observe({ 
    
    if(input$dataSetName == "Steady Rates"){
      plot <-  ggplot(monthlySteadyRates, aes(x=Date,y=Rate)) +
        geom_line(colour = "black") +
        labs(title = "Steady rates scenario - US Treasury 5 Year Rates")
    }
    
    if(input$dataSetName == "Increasing Rates"){
      plot<- ggplot(monthlyIncreasingRates, aes(x=Date,y=Rate)) +
        geom_line(colour = "black") +
        labs(title = "Increasing rates scenario - US Treasury 5 Year Rates")
    }
    
    if(input$dataSetName == "Decreasing Rates"){
      plot <- ggplot(monthlyDecreasingRates, aes(x=Date,y=Rate)) +
        geom_line(colour = "black") +
        labs(title = "Decreasing rates scenario - US Treasury 5 Year Rates")
    }
    
    if(input$dataSetName == "Recovering Rates"){
      plot <- ggplot(monthlyRecoveringRates, aes(x=Date,y=Rate)) +
        geom_line(colour = "black") +
        labs(title = "Recovering rates scenario - US Treasury 5 Year Rates")
    }
    
    output$ratesPlot <- renderPlot({ plot })
    
  })      # observe close
  
  observe({
    if(input$ptfFile == "BondPortfolio"){
      cdfn <- system.file("extdata","BondPortfolio.csv",package = "FEMSdevBase")
    }
    if(input$ptfFile == "MortgagePortfolio"){
      cdfn <- system.file("extdata","AnnuityPortfolio.csv",package = "FEMSdevBase")
    }
    rfdfn <- system.file("extdata","RiskFactors.csv",package = "FEMSdevBase")
    
    #create the portfolio with the respective files
    ptf   <-  samplePortfolio(cdfn,rfdfn)
    
    portfolioDF <- read.csv(cdfn)
    #portfolioDF <- portfolioDF[ , colSums(portfolioDF == "NULL") < nrow(portfolioDF)] 
    #still too many columns therefore, manual selection of most important columns
    portfolioDF <- portfolioDF[,c("contractType","statusDate","contractRole","contractID",
                                  "nominalInterestRate","currency","initialExchangeDate",
                                  "premiumDiscountAtIED","maturityDate","notionalPrincipal",
                                  "rateSpread","description")]
    
    output$portfolioDF <- renderDataTable(portfolioDF,
                                          options = list(autoWidth = TRUE, scrollX = TRUE))
    
    #create eventSeries for the selected contract
    if(input$rfScenario == "decreasing Rates"){
      plotlist <- reactive(simulatePortfolio(ptf, serverURL(), list(rfx_falling),
                                             rfx_falling$riskFactorID))
    }
    
    if(input$rfScenario == "increasing Rates"){
      plotlist <- reactive(simulatePortfolio(ptf, serverURL(), list(rfx_rising),
                                             rfx_rising$riskFactorID))
    }
    if(input$rfScenario == "steady Rates"){
      plotlist <- reactive(simulatePortfolio(ptf, serverURL(), list(rfx_steady),
                                             rfx_rising$riskFactorID))
    }
    if(input$rfScenario == "recovering Rates"){
      plotlist <- reactive(simulatePortfolio(ptf, serverURL(), 
                                             list(rfx_recovering),
                                             rfx_rising$riskFactorID))
    }
    
    output$CFPlot <- renderPlot({
      plotlist()[[input$analysisType]]
    })
    
  })   #observe close
  
  observe({
    if(is.null(input$uploadedFile) != TRUE){
      file <- reactive(input$uploadedFile)
      cdfn_u <- file()$datapath
      rfdfn <- system.file("extdata","RiskFactors.csv",package = "FEMSdevBase")
      
      portfolioDF_u <- read.csv(cdfn_u)
      portfolioDF_u <- portfolioDF_u[,c("contractType","statusDate","contractRole","contractID",
                                        "nominalInterestRate","currency","initialExchangeDate",
                                        "premiumDiscountAtIED","maturityDate","notionalPrincipal",
                                        "rateSpread","description")]
      
      output$portfolioDF_u <- renderDataTable(portfolioDF_u,
                                              options = list(autoWidth = TRUE, scrollX = TRUE))
      
      
      #create the portfolio with the respective files
      ptf1   <-  samplePortfolio(cdfn_u,rfdfn)
      
      
      #create eventSeries for the selected contract
      if(input$rfScenarioCus == "decreasing Rates"){
        plotlistCus <- reactive(simulatePortfolio(ptf1, serverURL(), list(rfx_falling),
                                                  rfx_falling$riskFactorID))
      }
      
      if(input$rfScenarioCus == "increasing Rates"){
        plotlistCus <- reactive(simulatePortfolio(ptf1, serverURL(), list(rfx_rising),
                                                  rfx_rising$riskFactorID))
      }
      if(input$rfScenarioCus == "steady Rates"){
        plotlistCus <- reactive(simulatePortfolio(ptf1, serverURL(), list(rfx_steady),
                                                  rfx_rising$riskFactorID))
      }
      if(input$rfScenarioCus == "recovering Rates"){
        plotlistCus <- reactive(simulatePortfolio(ptf1, serverURL(), 
                                                  list(rfx_recovering),
                                                  rfx_rising$riskFactorID))
      }
      
      output$CFPlotCus <- renderPlot({
        plotlistCus()[[input$analysisTypeCus]]
      })
      
      output$warning <- renderText("")
    }
    else if(is.null(input$uploadedFile) == TRUE){
      output$warning <- renderText("No File uploaded yet")
    }
    
  })   #observe close
  
}      #server close

# Run the application 
shinyApp(ui = ui, server = server)
