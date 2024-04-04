# ScenarioAnalysis.R  FEMS dev code by Francis Parr April 2024
# included in FEMSdevPkg; Licensing and Copyright notices from there
# Defines class ScenarioAnalysis
# Performs liquidity, income and valuation analyses for a specific risk 
# scenario on the portfolio of ACTUS contracts, specified timeline, serverURL
# passed in a parameters and typically coming from a financialModel.
# It creates and saves lists of cashflow events, a dataframe with bucketized
# liquidity events, and vectors of liquidity, valuations and income reports,
# for the report dates in the input Timeline
# **************************************************
# defines: class ScenarioAnalysis, ScenarioAnalysis() constructor,
# defines ( But does not export) :
#  -- Class FinancialModel exports version of these methods - fewer parameters
#    ScenarioAnalysis( scenarioID,marketData,yieldCurve)

#    setPortfolio(<cflana>,<portfolio>)
#    setScenario(<cflana>, <scenario>)
#    simulateContracts(cflana>) 
#    setYieldCurve(<cflana>, <yieldCurve>) )
#    cashflowEventsByPeriod(cfla)
#    doAnalysis(<cflana>)
#.   getAnalysisReports(<cflana>)
# *********************************************************************
# class ScenarioAnalysis
# *************************************
#' @include YieldCurve.R
#' @include Portfolio.R
#' @include Timeline.R
setRefClass("ScenarioAnalysis",
            fields = list(
              scenarioID = "character",
              marketData = "list",
              yieldCurve = "YieldCurve",
              cashflowEventsLoL = "list",
              cashflowEventsByPeriod = "data.frame",
              contractLiquidityVectors = "list",
              liquidityReports = "list",
              incomeReports = "list",
              nominalValueReports = "list",
              logMsgs = "list"
            ))
# **************************************
# constructor ScenarioAnalysis(...) for a scenario analysis object
# *************************************
#  **** Generic ScenarioAnalysis(<>) ********
# Defines generic S4 constructor method for class ContractAnalysis
setGeneric("ScenarioAnalysis",
           function(scenarioID, marketData,yieldCurve)
             { standardGeneric("ScenarioAnalysis") }
)
#  ***** No parameters ScenarioAnalysis( )
# ScenarioAnalysis ( )  - no parameters instance of ScenarioAnalysis()  
#   no parameters method for internal use only 
# Creates an empty ScenarioAnalysis with no attributes initialized. 
# return  S4 reference with class=ScenarioAnalysis no attributes initialized.
setMethod("ScenarioAnalysis", c(), 
          function(){ return( new("ScenarioAnalysis")) }
)

#  ***** Initial values ScenarioAnalysis( )
# ScenarioAnalysis (required initialization parameters)   
# internal use only 
# Creates and returns S4 ref ScenarioAnalysis instance ready for contract 
# simulationized. 

setMethod("ScenarioAnalysis", c(scenarioID = "character",
                                marketData = "list",
                                yieldCurve = "YieldCurve"),
          function(scenarioID,marketData,yieldCurve) {
            scna <- ScenarioAnalysis()
            scna$scenarioID <- scenarioID
            scna$marketData <- marketData
            scna$yieldCurve <- yieldCurve
            scna$cashflowEventsLoL <- list()
            scna$contractLiquidityVectors <- list()
            scna$liquidityReports <- list()
            scna$incomeReports <- list()
            scna$nominalValueReports <-list()
            scna$logMsgs <- list() 
            return(scna)
          })

# ************************************************************************
# generateEvents(ScenarioAnalysis, Portfolio, ServerURL )
# ************************************************************************
#' generateEvents(ScenarioAnalysis, Portfolio, ServerURL )
#'
#'   The generateEvents(ScenarioAnalysis) function takes as input:(1) a 
#'   Portfolio of ACTUS contracts to be simulated, (2) the URL of an ACTUS
#'   server to compute the cashflow events and (3) an initialized S4 
#'   ScenarioAnalysis object with RiskFactor information. The method sends a 
#'   JSON simulation request to the designated ACTUS server with portfolio and 
#'   risk data then saves the results of the simulation in the cashflowEventsLoL
#'   attribute of the ScenarioAnalysis. A log message is saved in the logMsgs
#'   list attribute with key "generateEvents" and returned as ouput of the 
#'   method 
#'
#' @param ptf   Portfolio of ACTUS contracts tobe simulated
#' @param serverURL  character string locating ACTUS server to be used  
#' @param scna  ScenarioAnalysis S4 object with risk scenario data
#' @return      Log message listing which contracts were successfully simulated 
#' @export
#' @import    jsonlite
#' @import    httr
#' @examples {
#'    mydatadir <- "~/mydata"
#'    installSampleData(mydatadir)
#'    cdfn  <- "~/mydata/TestPortfolio.csv"
#'    ptf   <-  samplePortfolio(cdfn)
#'    serverURL <- "https://demo.actusfrf.org:8080/"
#'    rxdfp <- paste0(mydatadir,"/UST5Y_fallingRates.csv")
#'    rfx <- sampleReferenceIndex(rxdfp,"UST5Y_fallingRates", "YC_EA_AAA",100)
#'    rfxs <-list(rfx)
#'    scnID= "UST5Y_fallingRates"
#'    scna <- ScenarioAnalysis(scenarioID=scnID, marketData= rfxs, 
#'                             yieldCurve = yc)
#'   logMsgs  <- generateEvents(ptf=ptf, serverURL = serverURL, scna = scna)
#' }
#'
setMethod (f = "generateEvents", 
           signature = c( ptf="Portfolio", serverURL="character", 
                          riskFactors="missing", scna = "ScenarioAnalysis") ,
           definition = function( ptf, serverURL, scna ){
             # sends input portfolio contracts and riskFactors to server as JSON
             simulationRsp <- simulationRequest(ptf,serverURL, scna$marketData)
             if (simulationRsp$status_code == 200 ){
               scna$cashflowEventsLoL <- content(simulationRsp)
               logmsg <- "Contract simulations were successful"
             }
             else {
               scna$cashflowEventsLoL <- list()
               logmsg <- paste0("Contract simulation error. status_code= ",
                                simulationRsp$status_code)
               #                              "Error info= ", content$error)
             }
             scna$logMsgs["generateEvents"]<- logmsg
             return(logmsg) 
           }
)          
          