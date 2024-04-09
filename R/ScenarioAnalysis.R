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
# Defines generic S4 constructor method for class ScenarioAnalysis
setGeneric("ScenarioAnalysis",
           function(scenarioID, marketData, yieldCurve)
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
#' ScenarioAnalysis (required initialization parameters)   
#' internal use only 
#' Creates and returns S4 ref ScenarioAnalysis instance ready for contract 
#' simulationized.
#' @param scenarioID  character ID for this risk Scenario
#' @param marketData  list of risk factor reference indexes
#' @param yieldCurve  a YieldCurve - related to factors for discounting 
#' @return   initialized ScenarioAnalysis object 
#' @export
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
#'   The generateEvents(ScenarioAnalysis) function takes as input: an 
#'   initialized S4 ScenarioAnalysis object with RiskFactor information, (2) a 
#'   Portfolio of ACTUS contracts to be simulated and (3) the URL of an ACTUS
#'   server to compute the cashflow events. The method sends a 
#'   JSON simulation request to the designated ACTUS server with portfolio and 
#'   risk data then saves the results of the simulation in the cashflowEventsLoL
#'   attribute of the ScenarioAnalysis. A log message is saved in the logMsgs
#'   list attribute with key "generateEvents" and returned as ouput of the 
#'   method 
#'
#' @param host  ScenarioAnalysis S4 object with risk scenario data
#' @param ptf   Portfolio of ACTUS contracts to be simulated
#' @param serverURL  character string locating ACTUS server to be used  
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
#'    scnID <- "UST5Y_fallingRates"
#'    yc <- YieldCurve()
#'    scna <- ScenarioAnalysis(scenarioID= scnID, marketData= rfxs, 
#'                             yieldCurve = yc)
#'   logMsgs  <- generateEvents(host= scna, ptf=ptf, serverURL = serverURL)
#' }
#'
setMethod (f = "generateEvents", 
           signature = c(host = "ScenarioAnalysis", ptf="Portfolio", 
                         serverURL="character", riskFactors="missing" ) ,
           definition = function( host, ptf, serverURL){
             # sends input portfolio contracts and riskFactors to server as JSON
             simulationRsp <- simulationRequest(ptf, serverURL, host$marketData)
             if (simulationRsp$status_code == 200 ){
               host$cashflowEventsLoL <- content(simulationRsp)
               logmsg <- "Contract simulations were successful"
             }
             else {
               host$cashflowEventsLoL <- list()
               logmsg <- paste0("Contract simulation error. status_code= ",
                                simulationRsp$status_code)
               #                              "Error info= ", content$error)
             }
             host$logMsgs["generateEvents"]<- logmsg
             return(logmsg) 
           }
)          
 
# ***** eventsdfByPeriod instance   signature = (ScenarioAnalysis, Timeline)   
#' events2dfByPeriod(host = <ScenarioAnalysis>, tl = <Timeline>)
#'
#'   This method reorganizes a list(by contract) of lists of cashflow events
#'   into a data frame with columns for: contractID, period, and for each 
#'   ACTUS cashflow event field. The input ScenarioAnalysis object must have 
#'   run generateEvents(host = scna) to populate scna$cashflowEventsLoL, and
#'   the status of each contract simulation must be "Success" . You can check 
#'   this using: 
#'  > unlist(lapply(cfla$cashflowEventsLoL,function(x){return(x$status)})) 
#'  
#'   If these conditions are met, events2dfByPeriod() will reorganize the data
#'   in cfla$cashflowEventsLoL as a dataframe with columns: 
#'   and save that as scna$cashflowEventsByPeriod for use in subsequent analysis
#'   steps on the ScenarioAnalysis object. 
#'   
#'   A text message is returned reporting on any issues in this processing step.
#'   
#'   Processing steps: (0) check valid host$cashflowEventsLoL, (1) merge 
#'   eventsLOL into eventsDF, (2) add periodIndex column,  (3) sort by 
#'   (contractID, periodIndex), (4) save as host$cashFlowEventsByPeriod. 
#'            
setMethod (f = "events2dfByPeriod", 
        signature = c(host = "ScenarioAnalysis", tl = "Timeline") ,
        definition = function(host,tl){ 
          if (! is.null(host$cashflowEventsLoL) && 
              all(unlist(lapply(host$cashflowEventsLoL,
                                function(x){return(x$status)})) == "Success" ) )
          { logmsg <- "OK" 
            df1 <- mergecfls(host$cashflowEventsLoL)
            df1["periodIndex"] <- sapply( df1$time, 
                                  function(x)
                                    {return(date2PeriodIndex(tl, 
                                                             substr(x,1,10)))})
            df2 <- df1[c( "contractId","periodIndex","time","type", "payoff",
                          "currency", "nominalValue","nominalRate",
                          "nominalAccrued")]
            host$cashflowEventsByPeriod <- df2
         }
         else
         { logmsg <- "Failed - check state of cashflowEventsLoL"}
        host$logMsgs["events2dfByPeriod"]<- logmsg
        return(logmsg)
       })
 