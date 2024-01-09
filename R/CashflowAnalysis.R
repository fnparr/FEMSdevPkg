# CashflowAnalysis.R  FEMS dev code by Francis Parr Jan 2024
# included in FEMSdevPkg; Licensing and Copyright notices from there
# Defines class CashflowAnalysis
# Performs liquidity, income and valuation analyses on portfolio of contracts
# for specified timeline, risk scenario and yield curve 
# creates and saves a dataframe with (liquidityD,income, NPV) for each report
# date in the defined timeline 
# **************************************************
# defines: class CashflowAnalysis, CashflowAnalysis() constructor,
# defines and exports:
#    CashflowAnalysis(<enterprise inf>, <portfolio>,<timeline> ...)
#    setPortfolio(<cflana>,<portfolio>)
#    setScenario(<cflana>, <scenario>)
#    simulateContracts(cflana>) 
#    setYieldCurve(<cflana>, <yieldCurve>) )
#    cashflowEventsByPeriod(cfla)
#    doAnalysis(<cflana>)
#.   getAnalysisReports(<cflana>)
# *********************************************************************
# class CashflowAnalysis
# *************************************
setRefClass("CashflowAnalysis",
            fields = list(
              analysisID = "character",
              analysisDescription = "character",
              enterpriseID = "character",
              yieldCurve = "YieldCurve",
              portfolio = "Portfolio",
              currency = "character",
              scenario = "list",
              actusServerURL = "character",
              timeline = "Timeline",
              cashflowEventsLoL = "list",
              cashflowEventsByPeriod = "data.frame",
              incomeLiquidityReports = "data.frame",
              analysisReports = "data.frame"
            ))
# **************************************
# constructors CashflowAnalysis(...) for a cash flow analysis object
# *************************************
# CashflowAnalysis < >  -  generic function definition 
#
#  **** Generic CashflowAnalysis(<>) ********
# Defines generic S4 constructor method on class CashflowAnalysis
setGeneric("CashflowAnalysis",
            function(analysisID, analysisDescription, enterpriseID,
                          yieldCurve, portfolio, currency, scenario, 
                          actusServerURL, timeline
                          ) { standardGeneric("CashflowAnalysis") }
           )
#  ***** No parameters CashflowAnalysis( )
# CashflowAnalysis ( )  - no parameters instance of CashflowAnalysis()  
#   no parameters method for internal use only 
# Creates an empty CashflowAnalysis with no attributes initialized. 
# return  S4 reference with class=CashflowAnalysis no attributes initialized.
setMethod("CashflowAnalysis", c(), 
          function(){ return( new("CashflowAnalysis")) }
          )

# ************************************************************************
# cashflowAnalysis( < > ) constructor to create/initialize a CashflowAnalysis
# ************************************************************************
#' CashflowAnalysis(analysisID, analysisDescription, enterpriseID, yieldCurve, 
#'                  portfolio, currency, scenario, actusServerURL, Timeline )
#'
#'   This method is used to start a cashflow analysis of contract holdings of 
#'   an enterprise. The user supplies information specifying the analysis to 
#'   br performed. The method is a constructor returning an initialized
#'   S4 CashflowAnalysis object which is then used to step through the analysis
#'   process saving results at each step. The completed analysis will provide 
#'   projected liquidity change, income and valuation for each held contract 
#'   at a set of specified report periods for a specified risk scenario. The
#'   risk scenario is defined as a list of projected market interest and 
#'   possibly stock price movements over time. Contract cashflow behavior is 
#'   simulated by calling out to an identified ACTUS server.  Input Yield curve
#'   data is used to set discounting and risk free groth factors used to 
#'   determine a projected contract value at each defined future report time for
#'   each contract in the enterprise portfolio. 
#'   
#'   Steps in the CashflowAnalysis are: (1) Initialization (2) Portfolio 
#'   simulation generating projected cashflow behavior for each contract 
#'   (3) Bucket the cashevents for each contract to enable reporting for each
#'   period in the timeline (4) Compute contract income for each report period 
#'   based in non principal flows for each contract (5) Compute contract 
#'   liquidity for each report period based on all period cashflows for each
#'   (6) compute NetPresentValue for each contract at the start time and at each
#'   report point using aggregated bucket flows for the contract and a 
#'   correction for residual cashflow project to occur beyond the bucket horizon
#'   - applying discounting and growth factors derived from the supplied Yield 
#'   Curve Data (7) Computing some key ratios for the Enterprise based on these 
#'   results    
#'
#' @param analysisID  character: a unique ID for this Cashflow Analysis
#' @param analysisDescription character: a short text describing this analysis
#' @param enterpriseID  character: a unique ID for the enterprise being analysed 
#' @param YieldCurve S4 YieldCurve object used for NetPresentValue discounting
#' @param Portfolio S4 Portfolio object with ACTUS contract enterprise holdings
#' @param currency character label "USD", "CHF" to be used for reports
#' @param scenario list of S4 RiskFactors for  projected market index values
#' @param actusServerURL character - url of an ACTUS server simulating contracts
#' @param timeline S$ Timeline object: future times for which reports generated   
#' 
#' @return    CashflowAnalysis S4 object: initialized/ready for simulation step
#' @export
#' @import    jsonlite
#' @import    httr
#' @examples {
#'    mydatadir <- "~/mydata"
#'    installSampleData(mydatadir)
#'    cdfn  <- "~/mydata/BondPortfolio.csv"
#'    ptf   <-  samplePortfolio(cdfn)
#'    serverURL <- "https://demo.actusfrf.org:8080/"
#'    rxdfp <- paste0(mydatadir,"/UST5Y_fallingRates.csv")
#'    rfx <- sampleReferenceIndex(rxdfp,"UST5Y_fallingRates", "YC_EA_AAA",100)
#'    cfla <- CashflowAnalysis( analysisID = "cfla001",
#'                              analysisDescription = "this_analysis_descr",
#'                              enterpriseID = "entp001",
#'                              yieldCurve = YieldCurve(),
#'                              portfolio =  ptf,
#'                              currency = "USD",
#'                              scenario = list(rfx),
#'                              actusServerURL = serverURL,
#'                              timeline = Timeline())
#'  }
#'
setMethod("CashflowAnalysis", 
          c ( analysisID = "character",
              analysisDescription = "character",
              enterpriseID = "character",
              yieldCurve = "YieldCurve",
              portfolio = "Portfolio",
              currency = "character",
              scenario = "list",
              actusServerURL = "character",
              timeline = "Timeline"
              ), 
           function (analysisID, analysisDescription, enterpriseID, yieldCurve, 
                     portfolio, currency, scenario, actusServerURL, timeline) {
            cfla <- CashflowAnalysis()
            cfla$analysisID <-          analysisID
            cfla$analysisDescription <- analysisDescription
            cfla$enterpriseID <-        enterpriseID
            cfla$yieldCurve <-          yieldCurve
            cfla$portfolio  <-          portfolio
            cfla$currency <-            currency
            cfla$scenario <-            scenario
            cfla$actusServerURL <-      actusServerURL
            cfla$timeline <-            timeline
            return(cfla)
          })

# ************************************************************************
# generateEvents(<CashflowAnalysis> )
# ************************************************************************
#' generateEvents(<cashflowAnalysis>)
#'
#'   The generateEvents(CashflowAnalysis) function takes as input a S4
#'   CashflowAnalysis object with the following attributes set: (1) the 
#'   portfolio of contracts held by the enterprise (2) the actusServerURL
#'   and (3) the scenario / riskFactor list to be used. The function sends a 
#'   simulation request to the designated ACTUS server with the contract and 
#'   risk data and saves the results of the simulation in the cashflowAnalysis
#'   The method return a log message with a report on which contracts were 
#'   successfully simulated
#'
#' @param cfla  CashAnalysis S4 object with portfolio, actusServer and risk data
#' @return      Log summarizing which contracts were successfully simulated 
#' @export
#' @import    jsonlite
#' @import    httr
#' @examples {
#'    mydatadir <- "~/mydata"
#'    installSampleData(mydatadir)
#'    cdfn  <- "~/mydata/BondPortfolio.csv"
#'    ptf   <-  samplePortfolio(cdfn)
#'    serverURL <- "https://demo.actusfrf.org:8080/"
#'    rxdfp <- paste0(mydatadir,"/UST5Y_fallingRates.csv")
#'    rfx <- sampleReferenceIndex(rxdfp,"UST5Y_fallingRates", "YC_EA_AAA",100)
#'    cfla <- CashflowAnalysis( analysisID = "cfla001", 
#'                              analysisDescription = "this_analysis_descr",
#'                              enterpriseID = "entp001", yieldCurve = YieldCurve(),
#'                              portfolio =  ptf, currency = "USD", 
#'                              scenario = list(rfx), 
#'                              actusServerURL = serverURL, 
#'                              timeline = Timeline())
#'    logMsgs  <- generateEvents(cfla = cfla)
#' }
#'
setMethod (f = "generateEvents", 
           signature = c(ptf="missing", serverURL="missing", 
                         riskFactors="missing", cfla = "CashflowAnalysis") ,
           definition = function(cfla){
             # sends input portfolio contracts and riskFactors to server as JSON
             simulationRsp <- simulationRequest(cfla$portfolio,
                                                cfla$actusServerURL,
                                                cfla$scenario
                                                )
             if (simulationRsp$status_code == 200 ){
                cfla$cashflowEventsLoL <- content(simulationRsp)
                logmsg <- "Contract simulations were successful"
             }
             else {
                cfla$cashFlowEvents <- NULL
                logmsg <- paste0("Contract simulation error. status_code= ",
                                 simulationRsp$status_code,
                                 "Error info= ", response_content$error)
             }
             return(logmsg) 
           }
)
             