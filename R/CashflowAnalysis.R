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

# *********************************
# Generic method to create/initialize CashflowAnalysis- needs export + document
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
#'                              actusServerURL = "https://demo.actusfrf.org:8080/", 
#'                              timeline = Timeline())
#'    logMsgs  <- generateEvents(cfla = cfla)
#' }
#'
setMethod (f = "generateEvents", 
           signature = c(ptf="missing", serverURL="missing", 
                         riskFactors="missing", cfla = "CashflowAnalysis") ,
           definition = function(cfla){
             # sends input portfolio contracts and riskFactors to server as JSON
             simulationRsp <- simulationRequest(ptf = cfla$portfolio,
                                                serverURL = cfla$actusServerURL,
                                                riskFactors = cfla$scenario
                                                )
             if (simulationRsp$status_code == 200 ){
                cfla$cashflowEventsLoL <- content(simulationRsp)
                logmsg <- "Contract simulation was successful"
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
             