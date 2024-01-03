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
           function (analysisID, analysisDescription, enterpriseID,
                                 yieldCurve, portfolio, currency, scenario, 
                                 actusServerURL, timeline) {
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

