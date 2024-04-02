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
              LogMsgs = "list"
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
            scna$LogMsgs <- list() 
            return(scna)
          })
          
          