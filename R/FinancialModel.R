# ******************
# FinancialModel.R  FEMS dev code by Francis Parr Feb 2024
# included in FEMSdevPkg; Licensing and Copyright notices from there
# Defines class FinancialAnalysis 
# Aggregated analysis of projected income, liquidity, and valuations under 
# different risk scenarios for account lines in tree structured projected 
# balance sheets for the enterprise. Holdings of the enterprise are modelled
# either as ACTUS contracts for which future cashflows can be simulated or 
# as directContracts providing formulae or data for future report values 
# **************************************************
# 7 Feb 2024 - start no YieldCurve Attribute, NominalValues only 
#            - will create Analysis from contractsAnalysis - refactored 
# ****************
# defines: class FinancialModel, FinancialModel() constructor,
# defines and exports:
#    Account ..
# library(data.tree)    
# *********************************************************************
# class FinancialModel
# *************************************
#' @include YieldCurve.R
#' @include Portfolio.R
#' @include Timeline.R
#' @include ScenarioAnalysis.R
#  #' @import data.tree
#' 
# setOldClass("Node")   # Allows data.tree::Node to be used in S4 object slots 
setRefClass("FinancialModel",
            fields = list(
              financialModelID = "character",
              financialModelDescription = "character",
              enterpriseID = "character",
              accountsTree = "AccountsTree",       
              portfolio = "Portfolio",
              currency = "character",   # all analysis reports same currency 
              timeline = "Timeline",    # all analysis reports same timeline 
              serverURL = "character",  # URL contract simulation ACTUS server 
              scenarioAnalysisList = "list",  # < ScenarioAnalysis> keyed scnID
              currentScenarioAnalysis = "ScenarioAnalysis"
                        )
           )
# **************************************
# constructor FinancialModel(...) for enterprise balance sheet projections
# *************************************
#  **** Generic FinancialModel(<>) ********
# Defines generic S4 constructor method for class FinancialModel 
# include parameters for 
setGeneric("FinancialModel",
           function(fmID, fmDescr, entprID, accntStr, ptf, curr, timeline, 
                    serverURL
                   ) { standardGeneric("FinancialModel") }
          )
#  ***** No parameters FinancialModel( )
# FinancialModel( )  - no parameters instance of FinancialModel()  
#   no parameters method for internal use only 
# Creates an empty FinancialModel instance with no attributes initialized. 
# return  S4 reference with class=FinancialModel no attributes initialized.
setMethod("FinancialModel", c(), 
          function(){ return( new("FinancialModel")) }
)
# ******* Pre Analysis FinancialModel() constructor 
# Allows: (fmID,fmDesc,entprID, accounts, ptf,curr,timeline,serverURL)
# ************************************************************************
#' FinancialModel( < > ) constructor to create a financial model and set 
#' its pre-analysis attributes
#' FinancialModel(fmID, fmDescr, entprID, accounts, ptf, curr, timeline, 
#'                serverURL) 
#'
#' @param fmID   character: a unique ID for this financial model 
#' @param fmDescr character: a short text describing the financial model 
#' @param entprID character: a unique ID for the enterprise being modelled 
#' @param accntsTree AccountsTree - enterprise accounts structure and CIDs
#' @param ptf Portfolio: list of enterprise holdings - ACTUS contracts 
#' @param curr character: currency for all analysis amounts e.g. CHF, EUR, USD  
#' @param timeline Timeline - sets timing of projected balance sheet reports
#' @param serverURL character URL of ACTUS contract simulation server 
#' 
#' @return  FinancialModel S4 object: ready for analyses to be added 
#' @export
#'
initFinancialModel <- function( 
    fmID = " ", fmDescr = " ", entprID = " ",
    accntsTree = AccountsTree(), ptf = Portfolio(), curr = " ",
    timeline = Timeline(), serverURL = " "
    ) {
  fm <- FinancialModel()
  fm$financialModelID           <- fmID
  fm$financialModelDescription  <- fmDescr
  fm$enterpriseID               <- entprID
  fm$accountsTree               <- accntsTree
  fm$portfolio                  <- ptf
  fm$currency                   <- curr
  fm$timeline                   <- timeline
  fm$serverURL                  <- serverURL
  fm$scenarioAnalysisList       <- list()
  return (fm)
  }
  
addScenarioAnalysis <- function( fm = FinancialModel(), scnID = " ", 
                                 rfxs = list(), yc = YieldCurve()
){ scna <- ScenarioAnalysis(scenarioID=scnID, marketData= rfxs, yieldCurve = yc)
   fm$scenarioAnalysisList[scnID] <- list(scna=scna)
   fm$currentScenarioAnalysis <- scna
   msg<- "new scenarioAnalysis added to Financial Model and made current"
   return(msg)
}
  
# ************************************************************************
# generateEvents(FinancialModel)
# ************************************************************************
#' generateEvents(FinancialModel)
#'
#'   The generateEvents(Financial) function takes as input an 
#'   initialized S4 FinancialModel object with at least one ScenarioAnaysis  
#'   added so that currentScenarioAnalysis is set. The method will simulate all
#'   contracts in the financialModel portfolio, with the risk environment of the
#'   currentScenarioAnalysis. The cashflow events generated are saved as data
#'   in the ScenarioAnalysis 
#'
#' @param host  FinancialModel S4 object with a currentScenarioAnalysis defined
#' @return      Log message listing which contracts were successfully simulated 
#' @export
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
           signature = c(host = "FinancialModel", ptf="missing", 
                         serverURL="missing", riskFactors="missing" ) ,
           definition = function( host ){
            # invokes generateEvents( ) on currentScenarioAnalysis passing 
            # fm$portfolio and fm$serverURL as parameters 
            logmsg <- generateEvents(host = host$currentScenarioAnalysis,
                                     ptf = host$portfolio,
                                     serverURL = host$serverURL)
            return(logmsg) 
           }
)          