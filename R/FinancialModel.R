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
# defines: class FInancialModel, FInancialModel() constructor,
# defines and exports:
#    Account ..
library(data.tree)    
# *********************************************************************
# class FinancialModel
# *************************************
#' @include YieldCurve.R
#' @include Portfolio.R
#' @include Timeline.R
#' @include ContractAnalysis.R
#' @import data.tree
setOldClass("Node")   # Allows data.tree::Node to be used in S4 object slots 
setRefClass("FinancialModel",
            fields = list(
              financialModelID = "character",
              financialModelDescription = "character",
              enterpriseID = "character",
              accounts = "Node",         # define Account extends Node 
              portfolio = "Portfolio",
              directContracts = "list",  # < DirectContract >
              currency = "character",   # all analysis reports same currency 
              timeline = "Timeline",    # all analysis reports same timeline 
              serverURL = "character",  # URL contract simulation ACTUS server 
              analyses = "list",        # < ContractAnalysis> keyed ScenarioID
              resultDfs= "list",     # selected results as dataframes
              resultTrees = "list",  # selected results as trees
              yieldCurve = "YieldCurve"  # as needed for NPV valuations
                        )
           )
# **************************************
# constructor FinancialModel(...) for enterprise balance sheet projections
# *************************************
#  **** Generic FinancialModel(<>) ********
# Defines generic S4 constructor method for class FinancialModel 
# include parameters for 
setGeneric("FinancialModel",
           function(fmID, fmDescr, entprID, accounts, ptf, curr, timeline, 
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
#' @param accounts Node: tree structure, names- account lines in balance sheets
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
    accounts = Node(), ptf = Portfolio(), curr = " ",
    timeline = Timeline(), serverURL = " "
    ) {
  fm <- FinancialModel()
  fm$financialModelID           <- fmID
  fm$financialModelDescription  <- fmDescr
  fm$enterpriseID               <- entprID
  fm$accounts                   <- accounts
  fm$portfolio                  <- ptf
  fm$currency                   <- curr
  fm$timeline                   <- timeline
  fm$serverURL             <- serverURL
  return (fm)
  }
  