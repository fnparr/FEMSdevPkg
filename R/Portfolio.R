# Portfolio.R  FEMS dev code by Francis Parr Feb 2022
# Edits/subset of Portfolio.R in main FEMS branch
# Licensing and Copyright notices from there
# **************************************************
# defines: class Portfolio, Portfolio() constructor,
#    add(<portfolio>,<contract_list>), set(<portfolio>,<rfconn>)
#    generateEvents(<portfolio>, <serverURL>)
#    samplePortfolio(<contractDataFilename>)

# *********************************************************************
# class Portfolio
# *************************************
#' class Portfolio
#'
#' A Portfolio consists of a list of contracts and a list of riskFactors
#' defining a scenario, A portfolio argument can be used as the input
#' parameter for a generateEvents( ) request
#'
#' @import methods
#' @importFrom methods new
#' @export Portfolio
#' @exportClass Portfolio
#' @field contracts list.
#' @field riskFactors list.
#'
#' @return  S4 object with class=Portfolio
#'
#' @examples { ptf1 <- Portfolio()}
setRefClass("Portfolio",
            fields = list(
              contracts = "list",   # contracts are instances of ContractType
              riskFactors = "list"  # riskFactors ReferenceIndex with moc
            ))

# **************************************
# constructors Portfolio(...) for a portfolio object
# *************************************
# define generic function Portfolio()
setGeneric(name = "Portfolio",
           def = function(contract, ...){
             standardGeneric("Portfolio")
           })
# and instantiate for the no parameters case
setMethod(f = "Portfolio", signature = c(),
          definition = function( ){
             return(new("Portfolio"))
          })

# allow creation of a portfolio with single contract in its contracts
# useful to run a cash flow generation on a test contract
setMethod(f = "Portfolio", signature = "ContractType",
          definition = function (contract) {
          ptf <- Portfolio()
          ptf$contracts = list(contract)
          ptf$riskFactors <- list()
          return(ptf)
          })

# ************************************************************************
# generateEvents(<Portfolio>)
# *************************************

#' generateEvents
#'
#'   The generateEvents(<Portfolio>, serverURL) function takes an initialized
#'   S4 Portfolio object and a serverURL as inputs, constructs a JSON
#'   representation of the Portfolio contents and calls out using https POST to
#'   the Actus server at URL serverURL to generate a list of cashflow event
#'   lists for each contract in the Portfolio using the portfolio's RiskFactor
#'   scenario data.
#'
#' @param ptf    Portfolio S4 object initialized with contract and risk factors
#' @param serverURL  character - identifies the ACTUS server to be called
#' @return       List of generated cashflow results - one entry per contract
#' @export
#' @import    jsonlite
#' @import    httr
#' @examples {
#'    serverURL <- "https://demo.actusfrf.org:8080/"
#'    cdfn  <- "~/Rprojects/FEMSdevPkg01/data/BondPortfolio.csv"
#'    rfdfn <- "~/Rprojects/FEMSdevPkg01/data/RiskFactors.csv"
#'    ptf   <-  samplePortfolio(cdfn,rfdfn)
#'    cfls  <- generateEvents(ptf,serverURL)
#' }
#'
setGeneric(name = "generateEvents",
           def = function(ptf,serverURL){
             standardGeneric("generateEvents")
           })

setMethod (f = "generateEvents", signature = c("Portfolio","character") ,
            definition = function(ptf,serverURL){
            # send contract and risk factors to the Server as valid JSON

#  Functional programming construction of preJson for Portfolio
            contractDefs <- lapply(ptf$contracts,preJcontract)
            riskFactors <-  preJSONrfs(ptf$riskFactors)
            fin_list <- list(contracts = contractDefs,
                             riskFactors = riskFactors)

            # create final request body in json format
            request_body <- toJSON(fin_list, pretty = TRUE, auto_unbox = FALSE)

            # issue POST command to have server generate cashflows
            response_events <- POST(paste0(serverURL, "eventsBatch"),
                                    body = request_body,
                                    content_type_json())
            response_content <- content(response_events)
            if (response_events$status_code != 200) {
              print(response_content$error)
              stop("ErrorIn::ContractType:: API response error; Check if all necessary contractTerms were set correctly!!!")
            }
            return(response_content)
        })

# ************************************************************
# samplePortfolio(contractDataFileName)  cdfn  FNP 13 April 2022
# ************************************************************
#' samplePortFolio
#'
#' samplePortfolio ( cdfn, rdfn ) takes as input a contracts-data-filepath and
#'     riskfactor- data-filepath, reads this data and returns an initialized
#'     Portfolio object with contracts and risk factors from these csv files.
#' @param cdfn      character string -  a contract-data-filepath
#' @param rfdfn     character string -  a riskfactor-data-filepath
#'
#' @return   Portfolio s4 object initialized with the data from the input files
#' @export
#'
#' @examples {
#'    cdfn  <- "~/Rprojects/FEMSdevPkg01/data/BondPortfolio.csv"
#'    rfdfn <- "~/Rprojects/FEMSdevPkg01/data/RiskFactors.csv"
#'    ptf <- samplePortfolio(cdfn,rfdfn)
#'    }
#'
samplePortfolio <- function(cdfn, rfdfn) {
  ptf <- Portfolio()            # create portfolio object no attributes set
                                # read in contract and riskFactor data from
                                # named files; convert to lists of contract
                                # and riskFactor objects
                                # riskfactors first - contract.moc valid check
  ptf$riskFactors <- riskFactors_df2list(riskFile2dataframe(rfdfn))
  ptf$contracts <-   contracts_df2list(contractFile2dataframe(cdfn))
                                # portfolio is now initialized and ready for
                                # cashflow generation
  return(ptf)
}
