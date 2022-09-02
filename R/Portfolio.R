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
#' @include ContractType.R
#' @import methods
#' @importFrom methods new
#' @export Portfolio
#' @exportClass Portfolio
#' 
#' @field contracts  List of contracts, class=ContractType, in the portfolio.
#' @field riskFactors List of class=ReferenceIndex's defining a risk Scenario.
#'        possibly better to change to riskFactorIndexList 
setRefClass("Portfolio",
            fields = list(
              contracts = "list",   # contracts are instances of ContractType
              riskFactors = "list"  #  ReferenceIndexList with moc
            ))

# **************************************
# constructors Portfolio(...) for a portfolio object
# *************************************
#' Portfolio < >  -  generic function definition 
#'
#' Defines generic S4 constructor method on class Portfolio
#' @param  contract   S4 reference Class=ContractType, a contract to include. 
#' @param  ...        Not used
setGeneric(name = "Portfolio",
           def = function(contract, ...){
             standardGeneric("Portfolio")
           })
#' Portfolio ( )  - no parameters instance of Portfolio< > 
#' 
#' Creates an empty Portfolio object with no attributes initialized. 
#' @return  S4 reference with class=Portfolio and no attributes initialized.
setMethod(f = "Portfolio", signature = c(),
          definition = function( ){
             return(new("Portfolio"))
          })
#' Portfolio("ContractType")  Constructs Portfolio containing a single contract.
#' 
#' This instance of the generic Portfolio< > method takes a reference to a 
#' contract as its input parameter and returns a portfolio with no defined risk 
#' Scenario and this single contract as its contents
#' @param contract  S4 reference class=ContractType
#' @return   S4 reference class=Portfolio, initialized attributes
setMethod(f = "Portfolio", signature = "ContractType",
          definition = function (contract) {
          ptf <- Portfolio()
          ptf$contracts = list(contract)
          ptf$riskFactors <- list()
          return(ptf)
          })

#' generateEvents < >     Generic method definition
#' 
#' Defines a generic method on S4 Class Portfolio. Instances will call out
#' to an ACTUS server at location serverURL to generate cashflow events for 
#' contracts in the portfolio using the risk scenario in the portfolio. 
#' Instances of this generic are: 1. signature ( "Portfolio", serverURL)
#'
#' @param ptf   S4 reference Class=Portfolio
#' @param serverURL  character string, the URL of ACTUS server to call out to. 
#' @return          List of generated cashflow results - one entry per contract
setGeneric(name = "generateEvents",
           def = function(ptf,serverURL){
             standardGeneric("generateEvents")
           })

# ************************************************************************
# generateEvents(<Portfolio>, ServerURL) - instance of generic method 
# ************************************************************************

#' generateEvents(<portfolio>, <ACTUS-server-URL>)
#'
#'   The generateEvents(Portfolio, character) function takes an initialized
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
#'    mydatadir <- "~/mydata"
#'    installSampleData(mydatadir)
#'    cdfn  <- "~/mydata/BondPortfolio.csv"
#'    rfdfn <- "~/mydata/RiskFactors.csv"
#'    ptf   <-  samplePortfolio(cdfn,rfdfn)
#'    serverURL <- "https://demo.actusfrf.org:8080/"
#'    cfls  <- generateEvents(ptf,serverURL)
#' }
#'
setMethod (f = "generateEvents", signature = c("Portfolio","character") ,
            definition = function(ptf,serverURL){
            # send contract and risk factors to the Server as valid JSON

#  Functional programming construction of preJson for Portfolio
            contractDefs <- lapply(ptf$contracts,preJcontract)
            riskFactors <-  preJSONrfxs(ptf$riskFactors)
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
#'    mydatadir <- "~/mydata"
#'    installSampleData(mydatadir)
#'    cdfn  <- "~/mydata/BondPortfolio.csv"
#'    rfdfn <- "~/mydata/RiskFactors.csv"
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

#' getContractIDs  <ptf>     Generic method definition
#' 
#' Defines a generic method on S4 Class Portfolio. Returns a vector with the 
#' contractIDs of all the contracts in the portfolio.
#'
#' @param ptf   S4 reference Class=Portfolio Portfolio with a list of contracts.
#' @return      A vector of character string contractIDs 
setGeneric(name = "getContractIDs",
           def = function(ptf) standardGeneric("getContractIDs"))


#' getContractIDs
#'
#' getContractIDs(ptf) takes as input an S4 ref to a Class=Portfolio object
#'     containing a list of contracts. It returns a vector of character string 
#'     contractID of the contracts in the portfolio. 
#'
#' @param ptf    S4 ref to class=Portfolio object with list of contracts
#' @return       Vector of character string contractIDs -  one for each contract
#'               in the portfolio
#' @export
#' @examples {
#'    mydatadir <- "~/mydata"
#'    installSampleData(mydatadir)
#'    cdfn  <- "~/mydata/BondPortfolio.csv"
#'    rfdfn <- "~/mydata/RiskFactors.csv"
#'    ptf <- samplePortfolio(cdfn,rfdfn)
#'    cids <- getContractIDs(ptf)
#'    }
#'
setMethod (f = "getContractIDs", signature = c("Portfolio") ,
           definition = function(ptf){
             cids <- sapply(ptf$contracts, getCIDfromContract)
             return(cids)
           })


#' getContract <ptf, cid >     Generic method definition
#' 
#'   Defines a generic method on S4 Class Portfolio also taking character string
#'   contractID as its second input. Returns an S4 reference to an object of 
#'   Class=ContractType if the input contractID matches a contract in the 
#'   Portfolio and NULL if it does not.  
#'
#' @param ptf   S4 reference Class=Portfolio Portfolio with a list of contracts
#' @param cid   A character string contractID.
#' @return      An S4 Reference to a portfolio Object if cid is matched or NULL  
setGeneric(name = "getContract", 
           def = function(ptf, cid ) standardGeneric("getContract"))


#' getContract(ptf, cid)     
#'
#' getContract(ptf, cid) takes as input an S4 ref to a Class=Portfolio object
#'     containing a list of contracts and a character string contractID. The 
#'     returns either an S4 ref to a Class=ContractType contract object whose 
#'     contractID the input cid string OR NULL if there is no matching contract
#'     in the portfolio.
#'
#' @param ptf    S4 ref to class=Portfolio object with list of contracts
#' @param cid    character - a contractID string to be matched
#' @return       EITHER an S4 Ref to a class=ContractType object with this input
#'                      cid as its its contractID 
#'               OR NULL if no such match exists 
#' @export
#' @examples {
#'    mydatadir <- "~/mydata"
#'    installSampleData(mydatadir)
#'    cdfn  <- "~/mydata/BondPortfolio.csv"
#'    rfdfn <- "~/mydata/RiskFactors.csv"
#'    ptf <- samplePortfolio(cdfn,rfdfn)
#'    cids <- getContractIDs(ptf)
#'    cntr <- getContract(ptf, cids[1])
#'    }
#'
setMethod ( f = "getContract",  signature = c("Portfolio", "character"),
            definition = function(ptf, cid) {
               cl <- ptf$contracts[
                     sapply(ptf$contracts, function(cntr){
                            getCIDfromContract(cntr) == cid
                     }) ]
               if (length(cl) == 1)  cntr_out <- cl[[1]]
               else  cntr_out = NULL
               return(cntr_out)
            } )
  