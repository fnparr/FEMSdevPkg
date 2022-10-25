
#*************************************************************
# Copyright (c) 2015 by ZHAW.
# Please see accompanying distribution file for license.
#*************************************************************
# ContractType definition
##############################################################
#' ContractType is  Reference Class parent to all ACTUS Contract Types
#'
#' This class is only used in an abstract sense in that it is
#' not intended to create instances of it. It's use is to
#' serve as parent to various implementations of ACTUS CTs as
#' a means for designing method-inheritance.'
#'
#' @import methods
#' @importFrom methods new
#' @include ContractABC.R
#' @include ContractModel.R

# Introduces ContractType refClass,  terms, RF, event, val -Eng
#  CT() generic constructor
#  set(contract_obj, list)  for copying in terms  but comment consistency check

# FNP why does a contract need a risk factor connector - portfolio ?
# in fact it should not - even suspicious that portfolio has a risk factor
# because may be used with multiple scenarios - BUT with old form of
# ACTUS cashflow api - portfolio and simulation run are confused

setRefClass("ContractType",
            contains = "ContractABC",
            fields = list(
                contractTerms = "list",  # of terms
                isCompound = "logical",   # if false contractStructure == list()
                contractStructure = "list"  # of contract legs
            ))

setGeneric(name = "CT",
           def = function(contract_name){
             standardGeneric("CT")
           })

setMethod(f = "CT", signature = c("character"),
          definition = function(contract_name) {
            # FNP cannot find actusDictionary so comment out test
            # if (!contract_name %in% names(actusDictionary$rflActus_attributes)) {
            #   stop(paste("ErrorIn::ContractType:: Type of Contract ",
            #              contract_name, " does not exist !!!"))
            # }
            return( new (contract_name))
          })

# FNP is set(object, xxx) a default generic on reference Classes

setMethod(f = "set", signature = c("ContractType", "list"),
          definition = function(object, what){
            for (i in 1:length(what)) {
              object$contractTerms[names(what[i])] <- what[[i]]
            }
            #  FNP comment out consistency check()
            #   details <- getContractModel(object)
            #   checkArguments(details, what)
          })

#getCIDfromContract(object)  expects object to be a ContractType instance
#   returns the ContractID as "character"
# using in add(<Portfolio>,<list>) in an lapply step to get list of CIDs
getCIDfromContract <- function(object) {
  return ( object$contractTerms$contractID)
}

# getTermValueFromContract takes ContractType object and character termName
# as input - retuns the term Value or NA if not existing
getTermValueFromContract <- function( contract, termName){
  tval <- contract$contractTerms[[termName]]
  if (is.null(tval)) { tval <- NA }
  return ( tval )
}

# ***********************************
# preJcontract(contract)  builds JSON list of lists for a contract
#
preJcontract <- function(contract){
   outJlist <- preJtermList(contract$contractTerms) # start with terms
   # preJSON for the contract structure must be added as a "term"
   # for structured contracts
   if (contract$isCompound){
      outJlist <- append(outJlist,
                         list( contractStructure =
                               preJSONcleglist(contract$contractStructure)
                             )
                         )
   }
   return(outJlist)
}

# **************************************
# preJtermList(term) builds preJSON list of lists for a contract term List
# initializes and uses a (constant) list of  Date_Term_Names
#
preJtermList <- function(terms){
  Date_Term_Names <- c(
    "statusDate","contractDealDate","initialExchangeDate",
    "maturityDate","cycleAnchorDateOfInterestCalculationBase",
    "amortizationDate","contractDealDate","cycleAnchorDateOfPrincipalRedemption",
    "arrayCycleAnchorDateOfPrincipalRedemption","purchaseDate",
    "terminationDate","cycleAnchorDateOfScalingIndex",
    "cycleAnchorDateOfRateReset","cycleAnchorDateOfInterestPayment",
    "capitalizationEndDate")
  dateTerms <- terms[sapply(names(terms),
                            function(x){x %in% Date_Term_Names})]
  nonDateTerms <- terms[sapply(names(terms),
                               function(x){!(x %in% Date_Term_Names)})]
  # start with nonDate terms jsonlite::unbox each atomic value
  outJlist <- lapply(nonDateTerms, function(x){jsonlite::unbox(x)})
  # append date terms converting to JSON dates and jsonlite::unboxing
  outJlist <- append(outJlist, lapply(dateTerms, function(x){jsonlite::unbox(paste0(x,"T00:00:00"))}))
  return(outJlist)
}
