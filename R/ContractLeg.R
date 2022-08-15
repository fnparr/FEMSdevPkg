# ContractLeg.R  defines class ContractLeg  - a leg in a structured contract
#  ************************************************************************
# A Reference Class representing a contractLeg within a structured contract
# as of 10th April 2022 - cashflow generation by the Actus core library
# has been tested (by FNP) only for underlier marketObjectCode legs -
# Underlier stock Contract may also be feasible but more validation needed
#  ************************************************************************
#' @import methods
#' @importFrom methods new
#' @importFrom methods new
setRefClass("ContractLeg",
            fields = list(marketObjectCode = "character",
                          referenceType    = "character",
                          referenceRole    = "character"
            ))
# later we may replace the marketObjectCode attribute  with object = "list"

setGeneric(name = "CLeg",
           def = function(moc){
             standardGeneric("CLeg")
           })

setMethod(f = "CLeg", signature = c("character"),
          definition = function(moc) {
             cleg <- new("ContractLeg")
             cleg$marketObjectCode <- moc
             cleg$referenceType <- "MOC"
             cleg$referenceRole <-"UDL"
             return(cleg)
          })

# ********************************************************
# preJSONcleglist
# reqd JSON is  a term: "contractStructure":
#    [{"object": {"marketObjectCode": "AAPL"},
#                "referenceType": "MOC","referenceRole": "UDL"}],

# preJSONcleglist takes list of clegs returns preJ for: [ <cleg> ]
preJSONcleglist <- function(cleglst) {
    clegseq <- lapply(cleglst, preJSONcleg)
    names(clegseq) <- NULL  # JSON sequence is unnamed clegs
    return(clegseq)
}

# preJSONcleg prepares cleg for:  { "object" : ... etc ... }
preJSONcleg <- function(cleg) {
    clegj <- list(object= list(marketObjectCode = jsonlite::unbox(cleg$marketObjectCode)),
                  referenceType = jsonlite::unbox(cleg$referenceType),
                  referenceRole = jsonlite::unbox(cleg$referenceRole))
    return(clegj)
}
