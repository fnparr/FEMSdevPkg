#*************************************************************
# Copyright (c) 2015 by ZHAW.
# Please see accompanying distribution file for license.
#*************************************************************
# FNP modifications Jul 2022
# Annuity
# defines: Annuity refClass  - extends ContractType
# introduces : Ann() constructor , Annuity() constructor
# Annuity() will be the constructor from longName() Ann() does
# the work of setting simple structure - terms set in ContractType:CT
# defines: Annuity refClass  - extends ContractType

#' @include ContractType.R
#' @import methods
#' @importFrom methods new
#' 
setRefClass("Annuity",
            contains = "ContractType")


# mark for deletion Ann()   Annuity() does everythig 

# setGeneric(name = "Ann",
#           def = function(...){
#             standardGeneric("Ann")
#           })

# setMethod(f = "Ann", signature = c(),
#          definition = function (...) {
#            object <- new("Annuity")
#            object$contractTerms<- list()
#            object$isCompound <- FALSE
#            object$contractStructure <- list()
#            return(object)
#          })

setGeneric(name = "Annuity",
           def = function(...){
             standardGeneric("Annuity")
           })

setMethod(f = "Annuity", signature = c(),
          definition = function(...){
            object <- new("Annuity")
            object$contractTerms<- list()
            object$isCompound <- FALSE
            object$contractStructure <- list()
            return(object)
          })
