#*************************************************************
# Copyright (c) 2015 by ZHAW.
# Please see accompanying distribution file for license.
#*************************************************************
# FNP modifications Feb 2022
# PrincipalAtMaturity
# defines: PrincipalAtMaturity refClass  - extends ContractType
# introduces : Pam() null constructor , PrincipalAtMaturiy() constructor

#' @include ContractType.R
#' @import methods
#' @importFrom methods new

setRefClass("PrincipalAtMaturity",
            contains = "ContractType")

setGeneric(name = "Pam",
           def = function(...){
             standardGeneric("Pam")
           })

# ***********************************************************
# FNP 7 April 2022 : this impl of Pam is used, seems to waste
# time setting attributes with sample data to be overwritten
# ***********************************************************
# New version: no Term parameters passed in so empty term list
# initialize isCompound and contractStructure for PAM
setMethod(f = "Pam", signature = c(),
          definition = function (...) {
            object <- new("PrincipalAtMaturity")
            object$contractTerms<- list()
            object$isCompound <- FALSE
            object$contractStructure <- list()
            return(object)
          })

setGeneric(name = "PrincipalAtMaturity",
           def = function(...){
             standardGeneric("PrincipalAtMaturity")
           })

setMethod(f = "PrincipalAtMaturity", signature = c(),
          definition = function(...){
            object <- Pam(...)
            return(object)
          })
