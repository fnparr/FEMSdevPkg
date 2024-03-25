#*************************************************************
# Copyright (c) 2015 by ZHAW.
# Please see accompanying distribution file for license.
#*************************************************************
# FNP modifications Feb 2022
# PrincipalAtMaturity
# defines: LinearAmortizer refClass  - extends ContractType
# introduces : Lam() null constructor , LinearAmortizer() constructor

#' @include ContractType.R
#' @import methods
#' @importFrom methods new

setRefClass("LinearAmortizer",
            contains = "ContractType")

setGeneric(name = "Lam",
           def = function(...){
             standardGeneric("Lam")
           })

# New version: no Term parameters passed in so empty term list
# initialize isCompound and contractStructure for LAM
setMethod(f = "Lam", signature = c(),
          definition = function (...) {
            object <- new("LinearAmortizer")
            object$contractTerms<- list()
            object$isCompound <- FALSE
            object$contractStructure <- list()
            return(object)
          })

setGeneric(name = "LinearAmortizer",
           def = function(...){
             standardGeneric("LinearAmortizer")
           })

setMethod(f = "LinearAmortizer", signature = c(),
          definition = function(...){
            object <- Lam(...)
            return(object)
          })
