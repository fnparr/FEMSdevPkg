#*************************************************************
# Copyright (c) 2015 by ZHAW.
# Please see accompanying distribution file for license.
#*************************************************************
# FNP modifications 7th - 10th Apr 2022
# Option
# defines: Option refClass  - extends ContractType
# introduces : Optns() null constructor , Option() constructor

setRefClass("Option",
            contains = "ContractType")

setGeneric(name = "Option",
           def = function(...){
             standardGeneric("Option")
           })

setMethod(f = "Option", signature = c(),
          definition = function(...){
            return ( new ("Option"))
          })

setGeneric(name = "Optns",
           def = function(cleg, ... ){
             standardGeneric("Optns")
           })

# ***********************************************************
# FNP 10 April 2022 : this impl of Optns  Creates the option then sets
# as Structure contract with input ContractLeg . Modelled on 7April2022
# of Pam method time setting attributes with sample data to be overwritten
# expectone underlying leg to be passed in - but could be a list of legs
# ***********************************************************

setMethod(f = "Optns", signature = c("ContractLeg"),
          definition = function (cleg) {
            object <- new("Option")
            object$contractTerms<- list()
            object$isCompound <- TRUE
            object$contractStructure <- list(cleg)
            return(object)
          })
