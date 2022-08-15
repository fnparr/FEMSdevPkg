#*************************************************************
# Copyright (c) 2020 by ZHAW.
# Please see accompanying distribution file for license.
#*************************************************************
# modification FNP feb 2022
# ContractModel.R  ContractModel refClass  set()  generic
##############################################################
#' A Reference Class that represents all Contract Models objects
#'
#' This class is only used in a virtual sense in that it is
#' not intended to create instances of it. It's use is to
#' provide the class structure, i.e. field definitions, to
#' various implementations of Contract Models for specific
#' Contract Types and as a means for designing method-inheritage.

# some additional notes about references to Java in the original file

setRefClass("ContractModel",
            fields = list(
              contract_type = "character",
              contractTerms = "list",
              required = "list",
              allowed = "list"
            )
)

setGeneric(name = "set", useAsDefault = TRUE,
           def = function(object, what, ...){
             standardGeneric("set")
           })

# FNP - no real need for a generic get()
# think the intent is to pass in  lists of attribute names
# but getting back vectors of results is very messy
# comment out and postpone

# setGeneric(name = "get", useAsDefault = TRUE,
#           def = function(object, what, ...){
#             standardGeneric("get")
#           })

