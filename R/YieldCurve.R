# YieldCurve.R  FEMSdevPkg code by Francis Parr Oct 2023 
#   updates earlier FEMS code by Henriette-Elise Breymann
# Licensing and Copyright notices to be added  XXXX
# **************************************************
# defines: class YieldCurve, YieldCurve() constructor,
#    getForwardRates(<yieldCurve>,<Tfrom>,<Tto>)
# *********************************************************************
# class YieldCurve 
# *************************************
#' class YieldCurve
#'
#' A YieldCurve object holds data for an (interest rate) yield curve for 
#' some risk class (most likely riskfree) at a particular reference date. 
#' This data is organized as a set of spot rates for user selected tenors 
#' at the time of the reference data.  
#' 
#' @include externalFunctionWrappers.R  
#' @import methods
#' @importFrom methods new
#' @export YieldCurve
#' @exportClass YieldCurve 
#' @field yieldCurveID  character label identifying this yieldcurve object
#' @field referenceDate character yyyy-mm-dd date for when YC valid 
#' @field rates numeric vector spot rates on refdate; rates are per annum 
#'              i.e. 2% pa = 0.02    Each rated labelled with tenor 
#'              rates$names = "1D" "1W" "1M" "3M" "6M" "1Y" "2Y" "5Y"  
#' @field dayCountConvention character ACTUS string eg "30E360"
#' @field compoundingFrequency character "NONE", "YEARLY", "CONTINUOUS"
#'                                       
setRefClass("YieldCurve",
            fields = list(
              yieldCurveID         = "character",
              referenceDate        = "character",
              tenorRates           = "numeric",
              dayCountConvention   = "character",
              compoundingFrequency = "character"
            ))

setGeneric(name = "YieldCurve",
           def = function(yieldCurveID, referenceDate, tenorRates,
                          dayCountConvention, compoundingFrequency )  
                  standardGeneric("YieldCurve") )

# constructor with parameters as listed in the generic 
setMethod(f = "YieldCurve", signature = c("character", "character","numeric",
                                     "character", "character"),
          definition= function(yieldCurveID, referenceDate, tenorRates,
                               dayCountConvention, compoundingFrequency) {
            yc <- new("YieldCurve")
            yc$yieldCurveId <- yieldCurveID
            yc$referenceDate <- referenceDate
            yc$tenorRates <- tenorRates
            yc$dayCountConvention <- dayCountConvention
            yc$compoundingFrequency <- compoundingFrequency
            return(yc)
          })
