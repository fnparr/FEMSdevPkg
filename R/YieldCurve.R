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
#' @field yfdcc character  mapped dayCountConvention used in fmdates:year_frac
#' @field compoundingFrequency character "NONE", "YEARLY", "CONTINUOUS"
#'                                       
setRefClass("YieldCurve",
            fields = list(
              yieldCurveID         = "character",
              referenceDate        = "character",
              tenorRates           = "numeric",
              dayCountConvention   = "character",
              yfdcc                = "character",
              compoundingFrequency = "character"
            ))

setGeneric(name = "YieldCurve",
           def = function(yieldCurveID, referenceDate, tenorRates,
                          dayCountConvention, compoundingFrequency )  
                  standardGeneric("YieldCurve") )

# exported constructor with parameters as listed in the generic 
setMethod(f = "YieldCurve", signature = c("character", "character","numeric",
                                     "character", "character"),
          definition= function(yieldCurveID, referenceDate, tenorRates,
                               dayCountConvention, compoundingFrequency) {
            
            # check that dayCountConvention is a valid ACTUS dcc with a mapping
            # to fmdates:year_frac dcc values; store both values in yc 
            yfdccs <- c("30e/360", "30e/360isda", "act/360", "act/365","act/actisda") 
            names(yfdccs) <- c("30E360", "30E360ISDA", "A360", "A365", "AA")
            
            # Permitted values in the function year_frac are: 
            # "30/360", "30/360us", "30e/360", "30e/360isda", "30e+/360", 
            # "act/360", "act/365","act/actisda")
            
            if(dayCountConvention %in% names(yfdccs)) {
              yfdcc <- yfdccs[[dayCountConvention]]
            } else {
              stop(paste("ErrorIn::YieldCurve:: ", dayCountConvention, 
                         " is not a valid ACTUS dcc or no map to a year-frac dcc!",
                         sep=" "))
            }
            
            yc <- new("YieldCurve")
            yc$yieldCurveID <- yieldCurveID
            yc$referenceDate <- referenceDate
            yc$tenorRates <- tenorRates
            yc$dayCountConvention <- dayCountConvention
            yc$yfdcc <- yfdcc
            yc$compoundingFrequency <- compoundingFrequency
            return(yc)
          })

# exported method getForwardRates()
setGeneric(name = "getForwardRates",
           def = function(yieldCurve, Tfrom, Tto )  
             standardGeneric("getForwardRates") )

setMethod(f = "getForwardRates", signature = c("YieldCurve", "character",
                                                "character"),
          definition= function(yieldCurve, Tfrom, Tto) {
# implementation V1 for single Tfrom, Tto value pair with Tfrom < Tto             
              
          })