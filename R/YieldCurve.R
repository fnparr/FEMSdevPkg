# YieldCurve.R  FEMSdevPkg code by Francis Parr Oct 2023 
#   updates earlier git wbreymann/FEMS code by Henriette-Elise Breymann
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

# ***********************************************************************
#  YieldCurve() exported constructor for YieldCurve objects 
# ************************************************************************
#' YieldCurve(yieldCurveID, referenceDate, tenorRates, dayCountConvention,
#'             compoundingFrequency )
#'
#'   YieldCurve(character, character, namedNumericVector, character,character)
#'   function takes as input: (1) a YieldCurveID label, (2) a referenceDate
#'   character string, (3) a named vector of numeric tenorRates, (4) a 
#'   character string ACTUS dayCountConvention and (5) a character string 
#'   compoundingFrequency with values in { "NONE", "YEARLY","CONTINUOUS"}.
#'   The function checks that the input dayCountConvention is a valid ACTUS code
#'   and mappable to the dayCountConvention values supported in 
#'   fmdates::year_frac() - used to compute arbitrage free forward rates.  
#'   An S4 YieldCurve object is created and returned with attributes initialized
#'   to these values. 
#'
#' @export
#' @param yieldCurveID  character  label uniquely identifying this yieldCurve
#' @param referenceDate character date yyyy-mm-dd tenorRates observed this day 
#' @param tenorRates numeric  pa rates (0.02=2%) vector, names "1M", "2Y" etc 
#' @param dayCountConvention character: "30E360","30E360ISDA","A360","A365","AA"
#' @param compoundingFrequency character: "NONE", "YEARLY", "CONTINUOUS"
#' @return               fully initialized S4 class YieldCurve object
#' @export
#' @examples {
#'    ycID <- "yc001"
#'    rd <- "2023-10-31"
#'    tr <-  c(1.1, 2.0, 3.5 )
#'    names(tr) <- c("1M", "1Y", "5Y")
#'    dcc <- "30E360"
#'    cf <- "NONE"
#'    yc <- YieldCurve(ycID,rd,tr,dcc,cf)
#' }
#'
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

setGeneric(name = "getForwardRates",
           def = function(yieldCurve, Tfrom, Tto )  
             standardGeneric("getForwardRates") )

# ************************************************************************
# getForwardRates(<YieldCurve>, Tfrom, Tto) 
# ************************************************************************
#' getForwardRates(<yieldCurve>, Tfrom, Tto)
#'
#'   Function getforwardRates(YieldCurve, character, character) takes as
#'   input: (1) an initialized S4 YieldCurve object, (2) a character Tfrom date
#'   in yyyy-mm-dd format and (3) a character Tto date in yyyy-mm-ff from. 
#'   The function returns the perannum forward interest rate for a loan starting
#'   at Tfrom and maturing at Tto based on arbitrage free projection od the 
#'   supplied YieldCurve data with dayCountConvention and compoundung as 
#'   set in attributes of this yield Curve. 
#'   Tfrom must be earlier (<) Tto 
#'   
#'   getForwardRates() uses yearFraction() which in turn depends on and includes 
#'   fmdates::year_frac(), lubridate::ymd(); getForwardRates() also uses   
#'   function approx() from RBase to interpolate YieldCurve values
#'   
#'   The initial implementation of getForwardrates() restricts Tfrom and Tto to
#'   single date strings rather than vectors and compoundingFrequency == "NONE"  
#'
#' @param yieldCurve  class=YieldCurve S4 object with tenorRates, referenceDate
#' @param Tfrom character  yyyy-mm-dd date for start of forward rate interval
#' @param Tto   character yyyy-mm-dd date for end of forward rate interval  
#' @return Projected pa interest rate on loan from Tfrom to Tto using YieldCurve
#' @export
#' @include yearFraction.R  year fractions with specified dayCountConvention
#' @examples {
#'    ycID <- "yc001"
#'    rd <- "2023-10-31"
#'    tr <-  c(1.1, 2.0, 3.5 )
#'    names(tr) <- c("1M", "1Y", "5Y")
#'    dcc <- "30E360"
#'    cf <- "NONE"
#'    yc <- YieldCurve(ycID,rd,tr,dcc,cf)
#'    rate <- getForwardRates(yc,"2024-01-01","2024-06-01")
#' }
#'
setMethod(f = "getForwardRates", signature = c("YieldCurve", "character",
                                                "character"),
          definition= function(yieldCurve, Tfrom, Tto) {
# implementation V1 for single Tfrom, Tto value pair with Tfrom < Tto             
              
          })