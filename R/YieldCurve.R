# YieldCurve.R  FEMSdevPkg code by Francis Parr Oct 2023 
#   updates earlier git wbreymann/FEMS code by Henriette-Elise Breymann
# Licensing and Copyright notices to be added  XXXX
# **************************************************
# defines: class YieldCurve, YieldCurve() constructor,
#    getForwardRates(<yieldCurve>,<Tfrom>,<Tto>)
# *********************************************************************
# class YieldCurve 
# *************************************
# class YieldCurve
#
# A YieldCurve object holds data for an (interest rate) yield curve for 
# some risk class (most likely riskfree) at a particular reference date. 
# This data is organized as a set of spot rates for user selected tenors 
# at the time of the reference date.  
#
 
setRefClass("YieldCurve",
            fields = list(
              yieldCurveID         = "character",
              referenceDate        = "character",
              tenorRates           = "numeric",
              tenorYfs            = "numeric",
              dayCountConvention   = "character",
              yfdcc                = "character",
              compoundingFrequency = "character"
            ))


# ***********************************************************
# tenorNames2yfs(tnames)
#    function to convert a vector of tenor names to a numeric vector of
#    year fractions e.g. tnames= ["1D" "1W" "1M" "6M" "1Y" "1.5Y" "2Y"] 
#    used in YieldCurve( ) constructor but not exported; converted tenorNames
#    vector saved in yc$tenorYffs
# ***********************************************************
tenorNames2yfs <- function(tnames) {
  # computing tenors as year fractions 
  # get units as year fractions; pick up last char of tenorNames  and map to
  # year fraction for unit; inner product with tenorName number strings, last
  # char dropped, converted to numeric 
  
  pyfs <-  c(1/365, 7/365, 1/12, 1.0 )   # simple yf for D W M Y
  names(pyfs) <- c("D","W","M","Y")
  tenorYfs <- as.numeric(substr(tnames,1,nchar(tnames)-1))* pyfs[ 
    substr(tnames,nchar(tnames),nchar(tnames))]
  
  # could be made more accurate by (1) different pyfs for 360,365 day years etc 
  # or possibly  (2) convert to week and month multiples then use 
  # yc$referenceDate and convert with date arithmetic followed by yearFraction( ) 
  # - but do we want tenorRates to be so date senyitive?
  return(tenorYfs)
}
# ************************************************************************

setGeneric(name = "YieldCurve",
           def = function(yieldCurveID, referenceDate, tenorRates,
                          dayCountConvention, compoundingFrequency){  
                  standardGeneric("YieldCurve")})

# YieldCurve ( )  - no parameters instance of YieldCurve 
#  
# Creates an empty YieldCurve with no attributes initialized.
#   S4 reference with class=YieldCurve and no attributes initialized.
setMethod(f = "YieldCurve", signature = c(),
          definition = function( ){
            return(new("YieldCurve"))
          }
)

# ***********************************************************************
#  YieldCurve() exported constructor for YieldCurve objects 
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
#' @param yieldCurveID  character  label uniquely identifying this yieldCurve
#' @param referenceDate character date yyyy-mm-dd tenorRates observed this day 
#' @param tenorRates numeric  pa rates (0.02=2pc) vector, names "1M", "2Y" etc
#' @param dayCountConvention character: "30E360","30E360ISDA","A360","A365","AA"
#' @param compoundingFrequency character: "NONE", "YEARLY", "CONTINUOUS"
#' @return               fully initialized S4 class YieldCurve object
#' @export
#' @examples {
#'    ycID <- "yc001"
#'    rd <- "2023-10-31"
#'    tr <-  c(1.1, 2.0, 3.5 )/100
#'    names(tr) <- c("1M", "1Y", "5Y")
#'    dcc <- "30E360"
#'    cf <- "NONE"
#'    yc <- YieldCurve(ycID,rd,tr,dcc,cf)
#' }
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
            
            yc <- YieldCurve()
            yc$yieldCurveID <- yieldCurveID
            yc$referenceDate <- referenceDate
            yc$tenorRates <- tenorRates
            yc$tenorYfs  <- tenorNames2yfs(names(tenorRates))
            yc$dayCountConvention <- dayCountConvention
            yc$yfdcc <- yfdcc  # mapped from dayCountConvention with error check
            yc$compoundingFrequency <- compoundingFrequency
            return(yc)
          })

setGeneric(name = "getForwardRates",
           def = function(yc, Tfrom, Tto ){  
             standardGeneric("getForwardRates")})

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
#' @param yc    class=YieldCurve S4 object with tenorRates, 
#' @param Tfrom character  yyyy-mm-dd date for start of forward rate interval
#' @param Tto   character yyyy-mm-dd date for end of forward rate interval  
#' @return Projected pa interest rate on loan from Tfrom to Tto using YieldCurve
#' @export
#' @include yearFraction.R  
#' @examples {
#'    ycID <- "yc001"
#'    rd <- "2023-10-31"
#'    tr <-  c(1.1, 2.0, 3.5 )/100
#'    names(tr) <- c("1M", "1Y", "5Y")
#'    dcc <- "30E360"
#'    cf <- "NONE"
#'    yc <- YieldCurve(ycID,rd,tr,dcc,cf)
#'    rate <- getForwardRates(yc,"2024-01-01","2024-06-01")
#' }
#'
setMethod(f = "getForwardRates", signature = c("YieldCurve", "character",
                                                "character"),
              definition= function(yc, Tfrom, Tto) {
        # implementation V1 for single Tfrom, Tto value pair with Tfrom < Tto
        # yc$referenceDate < Tfrom
            
        #  1. get yearFractions of Tfrom, Tto relative to yc$referenceDate
        yfFrom <- yearFraction(yc$referenceDate, Tfrom, yc$yfdcc) 
        yfTo   <- yearFraction(yc$referenceDate, Tto,   yc$yfdcc)
          
        #  2. use Rbase::approx to interpolate a yieldCurve value - these times
        rateFrom <- interpolateYieldCurve(yc, yfFrom)
        rateTo   <- interpolateYieldCurve(yc, yfTo)
        
        #  3. Estimate forward rate with compoundingFrequency == NONE
        #     Formula from wbreymann/FEMS/DynamicYieldCurve.R lines 417-420
        
        if (yc$compoundingFrequency == "NONE") {
          frwdRate <- (((1 + rateTo*yfTo)/(1+ rateFrom*yfFrom)) - 1 )/(yfTo - yfFrom)
          return(frwdRate)
        } else if(yc$compoundingFrequency == "YEARLY") {
          frwdRate <- ((1 + rateTo)^yfTo / (1 + rateFrom)^yfFrom)^
            (1/(yfTo - yfFrom)) - 1
          return(frwdRate)
        } else if(yc$compoundingFrequency == "CONTINUOUS"){
          frwdRate <- (rateTo*yfTo - rateFrom*yfFrom)/(yfTo - yfFrom)
          return(frwdRate)
        } else {  
          stop(paste("ErrorIn::YieldCurve::getForwardRates: compoundingFrequency ", 
                  yc$compoundingFrequency , " not supported !!!"))
          }
              
          })

# **********************************************************
# interpolateYieldCurve(yc, tyf)
#     This YieldCurve method takes as input (1) yc a yieldCurve object, and 
#     (2) a numeric tenor  value tyf expressed as a fractional number of years
#     It uses linear approximation provided by RBase function approx() to 
#     linearly interpolate from the tenorRate data points and the precomputed 
#     tenorYfs year fraction x-values saved in yc, to estimate the pa interest
#     rate for tenor tyf. This numeric value is returned.  
# ***********************************************************
 interpolateYieldCurve <- function(yc,tyf) {
   tyfRate <- approx(yc$tenorYfs,yc$tenorRates, tyf, 
                     method = "linear", rule = 2)
   return(tyfRate$y)  # pass back just the estimated value from (x,y) pair 
 }
 
 # ***********************************************************
 # getDiscountFactor(yc, Tfrom, Tto, riskSpread )
 #    This yield curve method takes as input (1) yc a yieldCurve object, (2)
 #    a date Tfrom  in yy *************************************
 getDiscountFactor <- function(yc,Tfrom,Tto,riskSpread) {
    frwdRate <- getForwardRates(yc,Tfrom,Tto)
    if(yc$compoundingFrequency == "CONTINUOUS") {
      factor <- exp(-(frwdRate + riskSpread)*yearFraction(Tfrom,Tto,yc$yfdcc))
    } else if(yc$compoundingFrequency == "YEARLY") {
      factor <- 1/(1 + (frwdRate + riskSpread ))^yearFraction(Tfrom,Tto,yc$yfdcc)
    } else if(yc$compoundingFrequency == "NONE") {
      # factor <- 1/(1 + (frwdRate + riskSpread )*yearFraction(Tfrom,Tto,yc$yfdcc))
    } else {  
      stop(paste("ErrorIn::YieldCurve::getDiscountFactor: compoundingFrequency ", 
                 yc$compoundingFrequency , " not supported !!!"))
    }
    factor <- sapply(factor,function(x) {ifelse(is.nan(x),1,x)})
    return (factor)
 }
 scalarDiscF <- function(yc,Tfrom,Tto,riskSpread){
   if (Tfrom == Tto) {
     factor <-  1.0
   } else {
     frwdRate <- getForwardRates(yc,Tfrom,Tto)
     if(yc$compoundingFrequency == "CONTINUOUS") {
       factor <- exp(-(frwdRate + riskSpread)*yearFraction(Tfrom,Tto,yc$yfdcc))
     } else if(yc$compoundingFrequency == "YEARLY") {
       factor <- 1/(1 + (frwdRate + riskSpread ))^yearFraction(Tfrom,Tto,yc$yfdcc)
     } else if(yc$compoundingFrequency == "NONE") {
       factor <- 1/(1 + (frwdRate + riskSpread )*yearFraction(Tfrom,Tto,yc$yfdcc))
     } else {  
       stop(paste("ErrorIn::YieldCurve::getDiscountFactor: compoundingFrequency ", 
                  yc$compoundingFrequency , " not supported !!!"))
     }
   }
   return (factor)
 }

 newDiscountFactor <- function(yc,Tfrom,Tto, riskSpread){
   if  ((length(Tto) > 1 ) && length(Tfrom == 1) ){
     factorv <- 
       sapply(Tto, function(to) { return(scalarDiscF(yc,Tfrom,to,riskSpread))})
   } else if (( length(Tfrom ) > 1) && (length(Tto) == 1) ) {
     factorv <- 
       sapply(Tfrom,function(from){return(scalarDiscF(yc,from,Tto,riskSpread))})
   } else  { factorv <- scalarDiscF(yc,Tfrom,Tto,riskSpread)
   }  
   return(factorv)
 }
 # ***********************************************************
 # getGrowthFactor(yc, Tfrom, Tto)
 #    This yield curve method takes as input (1) yc a yieldCurve object, (2)
 #    a date Tfrom  in yyyy-mm-dd format at which a past cashflow occurred (3)
 #    a date Tto in yyyy-mm-dd format specifying the date at which received cash 
 #    is to be valued. Compounding with yc$counpoundingFrequency to be added  
 # ***********************************************************
 getGrowthFactor <- function(yc,Tfrom,Tto) {
   frwdRate <- getForwardRates(yc,Tfrom,Tto)
   if(yc$compoundingFrequency == "CONTINUOUS") {
     factor <- exp(frwdRate*yearFraction(Tfrom,Tto,yc$yfdcc))
     return (factor)
   } else if(yc$compoundingFrequency == "YEARLY") {
     factor <- (1 + frwdRate)^yearFraction(Tfrom,Tto,yc$yfdcc)
     return (factor)
   } else if(yc$compoundingFrequency == "NONE") {
     factor <- 1 + frwdRate*yearFraction(Tfrom,Tto,yc$yfdcc)
     return (factor)
   } else {  
     stop(paste("ErrorIn::YieldCurve::getGrowthFactor: compoundingFrequency ", 
                yc$compoundingFrequency , " not supported !!!"))
   }
   # factor <-  1 + frwdRate*yearFraction(Tfrom,Tto,yc$yfdcc)
   return (factor)
 }
