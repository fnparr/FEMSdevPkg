# YieldCurve.R  FEMSdevPkg code by Francis Parr Nov 2023 
#   updates earlier git wbreymann/FEMS code by Henriette-Elise Breymann
# Licensing and Copyright notices to be added  XXXX
# ************************************************************************
# yearFraction(Tfrom, Tto, yfDayCountConvention) 
#    -- NONexported wrapper for functions lubridate::ymd() and 
# ************************************************************************    
setGeneric(name = "yearFraction",
           def = function(Tfrom,Tto,yfdcc){
             standardGeneric("yearFraction")
           })

# ************************************************************************
#' yearFraction(Tfrom, Tto, yfDayCountConvention) 
#'    -- NONexported wrapper for functions lubridate::ymd() and 
#        fmdates::year_frac() 
#'
#'   Function yearFraction(character, character, character ) takes as
#'   input: (1) a yyyy-mm-dd start date of the interval, (2) a yyyy-mm-dd 
#'   end date for the interval and (3) a year_frac supported dayCountConvention
#'   string.  It returns the length of the interval as a fractional number of 
#'   years.  If Dto is earlier (<) Dfrom a negative fractional number of years
#'   is returned. 
#'    
#'   Function yearFraction(Tfrom,Tto, yfdcc) depends on and uses external 
#'   library functions: (1) lubridate::ymd() which converts a yyyy-mm-dd 
#'   character string date into a Date value and
#'    (2) fmdates::year_frac(Dfrom, Dto, yfdcc) which requires Dfrom and Dto 
#'    to be Date values and yfdcc a year_frac dayCountConvention string 
#'
#' @param Tfrom      character  start date of interval in yyyy-mm-dd form
#' @param Tto.       character  end date of internal in yyyy-mm-dd form 
#' @param yfdcc      character  year_frac supported dayCountConvention value 
#' @return       Length of time interval in fractional number of years 
#' @importFrom lubridate ymd
#' @importFrom fmdates year_frac

setMethod(f = "yearFraction", signature = c("character", "character", "character"),
          definition = function(Tfrom, Tto, yfdcc){
            
            # calculate year fraction
            frac <- fmdates::year_frac(lubridate::ymd(Tfrom), 
                                       lubridate::ymd(Tto), 
                                       yfdcc)
            return(frac)
          })
