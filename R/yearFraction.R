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
#        RQuantLib::yearFraction()
#'
#'   Function yearFraction(character, character, character ) takes as
#'   input: (1) a yyyy-mm-dd start date of the interval, (2) a yyyy-mm-dd
#'   end date for the interval and (3) a RQuantLib::yearFraction supported dayCountConvention
#'   string.  It returns the length of the interval as a fractional number of
#'   years.  If Dto is earlier (<) Dfrom a negative fractional number of years
#'   is returned.
#'
#'   Function yearFraction(Tfrom,Tto, yfdcc) depends on and uses external
#'   library functions: (1) lubridate::ymd() which converts a yyyy-mm-dd
#'   character string date into a Date value and
#'    (2) RQuantLib::yearFraction(startDates, endDates, dayCounters) which requires startDates and endDates
#'    to be Date vectors and dayCounters a vector of numeric dayCountConvention values
#'    (see https://search.r-project.org/CRAN/refmans/RQuantLib/html/Enum.html).
#'
#' @param Tfrom      character  start date of interval in yyyy-mm-dd form
#' @param Tto.       character  end date of internal in yyyy-mm-dd form
#' @param yfdcc      character  RQuantLib::yearFraction supported dayCountConvention value
#' @return       Length of time interval in fractional number of years
#' @importFrom lubridate ymd
#' @importFrom RQuantLib yearFraction

setMethod(f = "yearFraction", signature = c("character", "character", "numeric"),
          definition = function(Tfrom, Tto, yfdcc){
            # calculate year fraction
            frac <- RQuantLib::yearFraction(lubridate::ymd(Tfrom),
                                            lubridate::ymd(Tto),
                                            dayCounters = yfdcc)
            return(frac)
          })
