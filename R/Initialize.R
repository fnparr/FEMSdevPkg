# *****************************************
# Initialize.R   utilities to set up  maintest environment
# libraries
library(timeSeries)
library(jsonlite)
library(httr)

# mainTest setup() defined here

#' setup
#'
#' initializes the FEMSdevPgk environment sets the ACTUS server url
#'
#' initialize object env <- setup()
#' functions in the package will use env
#' inspect the code in Initialize.r::setup change for different ACTUS url
#'
#' @import  methods
#' @importFrom methods new
#' @return  A reference to S4 Class Environment ACTUS server url set
#'
setup <- function(){
  # initialize globalenvironment
  print (paste0("*** Setting Actus Server URL to ","http://ractus.ch:8080/"))
  print ("*** Initializing  Date_Term_Names and Event_Field_Names")
  outenv <- Environment()
  return(outenv)
}
# ************** the Environment object with global constants
# initialized by setup()
setRefClass("Environment",
            fields = list(
              serverURL = "character",
              Date_Term_Names = "character",  # a vector of names
              Event_Field_Names = "character"
            )
)
# *********************
#  constructors:  Environment() : (), (serverURL, Date_Term_Names )

setGeneric(name = "Environment",
           def = function(...){
             standardGeneric("Environment")
           })

setMethod(f = "Environment", signature = c(),
          definition = function(){
              outenv <- new("Environment")
              outenv$serverURL <- "https://demo.actusfrf.org:8080/"
              #  could also be
                # outenv$serverURL <- "http://ractus.ch:8080/"
              Date_Term_Names <- c(
                  "statusDate","contractDealDate","initialExchangeDate",
                  "maturityDate","cycleAnchorDateOfInterestCalculationBase",
                  "amortizationDate","contractDealDate","cycleAnchorDateOfPrincipalRedemption",
                  "arrayCycleAnchorDateOfPrincipalRedemption","purchaseDate",
                  "terminationDate","cycleAnchorDateOfScalingIndex",
                  "cycleAnchorDateOfRateReset","cycleAnchorDateOfInterestPayment",
                  "capitalizationEndDate")
              outenv$Date_Term_Names <- Date_Term_Names
              Event_Field_Names <- c("type","time","payoff","currency","nominalValue","nominalRate","nominalAccrued")
              # each is actually prefixed by events.  complete field name = events.<fieldname>
              outenv$Event_Field_Names <- Event_Field_Names
              return(outenv)
            })
